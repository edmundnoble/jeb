{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module J.Cycles.Streengs(allPtrs, applyEdits, searchForDays, consLog) where

import Data.Foldable(traverse_)
import Data.List(intercalate)
import Data.Maybe(fromJust)
import Data.Time(Day(..), addDays)
import Data.Word(Word8)

import Foreign.Ptr(Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable

import GHC.Magic(inline)
import GHC.Prim(chr#, word2Int#)
import GHC.Types(Char(C#))
import GHC.Word(Word8(W8#), Word64)

import qualified Data.Map as Map

import qualified System.IO.MMap as MMap
import qualified System.IO.Unsafe as U
import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT(FileOffset, COff(..))

import J.Cycles.Types

{-# inline entryLocs #-}
-- todo: why not return a `Ptr (Interval Day)` instead?
entryLocs :: Ptr RawEntry -> (Ptr Day, Ptr Day, Ptr RawStatus)
entryLocs p =
  let
    sp = castPtr p
    ep = sp `plusPtr` sizeOf (undefined :: Word64)
    stp = castPtr $ ep `plusPtr` sizeOf (undefined :: Word64)
  in
    (sp, ep, stp)

instance Storable RawEntry where
    -- one 64-bit int for each time, one byte for status
  sizeOf _ = sizeOf (undefined :: Word64) * 2 + sizeOf (undefined :: Word8)
  alignment _ = 1
  peek p =
    let (sp, ep, stp) = entryLocs p in
      RawEntry <$> (Interval <$> peekDay sp <*> peekDay ep) <*> peekRawStatus stp
  poke p (RawEntry (Interval s e) st) =
    let (sp, ep, stp) = entryLocs p in
      pokeDay sp s >> pokeDay ep e >> pokeRawStatus stp st

{-# inline conlike entrySize #-}
entrySize :: Int
entrySize = sizeOf (undefined :: RawEntry)

{-# inline pokeDay #-}
pokeDay :: Ptr Day -> Day -> IO ()
pokeDay p (ModifiedJulianDay d) = poke (castPtr p) (fromInteger d :: Word64)

{-# inline peekDay #-}
peekDay :: Ptr Day -> IO Day
peekDay p = (ModifiedJulianDay . fromIntegral) <$> (peek (castPtr p) :: IO Word64)

{-# inline pokeRawStatus #-}
pokeRawStatus :: Ptr RawStatus -> RawStatus -> IO ()
pokeRawStatus p = \case
  N -> poke p' 'N'
  Y -> poke p' 'Y'
  U c -> poke p' c
  where
    p' = castPtr p

{-# inline peekRawStatus #-}
peekRawStatus :: Ptr RawStatus -> IO RawStatus
peekRawStatus p = flip fmap (peek (castPtr p)) $ \case
  W8# wu ->
    case chr# (word2Int# wu) of
      'Y'# -> Y
      'N'# -> N
      c -> U (C# c)

data SizedPtr a = SizedPtr {-# unpack #-} !Int {-# unpack #-} !(Ptr a) deriving Show

eval :: IO a -> IO a
eval io = io >>= (return $!)

-- INTERVALS ARE OPEN ON THE RIGHT!
{-# inline binRangeQuery #-}
binRangeQuery :: forall k a.
  (Show a, Show k, Storable a, Ord k) =>
  (a -> Interval k) ->
  Interval k ->
  SizedPtr a ->
  IO [Ptr a]
binRangeQuery getKeys !k !szp = eval (go szp) where
  addIncrements :: Ptr a -> Int -> Ptr a
  addIncrements p i = p `plusPtr` (i * sizeOf (undefined :: a))

  intersects :: Interval k -> Interval k -> Bool
  intersects i1 i2 = (contpareIntervals i1 i2) == EQ

  contains :: Interval k -> Interval k -> Bool
  contains (Interval k1 k2) (Interval k1' k2') = k1 <= k1' && k2' <= k2

  go :: SizedPtr a -> IO [Ptr a]
  go (SizedPtr size start) = do
      peekStart <- peek start
      let end = start `addIncrements` (size - 1)
      peekEnd <- peek end
      let kStart = contpareIntervals k (getKeys peekStart)
      let kEnd = contpareIntervals k (getKeys peekEnd)
      -- putStrLn (show start ++ " " ++ show peekStart ++ " -> " ++ show end ++ " " ++ show peekEnd)
      -- putStrLn $ "kStart: " ++ show kStart ++ ", kEnd: " ++ show kEnd
      let whole = Interval (intervalStart $ getKeys peekStart) (intervalEnd $ getKeys peekEnd)

      if not $ (whole `intersects` k) then
        pure []
      else if getKeys peekStart == k then
        pure [start]
      else if getKeys peekEnd == k then
        pure [end]
      else if k `contains` whole then
        pure (addIncrements start <$> [0..(size - 1)])
      else if size == 1 then
        pure [start]
      else do
        let halfSize = size `div` 2
        let mid = start `addIncrements` halfSize
        peekMid <- peek mid
        -- putStrLn $ "mid: " ++ show mid
        let recRight = U.unsafeInterleaveIO $ go (SizedPtr (halfSize + (size `mod` 2)) mid)
        let recLeft = U.unsafeInterleaveIO $ go (SizedPtr halfSize start)
        let kMid = contpareIntervals k (getKeys peekMid)
        -- putStrLn $ "kmid: " ++ show kMid
        case kMid of
          LT ->
            recLeft
          GT ->
            recRight
          EQ ->
            let
              newLeft = if
                kStart /= GT
              then
                pure $ addIncrements start <$> [0 .. halfSize - 1]
              else
                recLeft

              newRight = if
                kEnd /= LT
              then
                pure $ addIncrements start <$> [halfSize + 1 .. size - 1]
              else
                recRight

            in
              (++) <$> newLeft <*> (((:) mid) <$> newRight)

data InsertionPoint a
  = InsertAtPtr !(Ptr a)
  | ConflictsWith ![Ptr a]
  deriving (Eq, Show)

{-# inline binSearchInsertionPoint #-}
-- (k -> a -> Ordering) where EQ is weakened to mean "contains"/"compare", like a preorder.
-- log time in the size of the passed pointer
binSearchInsertionPoint :: forall a k. (Storable a, Show a, Show k) => (k -> a -> Ordering) -> k -> SizedPtr a -> IO (InsertionPoint a)
binSearchInsertionPoint !contpare !k !szp = eval (go szp) where
  addIncrements :: Ptr a -> Int -> Ptr a
  addIncrements p i = p `plusPtr` (i * sizeOf (undefined :: a))

  combinePoints :: InsertionPoint a -> InsertionPoint a -> InsertionPoint a
  combinePoints (ConflictsWith ps) (ConflictsWith qs) = ConflictsWith (ps ++ qs)
  combinePoints c@(ConflictsWith _) _ = c
  combinePoints _ c@(ConflictsWith _) = c
  combinePoints _ i = i

  {-# inline go #-}
  go :: SizedPtr a -> IO (InsertionPoint a)
  go (SizedPtr size start) =
    do
      peekStart <- peek start
      let end = start `addIncrements` (size - 1)
      peekEnd <- peek end
      case (contpare k peekStart, contpare k peekEnd) of
        (EQ, EQ) ->
          -- every remaining interval contains the interval being searched for
          (pure . ConflictsWith . allPtrs start) (size * sizeOf (undefined :: a))
        (c, _) | size == 1 && c /= EQ ->
          pure $ InsertAtPtr start
        (GT, LT) | size == 2 ->
          pure $ InsertAtPtr end
        _ -> do
          let halfSize = size `div` 2
          let mid = start `addIncrements` halfSize
          let rtRec = go (SizedPtr (size - halfSize) mid)
          let ltRec = go (SizedPtr halfSize start)
          peekMid <- peek mid
          case contpare k peekMid of
            GT ->
              inline rtRec
            LT ->
              inline ltRec
            EQ ->
              combinePoints (ConflictsWith [mid]) <$> (combinePoints <$> inline ltRec <*> inline rtRec)

{-# inline binRangeQueryFile #-}
binRangeQueryFile :: forall a b k.
  (Show a, Storable a, Ord k, Show k) =>
  (Ptr a -> IO b) ->
  (a -> Interval k) ->
  Interval k ->
  FilePath ->
  IO [b]
binRangeQueryFile !conv !getKeys !k !fp =
  MMap.mmapWithFilePtr fp MMap.ReadOnly Nothing convSearchWithPtr
    where
      -- subtract one for the null byte
      searchWithPtr (p, s) =
        binRangeQuery getKeys k (SizedPtr (s `div` (sizeOf (undefined :: a))) (castPtr p))
      convSearchWithPtr t = searchWithPtr t >>= traverse conv

{-# inline contpareIntervals #-}
contpareIntervals :: (Show a, Ord a) => Interval a -> Interval a -> Ordering
contpareIntervals (Interval k1 k2) (Interval k1' k2') =
  -- closed on the right!
  if
    k2 == k1'
  then
    LT
  else if
    k2' == k1
  then
    GT
  else if
    (k1 < k1' && k2 < k2' && k2 < k1')
  then
    LT
  else if
    (k1' < k1 && k2' < k2 && k2' < k1)
  then
    GT
  else
    EQ

{-# inline entryStart #-}
entryStart :: RawEntry -> Day
entryStart = intervalStart . _entryToInterval

{-# inline entryEnd #-}
entryEnd :: RawEntry -> Day
entryEnd = intervalEnd . _entryToInterval

{-# inline searchForDays #-}
searchForDays :: Interval Day -> FilePath -> IO [RawEntry]
searchForDays = binRangeQueryFile peek _entryToInterval
--
-- ==============================================================================
--

{-# inline allPtrs #-}
allPtrs :: forall a. Storable a => Ptr a -> Int -> [Ptr a]
allPtrs ptr s =
  let
    sz = sizeOf (undefined :: a)
  in
    plusPtr ptr <$> ((* sz) <$> [0..((s - 1) `div` sz)])

{-# inline inc #-}
inc :: forall a. Storable a => Int -> Ptr a -> Ptr a
inc n ptr = ptr `plusPtr` (n * sizeOf (undefined :: a))

{-# inline memcpy #-}
memcpy :: forall a. Storable a => Ptr a -> SizedPtr a -> IO ()
memcpy tgt (SizedPtr s src) = go 0 where
  go n | n == s = pure ()
  go n = do
    peek (inc n src) >>= poke (inc n tgt)
    go (n + 1)

singInterval :: Day -> Interval Day
singInterval d = Interval d (addDays 1 d)

-- this should amortize more.
applyEdits :: PendingEdits -> FilePath -> IO ()
applyEdits (PendingEdits edits) logPath = traverse_ (uncurry loadEdit) (Map.toList edits)
  where
    -- gonna have to delete/shrink other intervals
    -- todo: make this less slow in the presence of multiple non-tail edits.
    loadEdit :: Day -> MetaStatus -> IO ()
    loadEdit d s = consEdit (Edit d s) logPath

fileSize :: FilePath -> IO (PT.FileOffset)
fileSize logPath = do
  logExists <- PF.fileExist logPath
  if
    logExists
  then
    PF.fileSize <$> PF.getFileStatus logPath
  else
    pure $ PT.COff 0

editToEntry :: Edit -> Maybe RawEntry
editToEntry (Edit d s) = RawEntry (singInterval d) . statusToRawStatus <$> fromMetaStatus s

consEdit :: Edit -> FilePath -> IO ()
consEdit e logPath = do
  logSizeOff <- fileSize logPath
  let logSizeBytes = fromIntegral logSizeOff
  let newLogSize = logSizeBytes + entrySize
  MMap.mmapWithFilePtr logPath MMap.ReadWriteEx (Just (0,newLogSize)) (\(fp, s) -> do
      if
        logSizeBytes == 0
      then do
        poke (castPtr fp) (fromJust $ editToEntry e)
      else
        do
          -- why do this search if `_editDay e > entryEnd end`?
          place <- binSearchInsertionPoint (\k a -> contpareIntervals k (_entryToInterval a)) (singInterval $ _editDay e) (SizedPtr ((s - 1) `div` entrySize) (castPtr fp))
          end <- peek (fp `plusPtr` (logSizeBytes - entrySize)) :: IO RawEntry
          res <- case place of
            InsertAtPtr placePtr -> do
              if _editDay e > entryEnd end
                then
                  Right <$> poke (fp `plusPtr` logSizeBytes) (fromJust $ editToEntry e)
                else
                  MMap.mmapWithFilePtr (logPath ++ ".bak") MMap.ReadWriteEx (Just (0, ((fp `plusPtr` logSizeBytes) `minusPtr` placePtr))) (\(bfp, bs) -> do
                    let numElems = (bs `div` entrySize)
                    memcpy (castPtr bfp) (SizedPtr numElems placePtr)
                    poke (castPtr placePtr) (fromJust $ editToEntry e)
                    memcpy ((placePtr `plusPtr` entrySize) :: Ptr RawEntry) (SizedPtr numElems (castPtr bfp))
                    pure (Right ())
                  )
            ConflictsWith ptrs -> do
              (Left . intercalate ",") <$> (traverse (fmap show . peek) ptrs)
          case res of
            Left els -> do
              putStrLn $ "Input entry (" ++ show e ++ ") conflicts with other entries: " ++ els ++ " and overwriting isn't implemented yet"
              PF.setFileSize logPath logSizeOff
            _ -> pure ()
    )

consLog :: RawEntry -> FilePath -> IO ()
consLog e logPath = do
  logSizeOff <- fileSize logPath
  let logSizeBytes = fromIntegral logSizeOff
  let newLogSize = logSizeBytes + entrySize
  MMap.mmapWithFilePtr logPath MMap.ReadWriteEx (Just (0,newLogSize)) (\(fp, s) -> do
      if
        logSizeBytes == 0
      then do
        poke (castPtr fp) e
      else
        do
          place <- binSearchInsertionPoint (\k a -> contpareIntervals k (_entryToInterval a)) (_entryToInterval e) (SizedPtr ((s - 1) `div` entrySize) (castPtr fp))
          end <- peek (fp `plusPtr` (logSizeBytes - entrySize)) :: IO RawEntry
          res <- case place of
            InsertAtPtr placePtr -> do
              if entryStart e > entryEnd end
                then
                  Right <$> poke (fp `plusPtr` logSizeBytes) e
                else
                  MMap.mmapWithFilePtr (logPath ++ ".bak") MMap.ReadWriteEx (Just (0, ((fp `plusPtr` logSizeBytes) `minusPtr` placePtr))) (\(bfp, bs) -> do
                    let numElems = (bs `div` entrySize)
                    memcpy (castPtr bfp) (SizedPtr numElems placePtr)
                    poke (castPtr placePtr) e
                    memcpy ((placePtr `plusPtr` entrySize) :: Ptr RawEntry) (SizedPtr numElems (castPtr bfp))
                    pure (Right ())
                  )
            ConflictsWith ptrs -> do
              (Left . intercalate ",") <$> (traverse (fmap show . peek) ptrs)
          case res of
            Left els -> do
              putStrLn $ "Input entry (" ++ show e ++ ") conflicts with other entries: " ++ els ++ " and overwriting isn't implemented yet"
              PF.setFileSize logPath logSizeOff
            _ -> pure ()
    )
