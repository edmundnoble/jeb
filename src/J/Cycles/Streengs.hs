{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module J.Cycles.Streengs(allPtrs, applyEdits, searchForDays, consLog) where

import Data.Foldable(traverse_)
import Data.Functor(($>))
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(fromJust)
import Data.Proxy(Proxy(..))
import Data.Time(Day(..), addDays)
import Data.Semigroup((<>))
import Data.Semigroup.Foldable(intercalate1)

import Foreign.Ptr(Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable(Storable(..))

import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty

import qualified System.IO.MMap as MMap
import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT(FileOffset, COff(..))

import J.Cycles.Types

data SizedPtr a = SizedPtr {-# unpack #-} !Int {-# unpack #-} !(Ptr a) deriving Show

eval :: IO a -> IO a
eval io = io >>= (return $!)

-- INTERVALS ARE OPEN ON THE RIGHT!
{-# inline binRangeQuery #-}
binRangeQuery :: forall a.
  (Show a, Storable a) =>
  (a -> TimeSpan) ->
  TimeSpan ->
  SizedPtr a ->
  IO [Ptr a]
binRangeQuery getKeys !k !szp = eval (go szp) where
  addIncrements :: Ptr a -> Int -> Ptr a
  addIncrements p i = p `plusPtr` (i * sizeOf (undefined :: a))

  go :: SizedPtr a -> IO [Ptr a]
  go (SizedPtr size start) = do
      peekStart <- peek start
      let end = start `addIncrements` (size - 1)
      peekEnd <- peek end
      let kStart = contpareTimeSpans k (getKeys peekStart)
      let kEnd = contpareTimeSpans k (getKeys peekEnd)
      -- putStrLn (show start ++ " " ++ show peekStart ++ " -> " ++ show end ++ " " ++ show peekEnd)
      -- putStrLn $ "kStart: " ++ show kStart ++ ", kEnd: " ++ show kEnd
      let whole = TimeSpan (intervalStart $ getKeys peekStart) (intervalEnd $ getKeys peekEnd)

      if not $ timespansIntersect whole k then
        pure []
      else if getKeys peekStart == k then
        pure [start]
      else if getKeys peekEnd == k then
        pure [end]
      else if k `spanContainsSpan` whole then
        pure (addIncrements start <$> [0..(size - 1)])
      else if size == 1 then
        pure [start]
      else do
        let halfSize = size `div` 2
        let mid = start `addIncrements` halfSize
        peekMid <- peek mid
        -- putStrLn $ "mid: " ++ show mid
        let recRight = go (SizedPtr (halfSize + (size `mod` 2)) mid)
        let recLeft = go (SizedPtr halfSize start)
        let kMid = contpareTimeSpans k (getKeys peekMid)
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
  | ConflictsWith !(NonEmpty (Ptr a))
  deriving (Eq, Show)

{-# inline binSearchInsertionPoint #-}
-- (k -> a -> Ordering) where EQ is weakened to mean "contains"/"compare", like a preorder.
-- log time in the size of the passed pointer
binSearchInsertionPoint :: forall a k. (Storable a, Show a, Show k) => (k -> a -> Ordering) -> k -> SizedPtr a -> IO (InsertionPoint a)
binSearchInsertionPoint !contpare !k !szp = eval (go szp) where
  addIncrements :: Ptr a -> Int -> Ptr a
  addIncrements p i = p `plusPtr` (i * sizeOf (undefined :: a))

  combinePoints :: InsertionPoint a -> InsertionPoint a -> InsertionPoint a
  combinePoints (ConflictsWith ps) (ConflictsWith qs) = ConflictsWith (ps <> qs)
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
              rtRec
            LT ->
              ltRec
            EQ ->
              combinePoints (ConflictsWith (mid:|[])) <$> (combinePoints <$> ltRec <*> rtRec)

{-# inline binRangeQueryFile #-}
binRangeQueryFile :: forall a b.
  (Show a, Storable a) =>
  (Ptr a -> IO b) ->
  (a -> TimeSpan) ->
  TimeSpan ->
  FilePath ->
  IO [b]
binRangeQueryFile !conv !getKeys !k !fp =
  MMap.mmapWithFilePtr fp MMap.ReadOnly Nothing convSearchWithPtr
    where
      -- subtract one for the null byte
      searchWithPtr (p, s) =
        binRangeQuery getKeys k (SizedPtr (s `div` (sizeOf (undefined :: a))) (castPtr p))
      convSearchWithPtr t = searchWithPtr t >>= traverse conv

{-# inline entryStart #-}
entryStart :: RawEntry -> Day
entryStart = intervalStart . _entryToTimeSpan

{-# inline entryEnd #-}
entryEnd :: RawEntry -> Day
entryEnd = intervalEnd . _entryToTimeSpan

{-# inline searchForDays #-}
searchForDays :: TimeSpan -> FilePath -> IO [RawEntry]
searchForDays = binRangeQueryFile peek _entryToTimeSpan
--
-- ==============================================================================
--

-- unsafe, only call with `s > 0`
allPtrs :: forall a. Storable a => Ptr a -> Int -> NonEmpty (Ptr a)
allPtrs ptr s =
  let
    sz = sizeOf (undefined :: a)
  in
    NonEmpty.fromList $ plusPtr ptr <$> ((* sz) <$> [0..((s - 1) `div` sz)])

inc :: forall a. Storable a => Int -> Ptr a -> Ptr a
inc n ptr = ptr `plusPtr` (n * sizeOf (undefined :: a))

memcpy :: forall p a. Storable a => p a -> Ptr a -> SizedPtr a -> IO ()
memcpy _ tgt (SizedPtr s src) = go 0 where
  go n | n == s = pure ()
  go n = do
    peek (inc n src) >>= poke (inc n tgt)
    go (n + 1)

singTimeSpan :: Day -> TimeSpan
singTimeSpan d = TimeSpan d (addDays 1 d)

-- this should amortize more.
applyEdits :: PendingEdits -> FilePath -> IO ()
applyEdits (PendingEdits edits) logPath = undefined -- traverse_ (uncurry loadEdit) (Map.toList edits)
  where
    -- gonna have to delete/shrink other intervals
    -- todo: make this less slow in the presence of multiple non-tail edits.
    loadEdit :: Day -> MetaStatus -> IO ()
    loadEdit d s = case fromMetaStatus s of
      Nothing -> deleteSwath (singTimeSpan d) logPath
      Just s -> consEdit (singTimeSpan d) s logPath

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
editToEntry (Edit d s) = RawEntry (singTimeSpan d) . statusToRawStatus <$> fromMetaStatus s

deleteSwath :: TimeSpan -> FilePath -> IO ()
deleteSwath ds logPath = do
  logSizeOff <- fileSize logPath
  let logSizeBytes = fromIntegral logSizeOff
  let
    -- invariant: to be passed to `cut`, all of the pointed-to spans must intersect with `ds`
    cut :: [Ptr TimeSpan] -> [TimeSpan]
    cut [] = pure []
    cut (p:ptrs) = do
      i@(TimeSpan s e) <- _entryToTimeSpan <$> peek p
      if ds `spanContainsSpan` i then
        cut ds ptrs
      else if i `spanContainsSpan` ds then
        undefined
      -- else if ds `intervalContains` (intervalStart i) then


      else Just i
      -- let
      --   md i@(TimeSpan s e) =
      --     else if i `spanContainsSpan` ds then
      --       -- we have to do some snipping.
      --       if then Nothing
      --       else if s == intervalStart ds then Nothing
      --       else Nothing
      --     else Just i
      -- in
      --   undefined
  let x (fp, s) =
        do
          end <- peek (fp `plusPtr` (logSizeBytes - entrySize)) :: IO RawEntry
          if intervalStart ds > entryEnd end
          then
            -- nothing to delete
            return logSizeBytes
          else
            do
              place <- binSearchInsertionPoint (\k a -> contpareTimeSpans k (_entryToTimeSpan a)) ds (SizedPtr ((s - 1) `div` entrySize) (castPtr fp))
              case place of
                  InsertAtPtr _ ->
                    return logSizeBytes
                  ConflictsWith ptrs -> do
                    -- let newPtrs = cut ds ptrs
                    let toDelete = length ptrs
                    if logSizeBytes `div` entrySize == toDelete
                    then
                      -- delete ALL
                      return 0
                    else
                      do
                        let firstDeleted = NonEmpty.head ptrs
                        let lastDeleted = NonEmpty.last ptrs
                        let fileEnd = fp `plusPtr` logSizeBytes
                        let frontSize = firstDeleted `minusPtr` fp
                        let backSize = fileEnd `minusPtr` lastDeleted
                        memcpy (Proxy :: Proxy RawEntry) firstDeleted (SizedPtr (backSize `div` entrySize) (castPtr lastDeleted))
                        return (frontSize + backSize)

  if logSizeBytes == 0
  then
    pure ()
  else do
    newSize <- MMap.mmapWithFilePtr logPath MMap.ReadWriteEx (Just (0,logSizeBytes)) x
    PF.setFileSize logPath (fromIntegral newSize)

consEdit :: TimeSpan -> Status -> FilePath -> IO ()
consEdit ds s logPath = do
  logSizeOff <- fileSize logPath
  let logSizeBytes = fromIntegral logSizeOff
  let maxLogSize = logSizeBytes + entrySize
  let entry = RawEntry ds (statusToRawStatus s)
  let doIt (fp, sz) =
        do
          if
            logSizeBytes == 0
          then do
            poke (castPtr fp) entry $> maxLogSize
          else
            do
              -- why do this search if `_editDay e > entryEnd end`?
              end <- peek (fp `plusPtr` (logSizeBytes - entrySize)) :: IO RawEntry
              if intervalStart ds > entryEnd end
              then
                poke (fp `plusPtr` logSizeBytes) entry $> maxLogSize
              else
                do
                  place <- binSearchInsertionPoint (\k a -> contpareTimeSpans k (_entryToTimeSpan a)) ds (SizedPtr ((sz - 1) `div` entrySize) (castPtr fp))
                  case place of
                      InsertAtPtr placePtr ->
                        MMap.mmapWithFilePtr (logPath ++ ".bak") MMap.ReadWriteEx (Just (0, ((fp `plusPtr` logSizeBytes) `minusPtr` placePtr))) (\(bfp, bs) ->
                          do
                            let numElems = (bs `div` entrySize)
                            memcpy (Proxy :: Proxy RawEntry) (castPtr bfp) (SizedPtr numElems placePtr)
                            poke (castPtr placePtr) entry
                            memcpy (Proxy :: Proxy RawEntry) ((placePtr `plusPtr` entrySize)) (SizedPtr numElems (castPtr bfp))
                            pure maxLogSize
                        )
                      ConflictsWith (p:|ps) -> do
                        if null ps then
                          poke p entry $> logSizeBytes
                        else
                          do
                            let lastDeleted = last ps
                            let fileEnd = fp `plusPtr` logSizeBytes
                            let backSize = fileEnd `minusPtr` lastDeleted
                            memcpy (Proxy :: Proxy RawEntry) (p `plusPtr` entrySize) (SizedPtr ((backSize `div` entrySize) + 1) (castPtr lastDeleted))
                            poke p entry
                            return $ logSizeBytes - length ps

  newLogSize <- MMap.mmapWithFilePtr logPath MMap.ReadWriteEx (Just (0,maxLogSize)) doIt
  PF.setFileSize logPath (fromIntegral newLogSize)
    --           case res of
--             Left els -> do
--               putStrLn $ "Input entry (" ++ show e ++ ") conflicts with other entries: " ++ els ++ " and overwriting isn't implemented yet"
--               PF.setFileSize logPath logSizeOff
--             _ -> pure ()
--     )
  -- undefined

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
          place <- binSearchInsertionPoint (\k a -> contpareTimeSpans k (_entryToTimeSpan a)) (_entryToTimeSpan e) (SizedPtr ((s - 1) `div` entrySize) (castPtr fp))
          end <- peek (fp `plusPtr` (logSizeBytes - entrySize)) :: IO RawEntry
          res <- case place of
            InsertAtPtr placePtr -> do
              if entryStart e > entryEnd end
                then
                  Right <$> poke (fp `plusPtr` logSizeBytes) e
                else
                  MMap.mmapWithFilePtr (logPath ++ ".bak") MMap.ReadWriteEx (Just (0, ((fp `plusPtr` logSizeBytes) `minusPtr` placePtr))) (\(bfp, bs) -> do
                    let numElems = (bs `div` entrySize)
                    -- memcpy (castPtr bfp) (SizedPtr numElems placePtr)
                    poke (castPtr placePtr) e
                    -- memcpy ((placePtr `plusPtr` entrySize) :: Ptr RawEntry) (SizedPtr numElems (castPtr bfp))
                    pure (Right ())
                  )
            ConflictsWith ptrs -> do
              (Left . intercalate1 ",") <$> (traverse (fmap show . peek) ptrs)
          case res of
            Left els -> do
              putStrLn $ "Input entry (" ++ show e ++ ") conflicts with other entries: " ++ els ++ " and overwriting isn't implemented yet"
              PF.setFileSize logPath logSizeOff
            _ -> pure ()
    )
