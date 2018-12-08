{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module J.Cycles.Streengs where -- (allPtrs, applyEdits, searchForDays, consLog) where

import Control.Monad(guard, join)
import Data.Foldable(traverse_)
import Data.Functor(($>))
import Data.List((\\))
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe(maybeToList)
import Data.Proxy(Proxy(..))
import Data.Time(Day(..), addDays)
import Data.Semigroup((<>))

import Foreign.Ptr(Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable(Storable(..))

import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty

import qualified System.IO.MMap as MMap
import qualified System.IO.Unsafe as U
import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT(FileOffset, COff(..))

import J.Cycles.Types

data SizedPtr a = SizedPtr {-# unpack #-} !Int {-# unpack #-} !(Ptr a) deriving Show

trace :: String -> b -> b
trace m b = U.unsafePerformIO (appendFile "log" (m ++ "\n") $> b)

traceShow :: Show a => a -> b -> b
traceShow a b = trace (show a) b

traceShowId :: Show a => a -> a
traceShowId a = traceShow a a

traceM :: String -> IO ()
traceM m = return $! trace m ()

traceShowM :: Show a => a -> IO a
traceShowM a = return $! traceShowId a

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
              (++) <$> newLeft <*> ((:) mid <$> newRight)

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
              combinePoints <$> ltRec <*> rtRec

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
      searchWithPtr (p, s) =
        binRangeQuery getKeys k (SizedPtr (s `div` sizeOf (undefined :: a)) (castPtr p))
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

singTimeSpan :: Day -> TimeSpan
singTimeSpan d = TimeSpan d (addDays 1 d)

-- this should amortize more.
applyEdits :: PendingEdits -> FilePath -> IO ()
applyEdits (PendingEdits edits) logPath =
  traverse_ (flip consLog logPath . editFrom) (Map.toList edits)
  where
    editFrom (d, ms) = traceShowId $ Edit (singTimeSpan d) (fromMetaStatus ms)

fileSize :: FilePath -> IO PT.FileOffset
fileSize logPath = do
  logExists <- PF.fileExist logPath
  if
    logExists
  then
    PF.fileSize <$> PF.getFileStatus logPath
  else
    pure $ PT.COff 0

editToEntry :: Edit -> Maybe RawEntry
editToEntry (Edit d s) = RawEntry d . statusToRawStatus <$> s

joinRedundant :: [RawEntry] -> [RawEntry]
joinRedundant (RawEntry ts1 st1:RawEntry ts2 st2:es)
  | st1 == st2 && intervalEnd ts1 == intervalStart ts2 =
    joinRedundant (RawEntry (TimeSpan (intervalStart ts1) (intervalEnd ts2)) st1:es)
joinRedundant (e:es) = e:joinRedundant es
joinRedundant [] = []

replace :: Edit -> [RawEntry] -> [Maybe RawEntry]
replace (editToEntry -> e) [] = [e]
replace en@(Edit ds newST) (RawEntry i@(TimeSpan s e) oldST:es)
  | ds `spanContainsSpan` i =
    -- replace the whole thing \---|---|---/
    let
      new = RawEntry (TimeSpan (intervalStart ds) (intervalEnd i)) . statusToRawStatus <$> newST
    in
      new : recc
  | intervalStart ds >= s && intervalEnd ds <= e =
    -- cut into two |---\---/---|
    -- also, no need to recurse further
    let
      first = RawEntry (TimeSpan s (intervalStart ds)) oldST
      second = RawEntry (TimeSpan (addDays 1 (intervalEnd ds)) e) oldST
      mid = RawEntry ds . statusToRawStatus <$> newST
    in
      (Just first : mid : Just second : fmap Just es)
  | intervalStart ds >= s =
    -- chop off end |---\---|---/ ==> |---\
    let
      old = RawEntry (TimeSpan s (intervalStart ds)) oldST
      new = RawEntry (TimeSpan (intervalEnd ds) e) . statusToRawStatus <$> newST
    in
      Just old : new : recc
  | otherwise =
    let
      new = RawEntry ds . statusToRawStatus <$> newST
      old = RawEntry (TimeSpan (intervalEnd ds) e) oldST
    -- chop off start \---|---/---| ==> /---|
    -- also, no need to recurse further
    in
      new : Just old : fmap Just es
  where
    -- if we recurse, we need to keep shrinking the passed `Edit` so that it starts
    -- at e.
    -- otherwise there would be no sane way to handle |---| \---|---|---/ |---|
    newSpan = guard (intervalStart ds > e) $> TimeSpan e (intervalStart ds)
    newEdit = flip Edit newST <$> newSpan
    recc = maybe (Just <$> es) (`replace` es) newEdit

(>*>) :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
(>*>) = traverse

serialize :: forall a. (Show a, Storable a) => Ptr a -> [a] -> IO ()
serialize p xs = do
  traceShowM =<< readEs "logs/A"
  traverse_ serializeInd $ zip xs [0..]
  traceShowM =<< readEs "logs/A"
  pure ()
  where
    serializeInd (x, i) =
      poke (p `plusPtr` (i * sizeOf x)) x

memcpy :: forall p a. Storable a => p a -> Ptr a -> SizedPtr a -> IO ()
memcpy _ tgt (SizedPtr s src) = go 0 where
  go n | n == s = pure ()
  go n = do
    peek (inc n src) >>= poke (inc n tgt)
    go (n + 1)

consLog :: Edit -> FilePath -> IO ()
consLog ed@(Edit ds _) logPath = do
  traceShowM "consLog"
  logSizeOff <- fileSize logPath
  let logSizeBytes = fromIntegral logSizeOff
  let contpareEntry k = contpareTimeSpans k . _entryToTimeSpan
  let insertPainless fp = traceM "insertPainless" *> maybe (pure ()) (poke (castPtr fp)) (editToEntry ed)
  -- either a) we insert an entry which doesn't conflict (one extra entry)
  -- or b) we insert an entry in the interior of another (two extra entries)
  let maxLogSize = logSizeBytes + 2 * entrySize
  -- length news >= length olds precondition
  let
    replaceAt :: SizedPtr RawEntry -> [RawEntry] -> [RawEntry] -> IO Int
    replaceAt pt@(SizedPtr n p) olds news =
      trace "replaceAt" $
       let
        -- this is totally screwed, isn't it?
        oldlen = length olds
        newlen = length news
        toCopy = n - oldlen
        write (fp, _) =
          let
          in do
            traceShowM pt
            traceShowM olds
            traceShowM news
            traceShowM "copying to"
            traceShowM toCopy
            traceShowM (SizedPtr toCopy (p `plusPtr` (oldlen * entrySize)))
            traceShowM =<< readEs logPath
            memcpy (Proxy :: Proxy RawEntry) (castPtr fp) (SizedPtr toCopy (p `plusPtr` (oldlen * entrySize)))
            traceShowM =<< readEs logPath
            traceShowM "inserting"
            -- print news
            serialize p news
            traceShowM =<< readEs logPath
            traceShowM "copying fro"
            memcpy (Proxy :: Proxy RawEntry) (p `plusPtr` (newlen * entrySize)) (SizedPtr toCopy (castPtr fp))
            traceShowM =<< readEs logPath
            -- print =<< peek fp
            pure $ n - oldlen + newlen
      in
        if oldlen == newlen then do
          -- we've got a straightforward overwrite
          traceShowM "replacing"
          traceShowM news
          traceShowM =<< readEs logPath
          serialize p news $> n
          traceShowM =<< readEs logPath
          return n
        else do
          traceShowM "mmaping"
          MMap.mmapWithFilePtr (logPath ++ ".bak") MMap.ReadWriteEx (Just (0, toCopy * entrySize)) write
  let m (fp, _) =
        traceShow fp $
        if
          logSizeBytes == 0
        then
          insertPainless fp $> entrySize
        else
          do
            let endPtr = fp `plusPtr` (logSizeBytes - entrySize)
            end <- peek endPtr :: IO RawEntry
            if intervalStart ds >= entryEnd end
            then insertPainless (fp `plusPtr` logSizeBytes) $> logSizeBytes + entrySize
            else do
              let searchTarget = SizedPtr (logSizeBytes `div` entrySize) (castPtr fp)
              place <- binSearchInsertionPoint contpareEntry ds searchTarget
              case place of
                InsertAtPtr placePtr -> do
                  -- why am I trying to insert past the end?
                  let remaining = (endPtr `minusPtr` placePtr) `div` entrySize + 1
                  case editToEntry ed of
                    Nothing -> pure logSizeBytes
                    Just e ->  replaceAt (SizedPtr remaining placePtr) [] [e] $> logSizeBytes + entrySize

                ConflictsWith ptrs -> do
                  traceShowM "ConflictsWith"
                  traceShowM ptrs
                  conflicts <- NonEmpty.toList <$> peek >*> ptrs
                  let news = joinRedundant $ replace ed conflicts >>= maybeToList
                  traceM "news"
                  let ptr = NonEmpty.head ptrs
                  let startSz = ptr `minusPtr` fp
                  let endSz = (logSizeBytes - startSz) `div` entrySize
                  (startSz +) . (entrySize *) <$> replaceAt (SizedPtr endSz ptr) conflicts news
  traceM "\n"
  traceShowM =<< readEs logPath
  sz <- MMap.mmapWithFilePtr logPath MMap.ReadWriteEx (Just (0,maxLogSize)) m
  traceM "\n"
  traceShowM =<< readEs logPath
  traceM "\n"
  PF.setFileSize logPath (fromIntegral sz)

readS :: forall a. Storable a => FilePath -> IO [a]
readS fp =
  MMap.mmapWithFilePtr fp MMap.ReadOnly Nothing (\(p, s) -> print s *> readP (castPtr p) (s - 1))

readP :: forall a. Storable a => Ptr a -> Int -> IO [a]
readP ptr s =
  if s <= 0 then pure [] else NonEmpty.toList <$> traverse peek (allPtrs ptr s)

readEs :: FilePath -> IO [RawEntry]
readEs = readS

readEsD = ((=<<) putStrLn) . fmap (unlines . fmap show) . readEs
