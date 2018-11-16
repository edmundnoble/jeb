{-# language BangPatterns #-}
{-# language ExplicitForAll #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language PatternSynonyms #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}

module J.Cycles.Main (brickMain) where

import Brick.AttrMap
import Brick.Main
import Brick.Themes (loadCustomizations, themeToAttrMap)
import Brick.Types hiding (Max, Down)
import Brick.Util(clamp)

import Control.Applicative((<|>))
import Control.Monad.IO.Class(liftIO)
import Data.Bifunctor(Bifunctor(..))
import Data.Either(fromRight)
import Data.Foldable(find, toList)
import Data.Functor(($>))
import Data.Functor.Compose(Compose(..))
import Data.Map.Strict(Map)
import Data.Maybe(fromMaybe)
import Data.List(isSuffixOf)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Time(Day(..), addDays, diffDays, getCurrentTime, getCurrentTimeZone, localDay, utcToLocalTime)

import J.Cycles.Graphics
import J.Cycles.Types
import qualified J.Cycles.Streengs as Streengs

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Validation as V
import qualified Graphics.Vty as Vty
import qualified System.Directory as D
import qualified System.FilePath.Posix as FP

singletonNE :: a -> NonEmpty a
singletonNE = flip (:|) []

sequenceE :: forall a e. [Either e a] -> Either (NonEmpty e) [a]
sequenceE ne = bimap NonEmpty.reverse reverse $ go (Right []) ne where
  go :: Either (NonEmpty e) [a] -> [Either e a] -> Either (NonEmpty e) [a]
  go acc [] = acc
  go acc ((Right a):xs) =
    let
      !newAcc = fmap ((:) a) acc
    in
      go newAcc xs
  go acc ((Left e):xs) =
    let
      !newAcc = case acc of
        Left es -> Left (e:|(NonEmpty.toList es))
        Right _ -> Left (singletonNE e)
    in
      go newAcc xs

validateStatus :: RawStatus -> Either InvalidStatus Status
validateStatus Y = Right On
validateStatus N = Right Off
validateStatus (U c) = Left (InvalidStatus c)

-- likely generalizable to any intervals
fillInGaps :: NonEmpty DatedStatus -> NonEmpty DatedStatus
fillInGaps (ds1@(DatedStatus (Interval _ e) _):|ds2@(DatedStatus (Interval s _) _):xs)
  | e < s =
    let
      intervening = Interval e s
    in
      ds1:|((DatedStatus intervening UnknownM):(NonEmpty.toList $ fillInGaps (ds2:|xs)))
fillInGaps (d:|(x:xs)) = d :| (NonEmpty.toList (fillInGaps (x:|xs)))
fillInGaps (d:|[]) = d:|[]

-- also likely generalizable to any intervals
fitClip :: Interval Day -> [DatedStatus] -> NonEmpty DatedStatus
fitClip i [] = singletonNE $ DatedStatus i UnknownM
fitClip (Interval sl el) ([ds@(DatedStatus (Interval start end) st)]) =
  let
    elLeft = DatedStatus (Interval sl start) UnknownM
    elRight = DatedStatus (Interval end el) UnknownM
  in
    NonEmpty.fromList $ case (start > sl, end <= el) of
      (True, True) -> [elLeft, ds, elRight]
      (False, True) -> [DatedStatus (Interval sl end) st, elRight]
      (True, False) -> [elLeft, DatedStatus (Interval start el) st]
      (False, False) -> [DatedStatus (Interval sl el) st]

fitClip (Interval sl el) (d@(DatedStatus (Interval start _) _):ds) =
  let
    lds@(DatedStatus (Interval _ lend) _) = last ds
    i = init ds
    elLeft = DatedStatus (Interval sl start) UnknownM
    elRight = DatedStatus (Interval lend el) UnknownM
    clipL (DatedStatus (Interval s e) t) = flip DatedStatus t $ if s < sl then Interval sl e else Interval s e
    clipR (DatedStatus (Interval s e) t) = flip DatedStatus t $ if e > el then Interval s el else Interval s e
  in
    NonEmpty.fromList $ case (start > sl, lend <= el) of
      (True, True) ->   [elLeft,clipL d] ++ i ++ [clipR lds,elRight]
      (False, True) ->  [clipL d] ++        i ++ [clipR lds,elRight]
      (True, False) ->  [elLeft,clipL d] ++ i ++ [clipR lds]
      (False, False) -> [clipL d] ++        i ++ [clipR lds]

prepareRawEntries ::
  Interval Day ->
  [RawEntry] ->
  Either (NonEmpty InvalidStatus) (NonEmpty DatedStatus)
prepareRawEntries ds entries =
  let
    entryToDatedStatus (RawEntry i rs) = DatedStatus i . toMetaStatus <$> validateStatus rs
    sparseDatedStatuses = sequenceE (entryToDatedStatus <$> entries)
    clippedDenseDatedStatuses = fillInGaps . fitClip ds <$> sparseDatedStatuses
  in
    clippedDenseDatedStatuses

loadBetweenInterval :: Interval Day -> FilePath -> IO (Either LogParsingError (NonEmpty DatedStatus))
loadBetweenInterval ds filePath =
  let
    rawEntries = Streengs.searchForDays ds filePath
    datedStatuses = first LogParsingError . prepareRawEntries ds <$> rawEntries
  in
    datedStatuses

-- does exactly what the type says ;)
buildMapA :: (Traversable t, Applicative f, Ord k) => (k -> f a) -> t k -> f (Map k a)
buildMapA f = fmap (Map.fromList . toList) . traverse tupleAndRun
  where
    tupleAndRun n = (,) n <$> (f n)

-- always loads *all* at once
loadToState ::
  ViewerConfig ->
  PartialViewerState ->
  IO (Either (NonEmpty LoadViewerStateError) ViewerState)
loadToState vc pvs = do
  let logFolderPath = _configLogPath vc
  logExists <- D.doesDirectoryExist logFolderPath
  if not logExists then
    pure $ Left $ singletonNE LogFolderMissing
  else do
    cycleNames <- filter (not . isSuffixOf ".bak") <$> D.listDirectory logFolderPath
    case cycleNames of
      [] -> pure $ Left $ singletonNE LogFolderEmpty
      ns -> do
        csv <- getCompose $ buildMapA (Compose . loadValidation) ns
        return $ V.toEither $ fmap remakeViewerState csv
  where
    pcs = _partialCycleStates pvs
    ci = _partialInterval pvs

    loadEither :: String -> IO (Either LogParsingError CycleState)
    loadEither name =
      let
        existing = Map.lookup name pcs
        offset = maybe 0 _partialBoundOffset existing
        pendingEdits = maybe mempty _partialPendingEdits existing
        remakeCycleState = fmap (flip (CycleState offset) pendingEdits)
      in
        remakeCycleState <$> loadBetweenInterval (shiftIntervalRight offset ci) (_configLogPath vc FP.</> name)

    fixupError :: String -> LogParsingError -> NonEmpty LoadViewerStateError
    fixupError k v = (singletonNE . ErrorsParsingState . singletonNE) (k,v)

    loadValidation :: String -> IO (V.Validation (NonEmpty LoadViewerStateError) CycleState)
    loadValidation k = V.fromEither . first (fixupError k) <$> loadEither k

    -- when the currently selected cycle disappears from the filesystem, which do we select next?
    -- the one before it if any are; otherwise, the one on the top, if it exists.
    findNewCurrentCycle :: Map String CycleState -> Maybe String
    findNewCurrentCycle cs =
      case _partialSelectedCycle pvs of
        Just n ->
          let
            le = Map.lookupLE n cs
          in
            (fst <$> le) <|> defaultName
        Nothing ->
          defaultName
      where
        defaultName = fmap (fst . fst) $ Map.minViewWithKey cs

    remakeViewerState :: Map String CycleState -> ViewerState
    remakeViewerState cs = ViewerState {
      _selectedCycle = findNewCurrentCycle cs
    , _interval = _partialInterval pvs
    , _cursor = _partialCursor pvs
    , _cycleStates = cs
    }

doToAll :: Applicative f => (String -> CycleState -> f CycleState) -> ViewerState -> f ViewerState
doToAll f vs = fmap (\cs -> vs { _cycleStates = cs }) (Map.traverseWithKey f (_cycleStates vs))

shiftIntervalRight :: Integral a => a -> Interval Day -> Interval Day
shiftIntervalRight n (Interval s e) =
  let ad = addDays (fromIntegral n) in Interval (ad s) (ad e)

moveViewerRight :: Integral a => a -> ViewerConfig -> PartialViewerState -> IO ViewerState
moveViewerRight n vc pvs =
  let
    newInterval = shiftIntervalRight n (_partialInterval pvs)

    newViewerState = loadToState vc pvs { _partialInterval = newInterval }
  in
    V.valueOr (const (error "error moving viewer")) <$> newViewerState

moveCycleRight :: Integral a => a -> ViewerConfig -> PartialViewerState -> IO ViewerState
moveCycleRight n vc pvs =
  let
    cc = _partialSelectedCycle pvs
    pcs = _partialCycleStates pvs

    moveState st = st { _partialBoundOffset = _partialBoundOffset st + fromIntegral n }

    updatedCycleStates = case cc of
      Nothing -> pcs
      Just curName -> Map.update (Just . moveState) curName pcs

    newViewerState =
      loadToState vc pvs { _partialCycleStates = updatedCycleStates }
  in
    V.valueOr (const (error "error moveing cycle")) <$> newViewerState

moveCursorRight :: Integral a => a -> ViewerConfig -> ViewerState -> IO ViewerState
moveCursorRight n vc vs =
  let
    ds = _interval vs
    curs = _cursor vs
    dayCount = fromIntegral $ diffDays (intervalEnd ds) (intervalStart ds)
    movedCursor = curs + fromIntegral n
    clippedCursor = clamp 0 (dayCount - 1) movedCursor
    excess = movedCursor - clippedCursor
    setCursor s = s { _cursor = clippedCursor }
  in
    if excess /= 0 then
      setCursor <$> moveViewerRight excess vc (forgetVS vs)
    else
      pure $ setCursor vs

dumpAndDie :: ViewerState -> String -> a
dumpAndDie vs s = error (s ++ "\n\nState: \n" ++ printViewerState vs)

dumpAndDieC :: ViewerConfig -> ViewerState -> String -> a
dumpAndDieC vc vs s = error (s ++ "\n\nConfig: \n" ++ printViewerConfig vc ++ "\n\nState: \n" ++ printViewerState vs)

alterCurrentStatus :: (MetaStatus -> Maybe MetaStatus -> MetaStatus) -> ViewerState -> ViewerState
alterCurrentStatus alterStatus vs =
  let
    cyclesMissing = dumpAndDie vs "Missing cycles being altered"

    dayAtCursor = addDays (fromIntegral $ _cursor vs) (intervalStart $ _interval vs)

    newVs = do
      cc <- _selectedCycle vs
      let newCycleStates = Map.adjust newCycleState cc (_cycleStates vs)
      Just (vs { _cycleStates = newCycleStates })

    newCycleState cycleState =
      let
        cycleStatus = fromMaybe cyclesMissing $ datedStatus <$>
          find (flip intervalContains dayAtCursor . statusDates) (_cycleHistory cycleState)
        PendingEdits pendingEdits = _cyclePendingEdits cycleState
        alterEditStatus editStatus =
          let
            newStatus = alterStatus cycleStatus editStatus
            -- we don't want it to show up as an edit if it's the same as on-disk
            redundant = newStatus == cycleStatus
          in
            if redundant then Nothing else Just newStatus
      in
        cycleState {
          _cyclePendingEdits = PendingEdits $ Map.alter alterEditStatus dayAtCursor pendingEdits
        }
  in
    fromMaybe cyclesMissing newVs

togCurrentStatus :: ViewerState -> ViewerState
togCurrentStatus = alterCurrentStatus (\s e -> tog $ fromMaybe s e) where
  tog OffM = OnM
  tog OnM = OffM
  tog UnknownM = OnM

delCurrentStatus :: ViewerState -> ViewerState
delCurrentStatus = alterCurrentStatus (const $ const UnknownM)

resetCell :: ViewerState -> ViewerState
resetCell = alterCurrentStatus (const . id)

saveEditsAndReload :: ViewerConfig -> ViewerState -> IO (Either (NonEmpty LoadViewerStateError) ViewerState)
saveEditsAndReload vc vs = do
  let saveCycle name state = Streengs.applyEdits (_cyclePendingEdits state) (_configLogPath vc FP.</> name) $> (state { _cyclePendingEdits = mempty })
  allSaved <- doToAll saveCycle vs
  let pvs = forgetVS allSaved
  loadToState vc pvs

handleAppEvent ::
  ViewerConfig ->
  ViewerEvent ->
  ViewerState ->
  EventM () (Next ViewerState)

handleAppEvent vc (MoveViewerRight n) vs =
  liftIO (moveViewerRight n vc (forgetVS vs)) >>= continue

handleAppEvent vc (MoveCycleRight n) vs =
  liftIO (moveCycleRight n vc (forgetVS vs)) >>= continue

handleAppEvent vc (MoveCursorRight n) vs =
  liftIO (moveCursorRight n vc vs) >>= continue

handleAppEvent vc Refresh vs = do
  -- note that this makes refresh a special case of moving, the identity move
  let err e = dumpAndDieC vc vs $ "refresh failed: " ++ show e
  newStateOrErr <- liftIO $ loadToState vc (forgetVS vs)
  continue =<< either err pure newStateOrErr

handleAppEvent _ MoveUp vs = do
  let states = _cycleStates vs
  let newState = vs { _selectedCycle = case _selectedCycle vs of
    Nothing -> (fst . fst) <$> Map.minViewWithKey states
    Just x -> (Just . fromMaybe x) (fst <$> Map.lookupLT x states)
  }
  continue newState

handleAppEvent _ MoveDown vs@ViewerState { _selectedCycle = n } = do
  let states = _cycleStates vs
  let newState = vs { _selectedCycle = case n of
    Nothing -> (fst . fst) <$> Map.maxViewWithKey states
    Just x  -> (Just . fromMaybe x) (fst <$> Map.lookupGT x states)
  }
  continue newState

handleAppEvent vc ResetAll vs = do
  let err e = dumpAndDieC vc vs $ "resetting all failed: " ++ show e
  newStateOrErr <- liftIO $ initialState vc
  continue =<< either err pure newStateOrErr

handleAppEvent _ ResetCell vs = do
  continue (resetCell vs)

handleAppEvent _ Toggle vs = do
  continue (togCurrentStatus vs)

handleAppEvent _ Delete vs = do
  continue (delCurrentStatus vs)

handleAppEvent vc Save vs = do
  -- when errors become recoverable, don't throw edits out
  let err e = dumpAndDieC vc vs $ "saving edits failed: " ++ show e
  newStateOrErr <- liftIO $ saveEditsAndReload vc vs
  continue =<< either err pure newStateOrErr

handleAppEvent _ Debug vs = do
  liftIO (putStrLn $ printViewerState vs) *> continue vs

handleAppEvent vc e vs = dumpAndDieC vc vs $ "Unhandled app event received; this may mean a feature is not yet implemented: " ++ show e

handleVtyEvent ::
  (ViewerEvent -> ViewerState -> EventM () (Next ViewerState)) ->
  Vty.Event ->
  ViewerState ->
  EventM () (Next ViewerState)
handleVtyEvent handle (Vty.EvKey (Vty.KChar 'a') []) =
  handle (MoveCursorRight (-1))

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'd') []) =
  handle (MoveCursorRight 1)

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl]) =
  handle (MoveViewerRight (-1))

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) =
  handle (MoveViewerRight 1)

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'A') []) =
  handle (MoveViewerRight (-5))

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'D') []) =
  handle (MoveViewerRight 5)

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'w') []) =
  handle MoveUp

handleVtyEvent handle (Vty.EvKey (Vty.KChar 's') []) =
  handle MoveDown

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'r') []) =
  handle ResetCell

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'R') []) =
  handle ResetAll

handleVtyEvent handle (Vty.EvKey (Vty.KChar ' ') []) =
  handle Refresh

handleVtyEvent handle (Vty.EvKey Vty.KDel []) =
  handle Delete

handleVtyEvent handle (Vty.EvKey Vty.KEnter []) =
  handle Toggle

handleVtyEvent handle (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) =
  handle Save

handleVtyEvent handle (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) =
  handle Debug

handleVtyEvent _ (Vty.EvKey Vty.KEsc []) =
  halt

handleVtyEvent _ _ =
  continue

handleEvent ::
  ViewerConfig ->
  ViewerState ->
  BrickEvent () ViewerEvent ->
  EventM () (Next ViewerState)
handleEvent vc vs (VtyEvent e) = handleVtyEvent (handleAppEvent vc) e vs
handleEvent _ vs _ = continue vs

-- just return "today" + `i`.
adjustToday :: Integer -> IO Day
adjustToday i = addDays i . localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

initialState ::
  ViewerConfig ->
  IO (Either (NonEmpty LoadViewerStateError) ViewerState)
initialState vc = do
  left <- adjustToday (fromIntegral $ -(_configIntervalSize vc - 1))
  right <- adjustToday 1
  let ds = Interval left right
  loadToState vc (freshPVS ds)

app :: AttrMap -> ViewerConfig -> App ViewerState ViewerEvent ()
app mapping vc =
  App {
    appDraw = showViewerFromState
  , appChooseCursor = const (const Nothing)
  , appHandleEvent = handleEvent vc
  , appStartEvent = pure
  , appAttrMap = const mapping
  }

brickMain :: ViewerConfig -> IO ()
brickMain vc = do
  customizedTheme <- fromRight undefined <$> loadCustomizations "theme.ini" defaultTheme
  let mapping = themeToAttrMap customizedTheme
  is <- fromRight (error "error in initialstate") <$> initialState vc
  _ <- customMain
    (Vty.mkVty Vty.defaultConfig)
    Nothing (app mapping vc) is
  return ()
