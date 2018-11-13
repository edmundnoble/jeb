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
import Brick.Themes (Theme, loadCustomizations, newTheme, themeToAttrMap)
import Brick.Types hiding (Max, Down)
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Util(clamp, fg, on)

import Control.Applicative((<|>))
import Control.Monad.IO.Class(liftIO)
import Control.Monad.State
import Data.Bifunctor(Bifunctor(..))
import Data.Either(fromRight)
import Data.Map(Map)
import Data.Maybe(fromMaybe)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map as Map(fromList, toList)
import Data.Semigroup((<>), Max(..))
import Data.Time(Day(..), addDays, diffDays, getCurrentTime, getCurrentTimeZone, localDay, utcToLocalTime)

import J.Cycles.Types
import qualified J.Cycles.Streengs as Streengs

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Validation as V
import qualified Graphics.Vty as Vty
import qualified System.Directory as D
import qualified System.FilePath.Posix as FP

fromListMaybe :: [a] -> Maybe (NonEmpty a)
fromListMaybe [] = Nothing
fromListMaybe (x:xs) = Just (x :| xs)

pattern Nel :: forall a. NonEmpty a -> [a]
pattern Nel nel <- (fromListMaybe -> Just nel)

jBorder :: Widget n -> Widget n
jBorder = withBorderStyle unicode . borderWithLabel (str "J!")

validateStatus :: Streengs.RawStatus -> Either InvalidStatus Status
validateStatus Streengs.Y = Right On
validateStatus Streengs.N = Right Off
validateStatus (Streengs.U c) = Left (InvalidStatus c)

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

sequenceNE :: forall a e. NonEmpty (Either e a) -> Either (NonEmpty e) (NonEmpty a)
sequenceNE = second (NonEmpty.fromList) . sequenceE . NonEmpty.toList

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
  [Streengs.Entry] ->
  Either (NonEmpty InvalidStatus) (NonEmpty DatedStatus)
prepareRawEntries ds entries =
  let
    entryToDatedStatus (Streengs.Entry i rs) = DatedStatus i <$> (toMetaStatus <$> validateStatus rs)
    sparseDatedStatuses = sequenceE (entryToDatedStatus <$> entries)
    clippedDenseDatedStatuses = (fillInGaps . fitClip ds) <$> sparseDatedStatuses
  in
    clippedDenseDatedStatuses

defaultTheme :: Theme
defaultTheme =
  newTheme (Vty.brightWhite `on` Vty.black)
           [ (attrName "cycleOn", fg Vty.green)
           , (attrName "cycleOff", fg Vty.red)
           , (attrName "cycleUnknown", fg Vty.white)
           , (attrName "cycleSelectedOn", fg Vty.brightGreen)
           , (attrName "cycleSelectedOff", fg Vty.brightRed)
           , (attrName "cycleSelectedUnknown", fg Vty.brightWhite)
           , (attrName "cursorOn", Vty.brightGreen `on` Vty.brightBlack)
           , (attrName "cursorOff", Vty.brightRed `on` Vty.brightBlack)
           , (attrName "cursorUnknown", Vty.brightWhite `on` Vty.brightBlack)
           ]

renderDay :: Int -> Int -> Bool -> String -> MetaStatus -> State Int (Widget CycleName)
renderDay dayWidth offset selected name status =
  let
    -- this is NOT a hyphen, it's a line character
    lineChar = '─'

    offW = if offset == 0 then "" else (" (" ++ show offset ++ ")")
  in
    do
      cur <- get
      modify (\u -> u - 1)
      let color = withAttr $ attrName $ if
        cur == 0 && selected then
            case status of
              OnM ->      "cursorOn"
              OffM ->     "cursorOff"
              UnknownM -> "cursorUnknown"
          else if selected then
            case status of
              OnM ->      "cycleSelectedOn"
              OffM ->     "cycleSelectedOff"
              UnknownM -> "cycleSelectedUnknown"
          else
            case status of
              OnM ->      "cycleOn"
              OffM ->     "cycleOff"
              UnknownM -> "cycleUnknown"
      let line = vLimit 1 $ color $ fill lineChar
      return (hLimit dayWidth $ hBox [line, str (" " ++ name ++ offW ++ " "), line])

renderCycleState :: Int -> Int -> String -> Bool -> CycleState -> Widget CycleName
renderCycleState dayWidth cursor cycleName selected (CycleState { _cycleBoundOffset = offset, _cycleHistory = ch }) =
  let
    renderDatedStatus (DatedStatus i s) =
      let
        dayCount = diffDays (intervalEnd i) (intervalStart i)
        renderSingleDay = renderDay dayWidth offset selected cycleName s
        renderAll = hBox <$> (sequenceA $ replicate (fromIntegral dayCount) renderSingleDay)
      in
        renderAll
    widgets = flip evalState cursor $ NonEmpty.toList <$> traverse renderDatedStatus ch
  in
    padBottom (Pad 1) (hBox widgets)

-- TODO: flesh this out so that longer cycle names are wrapped properly
cycleViewerWidget :: Int -> Int -> CycleName -> [(String, CycleState)] -> Widget CycleName
cycleViewerWidget dayWidth cursor (CycleName selected) (Nel ch) =
  let
    vp :: String -> Widget CycleName -> Widget CycleName
    vp = flip viewport Horizontal . CycleName . Just

    renderCH k = renderCycleState dayWidth cursor k (Just k == selected)
  in
    vBox $ fmap (uncurry renderCH) (NonEmpty.toList ch)

cycleViewerWidget _ _ _ _ =
  str "No entries in log!"

-- todo: display time deltas from today somewhere too
drawTimeline :: Int -> Interval Day -> Widget a
drawTimeline dayWidth (Interval start end) =
  hBox (vLimit 3 . hLimit dayWidth . center . str . show <$> [start..(addDays (-1) end)])

showViewerFromState :: ViewerState -> [Widget CycleName]
showViewerFromState vs =
  let
    dayWidth = 18
    bound = _interval vs
    boundSize = fromIntegral $ intervalEnd bound `diffDays` intervalStart bound
    selected = _selectedCycle vs
    histories = Map.toList (_cycleStates vs)
    cycleViewer = cycleViewerWidget dayWidth (_cursor vs) selected histories
    timeline = drawTimeline dayWidth bound
    decorate = padBottom (Pad 20) . padLeftRight 10 . center . jBorder . padTop (Pad 2)
  in
    [decorate (cycleViewer <=> timeline)]

loadBetweenInterval :: Interval Day -> FilePath -> IO (Either LogParsingError (NonEmpty DatedStatus))
loadBetweenInterval ds filePath =
  let
    rawEntries = Streengs.searchForDays ds filePath
    datedStatuses = (first LogParsingError . prepareRawEntries ds) <$> rawEntries
  in
    datedStatuses

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
    cycleNames <- D.listDirectory logFolderPath
    case cycleNames of
      [] -> pure $ Left $ singletonNE LogFolderEmpty
      ns -> do
        -- ugh fuck this is ugly
        csv <- (fmap Map.fromList . sequenceA) <$> traverse (\n -> fmap (fmap ((,) n)) (loadV n)) ns
        return $ V.toEither $ fmap replaceState csv
  where
    pcs = _partialCycleStates pvs
    ci = _partialInterval pvs

    load :: String -> IO (Either LogParsingError CycleState)
    load name =
      let
        offset = maybe 0 _partialBoundOffset (Map.lookup name pcs)
      in
        fmap (flip (CycleState offset) mempty) <$> loadBetweenInterval (shiftIntervalRight offset ci) (_configLogPath vc FP.</> name)

    fixupError :: String -> LogParsingError -> NonEmpty LoadViewerStateError
    fixupError k v = (singletonNE . ErrorsParsingState . singletonNE) (k,v)

    loadV :: String -> IO (V.Validation (NonEmpty LoadViewerStateError) CycleState)
    loadV k = V.fromEither . first (fixupError k) <$> load k

    findNewCurrentCycle :: Map String CycleState -> CycleName
    findNewCurrentCycle cs =
      case _partialSelectedCycle pvs of
        CycleName (Just n) ->
          let
            le = Map.lookupLE n cs
          in CycleName $ (fst <$> le) <|> defaultName
        CycleName Nothing ->
          CycleName defaultName
      where
        defaultName = fmap (fst . fst) $ Map.minViewWithKey cs

    replaceState :: Map String CycleState -> ViewerState
    replaceState cs = ViewerState {
      _selectedCycle = findNewCurrentCycle cs
    , _interval = _partialInterval pvs
    , _cursor = _partialCursor pvs
    , _cycleStates = cs
    }

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
      CycleName Nothing -> pcs
      CycleName (Just curName) -> Map.update (Just . moveState) curName pcs

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

handleAppEvent ::
  ViewerConfig ->
  ViewerState ->
  ViewerEvent ->
  EventM CycleName (Next ViewerState)
handleAppEvent vc vs (MoveViewerRight n) = do
  newState <- liftIO (moveViewerRight n vc (forgetVS vs))
  continue newState
handleAppEvent vc vs (MoveCycleRight n) = do
  -- liftIO $ putStrLn "RIGHT WAT"
  newState <- liftIO (moveCycleRight n vc (forgetVS vs))
  continue newState
handleAppEvent vc vs (MoveCursorRight n) = do
  newState <- liftIO (moveCursorRight n vc vs)
  -- liftIO $ putStrLn ("right " ++ show (_cursor newState))
  continue newState
handleAppEvent vc vs Refresh = do
  -- note that this makes refresh a special case of moving, the identity move
  Right newState <- liftIO (loadToState vc (forgetVS vs))
  continue newState
handleAppEvent _ vs MoveUp = do
  let states = _cycleStates vs
  let newState = vs { _selectedCycle = case _selectedCycle vs of
    CycleName Nothing -> CycleName $ (fst . fst) <$> Map.minViewWithKey states
    CycleName (Just x) -> (CycleName . Just . fromMaybe x) (fst <$> Map.lookupLT x states)
  }
  continue newState
handleAppEvent _ vs@ViewerState { _selectedCycle = CycleName n } MoveDown = do
  let states = _cycleStates vs
  let newState = vs { _selectedCycle = case n of
    Nothing -> CycleName $ (fst . fst) <$> Map.maxViewWithKey states
    Just x  -> (CycleName . Just . fromMaybe x) (fst <$> Map.lookupGT x states)
  }
  continue newState
handleAppEvent vc _ ResetAll = do
  Right newState <- liftIO $ initialState vc
  continue newState
handleAppEvent _ _ _ = error "unknown app event"

handleVtyEvent ::
  (ViewerEvent -> EventM CycleName (Next ViewerState)) ->
  ViewerState ->
  Vty.Event ->
  EventM CycleName (Next ViewerState)
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'a') []) =
  handle (MoveCursorRight (-1))
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'd') []) =
  handle (MoveCursorRight 1)
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'a') [Vty.MCtrl]) =
  handle (MoveViewerRight (-1))
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) =
  handle (MoveViewerRight 1)
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'A') []) =
  handle (MoveViewerRight (-5))
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'D') []) =
  handle (MoveViewerRight 5)
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'w') []) =
  handle MoveUp
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 's') []) =
  handle MoveDown
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 'R') []) =
  handle ResetAll
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar ' ') []) =
  handle Refresh
handleVtyEvent handle _ (Vty.EvKey Vty.KDel []) =
  handle Delete
handleVtyEvent handle _ (Vty.EvKey Vty.KEnter []) =
  handle Toggle
handleVtyEvent handle _ (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) =
  handle Save
handleVtyEvent _ vs (Vty.EvKey Vty.KEsc []) =
  halt vs
handleVtyEvent _ vs _ =
  continue vs

handleEvent ::
  ViewerConfig ->
  ViewerState ->
  BrickEvent CycleName ViewerEvent ->
  EventM CycleName (Next ViewerState)
handleEvent vc vs (VtyEvent e) = handleVtyEvent (handleAppEvent vc vs) vs e
handleEvent _ vs _ = continue vs

adjustToday :: Integer -> IO Day
adjustToday i = addDays i <$> (localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime))

initialState ::
  ViewerConfig ->
  IO (Either (NonEmpty LoadViewerStateError) ViewerState)
initialState vc = do
  left <- adjustToday (fromIntegral $ -(_configIntervalSize vc - 1))
  right <- adjustToday 1
  let ds = Interval left right
  loadToState vc (freshPVS ds)

app :: AttrMap -> ViewerConfig -> App ViewerState ViewerEvent CycleName
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
