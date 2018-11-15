{-# language BangPatterns #-}
{-# language ExplicitForAll #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoMonomorphismRestriction #-}
{-# language PatternSynonyms #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}

module J.Cycles.Graphics (showViewerFromState, defaultTheme) where

import Brick.AttrMap
import Brick.Themes (Theme, newTheme)
import Brick.Types hiding (Max, Down)
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Util(fg, on)

import Control.Monad.State
import Data.List.NonEmpty(NonEmpty(..))
import Data.Time(Day(..), addDays, diffDays)

import Debug.Trace

import J.Cycles.Types

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as Vty

fromListMaybe :: [a] -> Maybe (NonEmpty a)
fromListMaybe [] = Nothing
fromListMaybe (x:xs) = Just (x :| xs)

pattern Nel :: NonEmpty a -> [a]
pattern Nel nel <- (fromListMaybe -> Just nel)

jBorder :: Widget n -> Widget n
jBorder = withBorderStyle unicode . borderWithLabel (str "J!")

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

renderDay :: Int -> Int -> Bool -> String -> MetaStatus -> Maybe MetaStatus -> State Int (Widget ())
renderDay dayWidth offset selected name status editStatus =
  let
    -- this is NOT a hyphen, it's a line character
    lineChar = '─'

    offW = if offset == 0 then "" else (" (" ++ show offset ++ ")")
  in
    do
      cur <- get
      modify (\u -> u - 1)
      let color st = withAttr $ attrName $ if
        cur == 0 && selected then
            case st of
              OnM ->      "cursorOn"
              OffM ->     "cursorOff"
              UnknownM -> "cursorUnknown"
          else if selected then
            case st of
              OnM ->      "cycleSelectedOn"
              OffM ->     "cycleSelectedOff"
              UnknownM -> "cycleSelectedUnknown"
          else
            case st of
              OnM ->      "cycleOn"
              OffM ->     "cycleOff"
              UnknownM -> "cycleUnknown"
      let uneditedLine = vLimit 1 $ color status $ fill lineChar
      let editedLine es = vLimit 1 $ (color status (str [lineChar]) <+> color es (fill lineChar) <+> color status (str [lineChar]))
      let line = maybe uneditedLine editedLine editStatus
      return (hLimit dayWidth $ hBox [line, str (" " ++ name ++ offW ++ " "), line])

renderCycleState :: Int -> Int -> String -> Bool -> CycleState -> Widget ()
renderCycleState dayWidth cursor cycleName selected (CycleState { _cycleBoundOffset, _cycleHistory, _cyclePendingEdits }) =
  let
    renderDatedStatus (DatedStatus i s) =
      let
        dayCount = diffDays (intervalEnd i) (intervalStart i)
        renderSingleDay = renderDay dayWidth _cycleBoundOffset selected cycleName s
        daysByEdit = renderSingleDay . flip Map.lookup (_getPendingEdits _cyclePendingEdits) <$> [intervalStart i .. intervalEnd i]
        renderAll = hBox <$> sequenceA daysByEdit
      in
        renderAll
    widgets = flip evalState cursor $ NonEmpty.toList <$> traverse renderDatedStatus _cycleHistory
  in
    padBottom (Pad 1) (hBox widgets)

-- TODO: flesh this out so that longer cycle names are wrapped properly
cycleViewerWidget :: Int -> Int -> Maybe String -> [(String, CycleState)] -> Widget ()
cycleViewerWidget dayWidth cursor selected (Nel ch) =
  let
    renderCH k = renderCycleState dayWidth cursor k (Just k == selected)
  in
    vBox $ fmap (uncurry renderCH) (NonEmpty.toList ch)

cycleViewerWidget _ _ _ _ =
  str "No entries in log!"

-- todo: display time deltas from today somewhere too
drawTimeline :: Int -> Interval Day -> Widget a
drawTimeline dayWidth (Interval start end) =
  hBox (vLimit 3 . hLimit dayWidth . center . str . show <$> [start..(addDays (-1) end)])

showViewerFromState :: ViewerState -> [Widget ()]
showViewerFromState vs =
  let
    dayWidth = 18
    bound = _interval vs
    selected = _selectedCycle vs
    histories = Map.toList (_cycleStates vs)
    cycleViewer = cycleViewerWidget dayWidth (_cursor vs) selected histories
    timeline = drawTimeline dayWidth bound
    decorate = padBottom (Pad 15) . padLeftRight 10 . center . jBorder . padTop (Pad 2)
  in
    [decorate (cycleViewer <=> timeline)]
