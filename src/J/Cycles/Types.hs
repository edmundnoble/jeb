{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language NamedFieldPuns #-}

module J.Cycles.Types(
  Status(..), MetaStatus(..), toMetaStatus, fromMetaStatus
, Interval(..), intervalStart, intervalEnd
, DatedStatus(..)
, CycleName(..)
, CycleHistory(..), CycleState(..), PartialCycleState(..), PartialViewerState(..)
, forgetCS, forgetVS, freshPVS
, ViewerConfig(..), ViewerEvent(..), ViewerState(..)
, InvalidStatus(..), LogParsingError(..), LoadViewerStateError(..)
) where

import Control.DeepSeq(NFData)
import Data.List.NonEmpty(NonEmpty)
import Data.Map(Map)
import Data.Time.Calendar(Day)
import Data.Semigroup(Semigroup(..))
import GHC.Generics(Generic)
import Text.PrettyPrint.ANSI.Leijen(Pretty(..), text)

import qualified Data.Map as Map

data Status = On | Off deriving (Eq, Generic, NFData, Show)

data MetaStatus = UnknownM | OnM | OffM deriving (Eq, Generic, NFData, Show)

{-# inline conlike toMetaStatus #-}
toMetaStatus :: Status -> MetaStatus
toMetaStatus On = OnM
toMetaStatus Off = OffM

{-# inline conlike fromMetaStatus #-}
fromMetaStatus :: MetaStatus -> Maybe Status
fromMetaStatus OnM = Just On
fromMetaStatus OffM = Just Off
fromMetaStatus UnknownM = Nothing

data Interval a = Interval a a deriving (Eq, Show)

instance Pretty a => Pretty (Interval a) where
  pretty (Interval s e) = pretty s <> text " -> " <> pretty e

{-# inline intervalStart #-}
intervalStart :: Interval a -> a
intervalStart (Interval a _) = a

{-# inline intervalEnd #-}
intervalEnd :: Interval a -> a
intervalEnd (Interval _ a) = a

data ViewerEvent
  = HideCycle String
  | ShowCycle String
  | ShowOnlyCycle String
  | ShowAllCycles
  | MoveLeft Int
  | MoveRight Int
  | MoveCycleLeft Int
  | MoveCycleRight Int
  | MoveUp
  | MoveDown
  | Refresh
  | ResetAll

newtype CycleName = CycleName (Maybe String) deriving (Eq, Ord, Show)

data DatedStatus = DatedStatus (Interval Day) MetaStatus deriving (Eq, Show)

data CycleHistory = CycleHistory {
  _historyName :: String
, _historyTransitions :: NonEmpty DatedStatus
} deriving (Eq, Show)

data ViewerConfig = ViewerConfig {
  _configLogPath :: FilePath
, _configIntervalSize :: Int
} deriving Show

data CycleState = CycleState {
  _stateBoundOffset :: Int
, _stateHistory :: NonEmpty DatedStatus
} deriving Show

data ViewerState = ViewerState {
  _currentCycle :: CycleName
, _currentInterval :: Interval Day
, _currentScreenIndex :: Int
, _cycleStates :: Map String CycleState
} deriving Show

data PartialCycleState = PartialCycleState {
  _partialStateBoundOffset :: Int
} deriving Show

data PartialViewerState = PartialViewerState {
  _partialCurrentCycle :: CycleName
, _partialCurrentInterval :: Interval Day
, _partialCurrentScreenIndex :: Int
, _partialCycleStates :: Map String PartialCycleState
} deriving Show

forgetCS :: CycleState -> PartialCycleState
forgetCS CycleState {_stateBoundOffset} =
  PartialCycleState {_partialStateBoundOffset = _stateBoundOffset}

forgetVS :: ViewerState -> PartialViewerState
forgetVS ViewerState {_currentInterval, _currentCycle, _currentScreenIndex, _cycleStates} = PartialViewerState {
  _partialCurrentCycle = _currentCycle
, _partialCurrentInterval = _currentInterval
, _partialCurrentScreenIndex = _currentScreenIndex
, _partialCycleStates = forgetCS <$> _cycleStates
}

freshPVS ds = PartialViewerState {
  _partialCurrentCycle = CycleName Nothing
, _partialCurrentInterval = ds
, _partialCurrentScreenIndex = 0
, _partialCycleStates = Map.empty
}

newtype InvalidStatus = InvalidStatus Char deriving (Eq, Show)

data LogParsingError
  = LogParsingError (NonEmpty InvalidStatus)
  | LogEmpty deriving Show

data LoadViewerStateError
  = ErrorsParsingState (NonEmpty (String, LogParsingError))
  | LogFolderEmpty
  | LogFolderMissing deriving Show
