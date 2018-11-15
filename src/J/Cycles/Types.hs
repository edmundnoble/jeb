{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language NamedFieldPuns #-}

module J.Cycles.Types(
  Status(..), MetaStatus(..), toMetaStatus, fromMetaStatus
, Interval(..), intervalContains, intervalStart, intervalEnd
, DatedStatus(..), datedStatus, statusDates
, PendingEdits(..)
, CycleHistory(..), CycleState(..), PartialCycleState(..), PartialViewerState(..), printCycleState
, forgetCS, forgetVS, freshPVS
, ViewerConfig(..), ViewerEvent(..), ViewerState(..), printViewerConfig, printViewerState
, InvalidStatus(..), LogParsingError(..), LoadViewerStateError(..)
) where

import Control.DeepSeq(NFData)
import Data.List(intercalate)
import Data.List.NonEmpty(NonEmpty)
import Data.Map.Strict(Map)
import Data.Time.Calendar(Day)
import Data.Semigroup(Semigroup(..))
import GHC.Generics(Generic)
import Text.PrettyPrint.ANSI.Leijen(Pretty(..), text)

import qualified Data.Map.Strict as Map

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

data Interval a = Interval !a !a deriving (Eq, Show)

instance Pretty a => Pretty (Interval a) where
  pretty (Interval s e) = pretty s <> text " -> " <> pretty e

{-# inline intervalStart #-}
intervalStart :: Interval a -> a
intervalStart (Interval a _) = a

{-# inline intervalEnd #-}
intervalEnd :: Interval a -> a
intervalEnd (Interval _ a) = a

{-# inline intervalContains #-}
intervalContains :: Ord a => Interval a -> a -> Bool
intervalContains (Interval s e) a = s <= a && a <= e

data ViewerEvent
  = HideCycle !String
  | ShowCycle !String
  | ShowOnlyCycle !String
  | ShowAllCycles
  | MoveViewerRight !Int
  | MoveCursorRight !Int
  | MoveCycleRight !Int
  | MoveUp
  | MoveDown
  | Refresh
  | ResetAll
  | ResetCell
  | Toggle
  | Delete
  | Save
  | Debug deriving Show

data DatedStatus = DatedStatus !(Interval Day) !MetaStatus deriving (Eq, Show)

{-# inline statusDates #-}
statusDates :: DatedStatus -> Interval Day
statusDates (DatedStatus i _) = i

datedStatus :: DatedStatus -> MetaStatus
datedStatus (DatedStatus _ s) = s

data CycleHistory = CycleHistory {
  _historyName :: !String
, _historyTransitions :: !(NonEmpty DatedStatus)
} deriving (Eq, Show)

data ViewerConfig = ViewerConfig {
  _configLogPath :: !FilePath
, _configIntervalSize :: !Int
} deriving Show

printViewerConfig :: ViewerConfig -> String
printViewerConfig ViewerConfig {
  _configLogPath,
  _configIntervalSize
} =
  "Config log path: " ++ _configLogPath ++
  "\nConfig interval size: " ++ show _configIntervalSize

newtype PendingEdits = PendingEdits { _getPendingEdits :: Map Day MetaStatus }
  deriving Show

instance Monoid (PendingEdits) where
  mappend (PendingEdits e1) (PendingEdits e2) = PendingEdits (Map.union e1 e2)
  mempty = PendingEdits Map.empty

data CycleState = CycleState {
  _cycleBoundOffset :: !Int
, _cycleHistory :: !(NonEmpty DatedStatus)
, _cyclePendingEdits :: !PendingEdits
-- todo: remove Show instance
} deriving Show

printViewerState :: ViewerState -> String
printViewerState ViewerState {
  _cursor,
  _cycleStates,
  _interval,
  _selectedCycle
} =
  "Cursor offset: " ++ show _cursor ++
  "\nCycle states: " ++ (printCycleStates _cycleStates) ++
  "\nInterval: " ++ show _interval ++
  "\nSelected cycle: " ++ show _selectedCycle

printCycleStates :: Map String CycleState -> String
printCycleStates = show

printCycleState :: CycleState -> String
printCycleState CycleState { _cycleBoundOffset, _cycleHistory, _cyclePendingEdits }  =
  "  Cycle offset: " ++ show _cycleBoundOffset ++
  "\n  Cycle history: " ++ printCycleHistory _cycleHistory ++
  "\n  Cycle pending edits: " ++ printPendingEdits _cyclePendingEdits
    where
      printCycleHistory = show
      printPendingEdits = show

data PartialCycleState = PartialCycleState {
  _partialBoundOffset :: !Int
, _partialPendingEdits :: !PendingEdits
} deriving Show

data ViewerState = ViewerState {
  _cursor :: !Int
, _cycleStates :: !(Map String CycleState)
, _interval :: !(Interval Day)
, _selectedCycle :: !(Maybe String)
}

data PartialViewerState = PartialViewerState {
  _partialCursor :: !Int
, _partialCycleStates :: !(Map String PartialCycleState)
, _partialInterval :: !(Interval Day)
, _partialSelectedCycle :: !(Maybe String)
} deriving Show

forgetCS :: CycleState -> PartialCycleState
forgetCS CycleState {_cycleBoundOffset, _cyclePendingEdits} =
  PartialCycleState {_partialBoundOffset = _cycleBoundOffset, _partialPendingEdits = _cyclePendingEdits}

forgetVS :: ViewerState -> PartialViewerState
forgetVS ViewerState {_interval, _selectedCycle, _cursor, _cycleStates} = PartialViewerState {
  _partialCursor = _cursor
, _partialCycleStates = forgetCS <$> _cycleStates
, _partialInterval = _interval
, _partialSelectedCycle = _selectedCycle
}

freshPVS :: Interval Day -> PartialViewerState
freshPVS ds = PartialViewerState {
  _partialCursor = 0
, _partialCycleStates = Map.empty
, _partialSelectedCycle = Nothing
, _partialInterval = ds
}

newtype InvalidStatus = InvalidStatus Char deriving (Eq, Show)

data LogParsingError
  = LogParsingError !(NonEmpty InvalidStatus)
  | LogEmpty deriving Show

data LoadViewerStateError
  = ErrorsParsingState !(NonEmpty (String, LogParsingError))
  | LogFolderEmpty
  | LogFolderMissing deriving Show
