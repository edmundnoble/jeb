{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language NamedFieldPuns #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}

module J.Cycles.Types(
  Status(..), MetaStatus(..), toMetaStatus, fromMetaStatus
, RawStatus(..), statusToRawStatus
, TimeSpan(..), intervalContains, intervalStart, intervalEnd, contpareTimeSpans, timespansIntersect, spanContainsSpan
, DatedStatus(..), datedStatus, statusDates
, PendingEdits(..)
, CycleHistory(..), CycleState(..), PartialCycleState(..), PartialViewerState(..), printCycleState
, forgetCS, forgetVS, freshPVS
, ViewerConfig(..), ViewerEvent(..), ViewerState(..), printViewerConfig, printViewerState
, InvalidStatus(..), LogParsingError(..), LoadViewerStateError(..)
, RawEntry(..), entrySize
, Edit(..)
) where

import Control.DeepSeq(NFData)
import Data.List.NonEmpty(NonEmpty)
import Data.Map.Strict(Map)
import Data.Time.Calendar(Day(..))
import Data.Semigroup(Semigroup(..))
import Foreign.Ptr(Ptr, castPtr, plusPtr)
import Foreign.Storable(Storable(..))
import GHC.Generics(Generic)
import GHC.Prim(chr#, word2Int#)
import GHC.Types(Char(C#))
import GHC.Word(Word8(W8#), Word64)
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

data TimeSpan = TimeSpan {-# unpack #-} !Day {-# unpack #-} !Day deriving (Eq, Show)

instance Pretty TimeSpan where
  pretty (TimeSpan s e) = text (show s <> " -> " <> show e)

{-# inline intervalStart #-}
intervalStart :: TimeSpan -> Day
intervalStart (TimeSpan a _) = a

{-# inline intervalEnd #-}
intervalEnd :: TimeSpan -> Day
intervalEnd (TimeSpan _ a) = a

{-# inline intervalContains #-}
intervalContains :: TimeSpan -> Day -> Bool
intervalContains (TimeSpan s e) a = s <= a && a < e

{-# inline spanContainsSpan #-}
spanContainsSpan :: TimeSpan -> TimeSpan -> Bool
spanContainsSpan (TimeSpan k1 k2) (TimeSpan k1' k2') = k1 <= k1' && k2' <= k2

{-# inline timespansIntersect #-}
timespansIntersect :: TimeSpan -> TimeSpan -> Bool
timespansIntersect i1 i2 = (contpareTimeSpans i1 i2) == EQ

{-# inline clipTo #-}
clipTo :: TimeSpan -> TimeSpan -> TimeSpan

{-# inline contpareTimeSpans #-}
contpareTimeSpans :: TimeSpan -> TimeSpan -> Ordering
contpareTimeSpans (TimeSpan k1 k2) (TimeSpan k1' k2') =
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

data DatedStatus = DatedStatus !(TimeSpan) !MetaStatus deriving (Eq, Show)

{-# inline statusDates #-}
statusDates :: DatedStatus -> TimeSpan
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
  "Log path: " ++ _configLogPath ++
  "\nInterval size: " ++ show _configIntervalSize

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
, _interval :: !TimeSpan
, _selectedCycle :: !(Maybe String)
}

data PartialViewerState = PartialViewerState {
  _partialCursor :: !Int
, _partialCycleStates :: !(Map String PartialCycleState)
, _partialInterval :: !TimeSpan
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

freshPVS :: TimeSpan -> PartialViewerState
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

data RawStatus = Y | N | U !Char deriving (Eq, Show)

statusToRawStatus :: Status -> RawStatus
statusToRawStatus On = Y
statusToRawStatus Off = N

data RawEntry = RawEntry {
  _entryToTimeSpan :: !TimeSpan
, _entryRawStatus :: !RawStatus
} deriving (Eq, Show)

{-# inline entryLocs #-}
-- todo: why not return a `Ptr (TimeSpan)` instead?
entryLocs :: Ptr RawEntry -> (Ptr Day, Ptr Day, Ptr RawStatus)
entryLocs p =
  let
    sp = castPtr p
    ep = sp `plusPtr` sizeOf (undefined :: Word64)
    stp = castPtr $ ep `plusPtr` sizeOf (undefined :: Word64)
  in
    (sp, ep, stp)

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
pokeRawStatus p s = poke p' $ case s of
  N -> 'N'
  Y -> 'Y'
  U c -> c
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


instance Storable RawEntry where
    -- one 64-bit int for each time, one byte for status
  sizeOf _ = sizeOf (undefined :: Word64) * 2 + sizeOf (undefined :: Word8)
  alignment _ = 1
  peek p =
    let (sp, ep, stp) = entryLocs p in
      RawEntry <$> (TimeSpan <$> peekDay sp <*> peekDay ep) <*> peekRawStatus stp
  poke p (RawEntry (TimeSpan s e) st) =
    let (sp, ep, stp) = entryLocs p in
      pokeDay sp s >> pokeDay ep e >> pokeRawStatus stp st

data Edit = Edit {
  _editDay :: Day
, _editStatus :: MetaStatus
} deriving Show
