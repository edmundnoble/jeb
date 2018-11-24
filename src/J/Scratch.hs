{-# language PartialTypeSignatures #-}
{-# language ViewPatterns #-}
{-# language ScopedTypeVariables #-}

module J.Scratch where

import Brick.BChan(BChan)
import Brick.Main
import Brick.Types hiding (Max, Min)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.AttrMap(attrMap)

import Data.Foldable(traverse_)

import Foreign.Ptr(Ptr, castPtr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable

import Control.Exception(SomeException, catch)
import Control.Monad
import Control.Monad.IO.Class(liftIO)
import Data.Bifunctor(first)
import Data.Either(fromRight)
import Data.Map(Map)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Maybe
import Data.Time
import GHC.IO.Unsafe
import System.IO.MMap(mmapWithFilePtr)

import Text.PrettyPrint.ANSI.Leijen(Pretty(..), text, line, nest)

import qualified Brick.BChan as BChan
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Validation as V
import qualified Graphics.Vty
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as D
import qualified System.FilePath.Posix as FP
import qualified System.Posix.Files as PF
import qualified System.Posix.Types as PT(COff(..))
import qualified System.IO.MMap as MMap

import J.Cycles.Main
import J.Cycles.Streengs
import J.Cycles.Types

cycleLogPath = "logs"

vc = ViewerConfig cycleLogPath 12

bm = brickMain vc

-- today = unsafePerformIO $ adjustToday 0

d = read "2001-01-01"

shiteI :: TimeSpan
shiteI = TimeSpan (read "2018-10-28") (read "2018-11-04")

idn n1 n2 = TimeSpan (n1 `addDays` d) (n2 `addDays` d)

-- myWidgetL = flip showViewer (idn 0 8) cyclePaths

del = D.removeFile "entries" *> D.removeFile "entries.bak"

day1 = read "2018-10-25" :: Day
day2 = read "2018-10-26" :: Day
day3 = read "2018-10-27" :: Day
day4 = read "2018-10-28" :: Day
day5 = read "2018-10-29" :: Day
day6 = read "2018-10-30" :: Day
day7 = read "2018-10-31" :: Day
day15 = read "2018-11-03" :: Day
day16 = read "2018-11-04" :: Day

es = [RawEntry (TimeSpan day1 day3) Y, RawEntry (TimeSpan day3 day4) N, RawEntry (TimeSpan day4 day5) Y, RawEntry (TimeSpan day7 day15) N]

fuckit = flip catch (\(e :: SomeException) -> pure undefined)

we1 = RawEntry (TimeSpan (read "2001-01-05") (read "2001-01-06")) Y
we2 = RawEntry (TimeSpan (read "2001-01-01") (read "2001-01-02")) Y
we3 = RawEntry (TimeSpan (read "2001-01-03") (read "2001-01-04")) Y

-- es2 = readEs "entries"

scratch d = unsafePerformIO $ do
  searchForDays d "entries" >>= print

scratchI t = unsafePerformIO $ do
  entries <- searchForDays t "entries"
  print entries

serialize :: Storable a => Ptr a -> [a] -> IO ()
serialize p xs = traverse_ serializeInd $ zip xs [0..]
  where
    serializeInd (x, i) =
      pokeElemOff p i x

-- l = length es * entrySize

writeEs :: forall a. Storable a => FilePath -> [a] -> IO ()
writeEs fp as = do
  let size = length as * (sizeOf (undefined :: a))
  PT.COff fileSize <- PF.fileSize <$> PF.getFileStatus fp
  let write = MMap.mmapWithFilePtr fp MMap.ReadWriteEx (Just (0,size)) (\(p, _) -> serialize (castPtr p) as)
  if size > fromIntegral fileSize then
    write
  else
    D.removeFile fp *> write

readS :: forall a. Storable a => FilePath -> IO [a]
readS fp =
  mmapWithFilePtr fp MMap.ReadOnly Nothing (\(p, s) -> readP (castPtr p) s)

readP :: forall a. Storable a => Ptr a -> Int -> IO [a]
readP ptr s =
  if s == 0 then pure [] else NonEmpty.toList <$> traverse peek (allPtrs ptr s)

readEs :: FilePath -> IO [RawEntry]
readEs = readS

      -- consFiles a fpn ((n,k):fps) = MMap.mmapWithFile
      -- consFiles a fpn [] = MMap.mmapWithFile

-- queryLog :: _1 -> _2
-- myFirstCH = renderCycleHistory myPalette (TimeSpan d (5 `addDays` d)) (head myDayMap)
