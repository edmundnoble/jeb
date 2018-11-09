import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Data.Time

import J.Cycles.Streengs
import J.Cycles.Types
import J.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  , parseCrossCycleLogLineTests testParseCrossCycleLogLine
  , parseCrossCycleStateLineTests testParseCrossCycleStateLine
  , parseCrossCycleFileLineTests testParseCrossCycleFileLine
  , testGroup "toList" $ [
    ]
  ]


testParseCrossCycleLogLine n r =
  testCase n $
    assertEqual "" (parseCrossCycleLogLine n) (Just r)

testParseCrossCycleStateLine n r =
  testCase n $
    assertEqual "" (parseCrossCycleStateLine n) (Just r)

testParseCrossCycleFileLine n r =
  testCase n $
    assertEqual "" (parseCrossCycleFileLine n) (Just r)

-- parseCrossCycleLogLineTests :: (String -> CycleLogEntry -> TestTree) -> TestTree
-- parseCrossCycleLogLineTests f = testGroup "parseCrossCycleLogLine" $
--   [ f "CycleName:On -> On:2001-01-01:2001-01-02" (CycleLogEntry "CycleName" (Transition OnM OnM) (ModifiedJulianDay 51910) (ModifiedJulianDay 51911))
--   , f "CycleName:Off -> Off:2001-01-01:2001-01-02" (CycleLogEntry "CycleName" (Transition OffM OffM) (ModifiedJulianDay 51910) (ModifiedJulianDay 51911))
--   , f "CycleName:On -> Off:2001-01-01:2001-01-02" (CycleLogEntry "CycleName" (Transition OnM OffM) (ModifiedJulianDay 51910) (ModifiedJulianDay 51911))
--   , f "CycleName:Off -> On:2001-01-01:2001-01-02" (CycleLogEntry "CycleName" (Transition OffM OnM) (ModifiedJulianDay 51910) (ModifiedJulianDay 51911)) ]

parseCrossCycleStateLineTests :: (String -> CycleStateEntry -> TestTree) -> TestTree
parseCrossCycleStateLineTests f = testGroup "parseCrossCycleStateLine" $
  [ f "CycleName:On:2001-01-01" (CycleStateEntry "CycleName" OnM (ModifiedJulianDay 51910))
  , f "CycleName:Off:2001-01-01" (CycleStateEntry "CycleName" OffM (ModifiedJulianDay 51910)) ]

parseCrossCycleFileLineTests :: (String -> CycleFileEntry -> TestTree) -> TestTree
parseCrossCycleFileLineTests f = testGroup "parseCrossCycleStateLine" $
  [ f "CycleName: On" (CycleFileEntry OnM "CycleName")
  , f "CycleName: Off" (CycleFileEntry OffM "CycleName") ]

