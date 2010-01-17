import Test.HUnit
import Test.QuickCheck
import Data.List (sort)
import Text.Printf (printf)
import TestHelper
import Ex05_07


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "fromList" ~:
  [
    testSort "" ~? "empty",
    testSort ['a'..'z'] ~? "ascending",
    testSort ['z','y'..'a'] ~? "descending",
    testSort (['a'..'z'] ++ ['z','y'..'a']) ~? "combined asc/desc",
    testSort (['z','y'..'a'] ++ ['a'..'z']) ~? "combined desc/asc",
    testSort (['a'..'z'] ++ replicate 20 'm') ~? "asc/const"
  ] ]

qcheckTests = [ 
  ("fromList", qcheck (testSort :: [Int] -> Bool)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 500 }


testSort xs = sortWithHeap xs == sort xs
