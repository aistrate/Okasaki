import Test.HUnit
import Test.QuickCheck
import Text.Printf (printf)
import TestHelper
import Data.List (sort)
import Ex04_02


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "(fully/partially) sorted input" ~:
  [
    testSort "" ~? "empty list",
    testSort ['a'..'z'] ~? "ascending",
    testSort ['z','y'..'a'] ~? "descending",
    testSort (['a'..'z'] ++ ['z','y'..'a']) ~? "combined asc/desc",
    testSort (['z','y'..'a'] ++ ['a'..'z']) ~? "combined desc/asc",
    testSort (replicate 100 'a') ~? "constant",
    testSort ([1,5..1000] ++ reverse [2,6..1000] ++ 
              [3,7..1000] ++ reverse [4,8..1000]) ~? "medium"
  ], 
  
  "first elements only" ~:
  [
    take 10 (insertionSort [1..100000]) ~?= [1..10],
    take 10 (insertionSort [100000,99999..1]) ~?= [1..10]
  ]
  ]

qcheckTests = [ 
  ("random input", qcheck (testSort::[Int] -> Bool)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 1000 }


testSort xs = insertionSort xs == sort xs
