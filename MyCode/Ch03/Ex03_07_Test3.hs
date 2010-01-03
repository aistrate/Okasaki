import Test.HUnit
import Test.QuickCheck
import Data.List (sort)
import Text.Printf (printf)
import TestHelper
import BinomialHeap
import Ex03_07


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "insert/findMin" ~:
  [
    findMin (insert 'e' (empty::ExplicitMin BinomialHeap Char)) ~?= 'e',
    findMin (insert 'f' (insert 'e' (empty::ExplicitMin BinomialHeap Char))) ~?= 'e',
    findMin (insert 'e' (insert 'f' (empty::ExplicitMin BinomialHeap Char))) ~?= 'e'
  ],
  "fromList/sorted input" ~:
  [
    testHeap "" ~? "empty list",
    testHeap ['a'..'z'] ~? "ascending",
    testHeap ['z','y'..'a'] ~? "descending",
    testHeap (['a'..'z'] ++ ['z','y'..'a']) ~? "combined asc/desc",
    testHeap (['z','y'..'a'] ++ ['a'..'z']) ~? "combined desc/asc",
    testHeap (replicate 100 'a') ~? "constant"
  ] ]

qcheckTests = [ 
  ("fromList/properties", qcheck (testHeap2::[Int] -> Property)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 100,
                                 configMaxFail = 2000,
                                 configEvery = \n args -> show n ++ ":\n" ++ 
                                                          unlines args }

testHeap2 :: Ord a => [a] -> Property
testHeap2 xs = --length xs > 1 ==> sum xs >= 20
               -- (not . null . drop 5 $ xs)
               length xs > 1 && head xs /= head (tail xs)
               ==> length xs >= 5
               ==> (let sorted = sort xs
                    in testHeap' (fromList_fold xs) sorted &&
                       testHeap' (fromList_pairs xs) sorted)

testHeap xs = let sorted = sort xs
              in testHeap' (fromList_fold xs) sorted &&
                 testHeap' (fromList_pairs xs) sorted

testHeap' :: Ord a => ExplicitMin BinomialHeap a -> [a] -> Bool
testHeap' h xs = toSortedList h == xs
