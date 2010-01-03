import Test.HUnit
import Test.QuickCheck
import Data.List (sort)
import Text.Printf (printf)
import TestHelper
--import BinomialHeap
import Ex03_05


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "insert/findMin" ~:
  [
    findMin (insert 'e' (empty::BinomialHeap Char)) ~?= 'e',
    findMin (insert 'f' (insert 'e' (empty::BinomialHeap Char))) ~?= 'e',
    findMin (insert 'e' (insert 'f' (empty::BinomialHeap Char))) ~?= 'e'
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
  ("fromList/properties", qcheck (testHeap::[Int] -> Bool)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 500 }


testHeap xs = let sorted = sort xs
              in testHeap' (fromList_fold xs) sorted &&
                 testHeap' (fromList_pairs xs) sorted

testHeap' :: Ord a => BinomialHeap a -> [a] -> Bool
testHeap' h xs = isCorrect h && toSortedList h == xs


isCorrect :: Ord a => BinomialHeap a -> Bool
isCorrect (BH ts) = isStrictlyInc (map rank ts) &&
                    all isTreeCorrect ts
  where isStrictlyInc xs = and $ zipWith (<) xs (drop 1 xs)
                      
isTreeCorrect :: Ord a => Tree a -> Bool
isTreeCorrect t@(Node r x ts) = (map rank ts) == [r-1,r-2..0] &&
                                minimum (x : (map root ts)) == x &&
                                all isTreeCorrect ts
