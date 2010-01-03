import Test.HUnit
import Test.QuickCheck
import Data.List (sort)
import Text.Printf (printf)
import TestHelper
import Ex03_06


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
                    all (\(RT r t) -> isTreeCorrect r t) ts
  where isStrictlyInc xs = and $ zipWith (<) xs (drop 1 xs)

isTreeCorrect :: Ord a => Int -> Tree a -> Bool
isTreeCorrect r t@(Node x ts) = let xs = toList t
                                in x == minimum xs &&
                                   length xs == 2^r &&
                                   length ts == r &&
                                   and (zipWith isTreeCorrect [r-1,r-2..0] ts)

toList (Node x ts) = x : concatMap toList ts
