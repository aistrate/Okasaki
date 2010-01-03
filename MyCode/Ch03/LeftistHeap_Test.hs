import Test.HUnit
import Test.QuickCheck
import Data.List (sort)
import Text.Printf (printf)
import TestHelper
--import LeftistHeap
import Ex03_02


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "insert/findMin" ~:
  [
    findMin (insert 'e' E) ~?= 'e',
    findMin (insert 'f' (insert 'e' E)) ~?= 'e',
    findMin (insert 'e' (insert 'f' E)) ~?= 'e'
  ],
  "fromList/sorted input" ~:
  [
    testHeap "" ~? "empty list",
    testHeap ['a'..'z'] ~? "ascending",
    testHeap ['z','y'..'a'] ~? "descending",
    testHeap (['a'..'z'] ++ ['z','y'..'a']) ~? "combined asc/desc",
    testHeap (['z','y'..'a'] ++ ['a'..'z']) ~? "combined desc/asc"
  ] ]

qcheckTests = [ 
  ("fromList/properties", qcheck (testHeap::[Int] -> Bool)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 500 }


testHeap xs = let sorted = sort xs
              in testHeap' (fromList_fold xs) sorted &&
                 testHeap' (fromList_pairs xs) sorted

testHeap' :: Ord a => LeftistHeap a -> [a] -> Bool
testHeap' h xs = isCorrect h && toSortedList h == xs


isCorrect, isHeapOrdered, isRankCorrect, isLeftist :: Ord a => LeftistHeap a -> Bool
isCorrect h = isHeapOrdered h && isRankCorrect h && isLeftist h

isHeapOrdered E = True
isHeapOrdered h@(T _ x a b) = x == minimum (toList h) &&
                              isHeapOrdered a && isHeapOrdered b

isRankCorrect E = True
isRankCorrect h@(T r _ a b) = r == rightSpineLength h &&
                              isRankCorrect a && isRankCorrect b
  where rightSpineLength E = 0
        rightSpineLength (T _ _ _ s) = 1 + rightSpineLength s

isLeftist E = True
isLeftist (T _ _ a b) = rank a >= rank b &&
                        isLeftist a && isLeftist b


toList :: Ord a => LeftistHeap a -> [a]
toList E = []
toList (T _ x a b) = x : (toList a ++ toList b)
