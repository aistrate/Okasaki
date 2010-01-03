import Test.HUnit
import Test.QuickCheck
import Data.List (sort)
import Text.Printf (printf)
import TestHelper
--import Ex03_04
import Ex03_04_c


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


isCorrect, isHeapOrdered, isSizeCorrect, isLeftist :: Ord a => LeftistHeap a -> Bool
isCorrect h = isHeapOrdered h && isSizeCorrect h && isLeftist h

isHeapOrdered E = True
isHeapOrdered h@(T _ x a b) = x == minimum (toList h) &&
                              isHeapOrdered a && isHeapOrdered b

isSizeCorrect E = True
isSizeCorrect h@(T s _ a b) = s == calcSize h &&
                              isSizeCorrect a && isSizeCorrect b
  where calcSize E = 0
        calcSize (T _ _ a b) = 1 + calcSize a + calcSize b

isLeftist E = True
isLeftist (T _ _ a b) = size a >= size b &&
                        isLeftist a && isLeftist b


toList :: Ord a => LeftistHeap a -> [a]
toList E = []
toList (T _ x a b) = x : (toList a ++ toList b)
