import Test.HUnit
import qualified Test.QuickCheck as QC
import Data.List (sort)
import Text.Printf (printf)
import TestHelper
import qualified PairingHeap as PH
import Ex05_08


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "fromList" ~:
  [
    testHeap "" ~? "empty",
    testHeap ['a'..'z'] ~? "ascending",
    testHeap ['z','y'..'a'] ~? "descending",
    testHeap (['a'..'z'] ++ ['z','y'..'a']) ~? "combined asc/desc",
    testHeap (['z','y'..'a'] ++ ['a'..'z']) ~? "combined desc/asc",
    testHeap (['a'..'z'] ++ replicate 20 'm') ~? "asc/const"
  ], 
  
  "merge" ~:
  [
    testMerge ("", ['a'..'z']) ~? "empty/asc",
    testMerge (['a'..'z'], "") ~? "asc/empty",
    testMerge (['a'..'z'], ['z','y'..'a']) ~? "asc/desc",
    testMerge (['z','y'..'a'], ['a'..'z']) ~? "desc/asc"
  ] ]


qcheckTests = [ 
  ("fromList", qcheck (testHeap :: [Int] -> Bool)), 
  ("merge", qcheck (testMerge :: ([Int], [Int]) -> Bool)),
  
  ("toBinary/fromList", qcheck (testToBinaryFromList :: [Int] -> Bool)),
  ("toBinary/insert", qcheck (testToBinaryInsert :: (Int, [Int]) -> Bool)), 
  ("toBinary/merge", qcheck (testToBinaryMerge :: ([Int], [Int]) -> Bool)), 
  ("toBinary/findMin", qcheck (testToBinaryFindMin :: (Int, [Int]) -> Bool)),
  ("toBinary/deleteMin", qcheck (testToBinaryDeleteMin :: (Int, [Int]) -> Bool)) ]


qcheck :: QC.Testable a => a -> IO ()
qcheck = --QC.quickCheck
         --QC.verboseCheck
         QC.check $ QC.defaultConfig { QC.configMaxTest = 500 }


testHeap xs = testHeap' (fromList xs) (sort xs)

testHeap' :: Ord a => BinTree a -> [a] -> Bool
testHeap' h xs = toSortedList h == xs

testMerge (xs, ys) = testHeap' (merge (fromList xs) (fromList ys))
                               (sort (xs ++ ys))


-- toBinary tests
testToBinaryFromList xs = toBinary (PH.fromList xs) == fromList xs

testToBinaryInsert (x, ys) = let h = PH.fromList ys
                             in insert x (toBinary h) == toBinary (PH.insert x h)

testToBinaryMerge (xs, ys) = let h1 = PH.fromList xs
                                 h2 = PH.fromList ys
                             in merge (toBinary h1) (toBinary h2) == toBinary (PH.merge h1 h2)

testToBinaryFindMin (x, xs) = let h = PH.fromList (x:xs)
                              in findMin (toBinary h) == PH.findMin h

testToBinaryDeleteMin (x, xs) = let h = PH.fromList (x:xs)
                                in deleteMin (toBinary h) == toBinary (PH.deleteMin h)
