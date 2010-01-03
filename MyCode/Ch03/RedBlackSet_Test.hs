import Test.HUnit
import Test.QuickCheck
import Data.List (sort, nub, group)
import Text.Printf (printf)
import TestHelper
import RedBlackSet


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "fromList/sorted input" ~:
  [
    testSet "" ~? "empty list",
    testSet ['a'..'z'] ~? "ascending",
    testSet ['z','y'..'a'] ~? "descending",
    testSet (['a'..'z'] ++ ['z','y'..'a']) ~? "combined asc/desc",
    testSet (['z','y'..'a'] ++ ['a'..'z']) ~? "combined desc/asc",
    testSet (replicate 100 'a') ~? "constant"
  ] ]

qcheckTests = [ 
  ("fromList/properties", qcheck (testSet::[Int] -> Bool)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 500 }


testSet xs = let expected = nub $ sort xs
             in testSet' (fromList xs) expected

testSet' :: Ord a => RedBlackSet a -> [a] -> Bool
testSet' t xs = isCorrect t && toSortedList t == xs


isCorrect :: Ord a => RedBlackSet a -> Bool
isCorrect t = invariant1 t && invariant2 t

-- "No red node has a red child"
invariant1 E = True
invariant1 (T c a _ b) = not (c == R && (color a == R || color b == R)) &&
                         invariant1 a && invariant1 b

-- "Every path from the root to an empty node
--  contains the same number of black nodes"
invariant2 t = let blacksPerPath = map (length . filter (== B) . map fst) $
                                   allPaths t
               in length (group $ sort blacksPerPath) <= 1

allPaths E = []
allPaths (T c E x E) = [[(c, x)]]
allPaths (T c a x b) = map ((c, x) :) (allPaths a ++ allPaths b)
