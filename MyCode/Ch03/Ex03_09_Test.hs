2import Test.HUnit
import Test.QuickCheck
import Data.List (sort, nub, group)
import Text.Printf (printf)
import TestHelper
import Ex03_09


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  "sorted input" ~:
  [
    testFromOrdList "" ~? "empty",
    testFromOrdList ['a'..'z'] ~? "small",
    testFromOrdList [1..500] ~? "medium",
    testFromOrdList [1..20000] ~? "large"
  ],
  
  "increasing input length" ~:
    map (\n -> testFromOrdList [1..n] ~? "length " ++ show n) [0..300]
  ]

--  8889: 0.49
-- 10000: 0.60s
-- 13333: 1.00s
-- 20000: 2.01s
-- 30000: 4.45s

qcheckTests = [ 
  ("invariants", qcheck (testFromOrdList::[Int] -> Bool)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 500 }


testFromOrdList xs = let ordered = nub $ sort xs
                     in testSet (fromOrdList ordered) ordered


testSet :: Ord a => RedBlackSet a -> [a] -> Bool
testSet t xs = isCorrect t && toSortedList t == xs


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
