{-# LANGUAGE MultiParamTypeClasses #-}

module Heap where
  

class Ord a => Heap h a where
  empty     :: h a
  isEmpty   :: h a -> Bool
  
  insert    :: a -> h a -> h a
  merge     :: h a -> h a -> h a
  
  findMin   :: h a -> a
  deleteMin :: h a -> h a


toSortedList :: (Ord a, Heap h a) => h a -> [a]
toSortedList h | isEmpty h = []
               | otherwise = let m = findMin h
                             in m : toSortedList (deleteMin h)


fromList :: (Ord a, Heap h a) => [a] -> h a
fromList = fromList_fold

fromList_fold :: (Ord a, Heap h a) => [a] -> h a
fromList_fold = foldl (flip insert) empty


-- Ex 3.3
fromList_pairs :: (Ord a, Heap h a) => [a] -> h a
fromList_pairs [] = empty
fromList_pairs xs = head . mergeAll $ map (flip insert empty) xs
  where mergeAll [h] = [h]
        mergeAll hs  = mergeAll $ mergePairs hs
        mergePairs (h1:h2:hs) = merge h1 h2 : mergePairs hs
        mergePairs hs         = hs
