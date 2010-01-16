{-# LANGUAGE MultiParamTypeClasses #-}

module Heap where
  

class Ord a => Heap h a where
  empty     :: h a
  isEmpty   :: h a -> Bool
  
  insert    :: a -> h a -> h a
  merge     :: h a -> h a -> h a
  
  findMin   :: h a -> a
  deleteMin :: h a -> h a


fromList :: (Ord a, Heap h a) => [a] -> h a
fromList = foldl (flip insert) empty


toSortedList :: (Ord a, Heap h a) => h a -> [a]
toSortedList h | isEmpty h = []
               | otherwise = let m = findMin h
                             in m : toSortedList (deleteMin h)
