{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module SplayHeap (module Heap, SplayHeap, toList, printHeap) where

import Heap


data SplayHeap a = E |
                   T (SplayHeap a) a (SplayHeap a)
                 deriving (Eq, Show)


partition pivot E = (E, E)
partition pivot t@(T a x b) =
  if x <= pivot then
    case b of
      E -> (t, E)
      T b1 y b2 ->
        if y <= pivot then
          let (small, big) = partition pivot b2
          in (T (T a x b1) y small, big)  -- corrected typo in book
        else
          let (small, big) = partition pivot b1
          in (T a x small, T big y b2)
  else
    case a of
      E -> (E, t)
      T a1 y a2 ->
        if y <= pivot then
          let (small, big) = partition pivot a2
          in (T a1 y small, T big x b)
        else
          let (small, big) = partition pivot a1
          in (small, T big y (T a2 x b))


instance Ord a => Heap SplayHeap a where
  empty = E
  
  isEmpty E = True
  isEmpty _ = False
  
  insert x t = T a x b
    where (a, b) = partition x t
  
  merge E t = t
  merge (T a x b) t = T (merge ta a) x (merge tb b)
    where (ta, tb) = partition x t
  
  findMin E = error "SplayHeap.findMin: empty heap"
  findMin (T E x b) = x
  findMin (T a x b) = findMin a
  
  deleteMin E = error "SplayHeap.deleteMin: empty heap"
  deleteMin (T E x b) = b
  deleteMin (T (T E x b) y c) = T b y c
  deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)


-- Helpers
toList :: SplayHeap a -> [a]
toList E = []
toList (T a x b) = toList a ++ x : toList b

printHeap :: Ord a => (a -> String) -> SplayHeap a -> IO ()
printHeap showF = printHeap' 0
  where printHeap' _ E = return ()
        printHeap' n (T a x b) = do printHeap' (n + 1) b
                                    putStrLn $ (replicate (1*n) ' ') ++ showF x
                                    printHeap' (n + 1) a
