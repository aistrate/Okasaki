{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ex03_07 (module Heap, ExplicitMin(..)) where

import Heap


data (Ord a, Heap h a) => ExplicitMin h a = E
                                          | NE a (h a)
                                          deriving (Eq, Show)


instance (Ord a, Heap h a) => Heap (ExplicitMin h) a where
  empty = E
  
  isEmpty E = True
  isEmpty _ = False
  
  insert x E        = NE x (insert x empty)
  insert x (NE m h) = NE (min x m) (insert x h)
  
  merge E  E  = E
  merge ne E  = ne
  merge E  ne = ne
  merge (NE m1 h1) (NE m2 h2) = NE (min m1 m2) (merge h1 h2)
  
  findMin E = error "ExplicitMin.findMin: empty heap"
  findMin (NE m h) = m
  
  deleteMin E = error "ExplicitMin.deleteMin: empty heap"
  deleteMin (NE m h) = let h' = deleteMin h
                       in if isEmpty h' then E
                          else NE (findMin h') h'
