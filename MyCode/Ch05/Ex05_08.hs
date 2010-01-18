{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ex05_08 (module Heap, BinTree, toBinary) where

import Heap
import PairingHeap


data BinTree a = E'
               | T' a (BinTree a) (BinTree a)
               deriving (Eq, Show)


toBinary :: PairingHeap a -> BinTree a
toBinary E = E'
toBinary h = toBinary' [h]
  where toBinary' [] = E'
        toBinary' ((T x ys):hs) = T' x (toBinary' ys) (toBinary' hs)


mergePairs E' = E'
mergePairs t@(T' x _ E') = t
mergePairs (T' x a1 (T' y a2 b2)) = merge (merge (T' x a1 E') (T' y a2 E')) (mergePairs b2)


instance Ord a => Heap BinTree a where
  empty = E'
  
  isEmpty E' = True
  isEmpty _  = False
  
  insert x t = merge (T' x E' E') t
  
  merge t E' = t
  merge E' t = t
  merge t1@(T' x a1 E') t2@(T' y a2 E') =
    if x < y then T' x (T' y a2 a1) E' else T' y (T' x a1 a2) E'
  
  findMin E' = error "BinTree.findMin: empty heap"
  findMin (T' x t E') = x
  
  deleteMin E' = error "BinTree.deleteMin: empty heap"
  deleteMin (T' x t E') = mergePairs t
