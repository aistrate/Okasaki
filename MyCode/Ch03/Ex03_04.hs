{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ex03_04 (module Heap, LeftistHeap(..), size) where

import Heap


data LeftistHeap a = E 
                   | T Int a (LeftistHeap a) (LeftistHeap a)
                   deriving (Eq, Show)

size E = 0
size (T s _ _ _) = s

makeT x a b = let s = size a + size b + 1
              in if size a >= size b then T s x a b
                 else T s x b a

-- Weight-biased leftist heap
instance Ord a => Heap LeftistHeap a where
  empty = E
  
  isEmpty E = True
  isEmpty _ = False
  
  insert x h = merge (T 1 x E E) h
  
  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
    if x <= y then makeT x a1 (merge b1 h2)
    else makeT y a2 (merge h1 b2)
  
  findMin E = error "LeftistHeap.findMin: empty heap"
  findMin (T _ x a b) = x
  
  deleteMin E = error "LeftistHeap.deleteMin: empty heap"
  deleteMin (T _ x a b) = merge a b


-- Helpers
printHeap :: Ord a => (a -> String) -> LeftistHeap a -> IO ()
printHeap showF = printHeap' 0
  where printHeap' _ E = return ()
        printHeap' n (T _ x a b) = do printHeap' (n + 1) b
                                      putStrLn $ (replicate (1*n) ' ') ++ showF x
                                      printHeap' (n + 1) a

printCharHeap = printHeap (:[])
