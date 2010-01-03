{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ex03_02 (module Heap, LeftistHeap(..), rank) where

import Heap


data LeftistHeap a = E 
                   | T Int a (LeftistHeap a) (LeftistHeap a)
                   deriving (Eq, Show)

rank E = 0
rank (T r _ _ _) = r

makeT x a b = if rank a >= rank b then T (rank b + 1) x a b
              else T (rank a + 1) x b a

instance Ord a => Heap LeftistHeap a where
  empty = E
  
  isEmpty E = True
  isEmpty _ = False
  
  insert x E = T 1 x E E
  insert x h@(T _ y a b) =
    -- if x <= y then makeT x E (merge E h)
    -- else makeT y a (merge (T 1 x E E) b)
    if x <= y then makeT x E h
    else makeT y a (insert x b)
  
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
        printHeap' n (T r x a b) = do printHeap' (n + 1) b
                                      putStrLn $ (replicate (1*n) ' ') ++ showF x
                                      printHeap' (n + 1) a

printCharHeap = printHeap (:[])
