{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ex03_04_c (module Heap, LeftistHeap(..), size) where

import Heap


data LeftistHeap a = E 
                   | T Int a (LeftistHeap a) (LeftistHeap a)
                   deriving (Eq, Show)

size E = 0
size (T s _ _ _) = s

-- Weight-biased leftist heap
instance Ord a => Heap LeftistHeap a where
  empty = E
  
  isEmpty E = True
  isEmpty _ = False
  
  insert x h = merge (T 1 x E E) h
  
  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
    let (z, h', h'', h''') = if x <= y then (x, a1, b1, h2)
                             else (y, a2, h1, b2)
        s = 1 + size h' + size h'' + size h'''
    in if size h' >= size h'' + size h''' then T s z h' (merge h'' h''')
       else T s z (merge h'' h''') h'
    
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
