{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ex03_06 (module Heap, BinomialHeap(..), Tree(..), rank, root) where

import Heap


data Tree a = Node a [Tree a]
  
newtype BinomialHeap a = BH [(Int, Tree a)]


rank (r, Node x c) = r
root (r, Node x c) = x

link (r, n1@(Node x1 c1)) (_, n2@(Node x2 c2)) =
  if x1 <= x2 then (r+1, Node x1 (n2:c1))
  else (r+1, Node x2 (n1:c2))

insTree t [] = [t]
insTree t ts@(t':ts') =
  if rank t < rank t' then t:ts else insTree (link t t') ts'

mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2')
  | rank t1 < rank t2 = t1 : mrg ts1' ts2
  | rank t2 < rank t1 = t2 : mrg ts1 ts2'
  | otherwise         = insTree (link t1 t2) (mrg ts1' ts2')

removeMinTree [] = error "BinomialHeap: empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t, ts) else (t', t : ts')
  where (t', ts') = removeMinTree ts


instance Ord a => Heap BinomialHeap a where
  empty = BH []
  isEmpty (BH ts) = null ts

  insert x (BH ts) = BH (insTree (0, Node x []) ts)
  merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

  findMin (BH ts) = root t
    where (t, _) = removeMinTree ts

  deleteMin (BH ts) = BH (mrg (zip [0..r-1] (reverse ts1)) ts2)
    where ((r, Node x ts1), ts2) = removeMinTree ts


-- Helpers
printHeap :: Ord a => (a -> String) -> BinomialHeap a -> IO ()
printHeap showF (BH ps) = mapM_ (\(r, t) -> do putStrLn ("\n" ++ show r ++ 
                                                         ": ----------------")
                                               printTree showF t) ps

printTree showF = printTree' 0
  where printTree' n (Node x ts) = do putStrLn $ (replicate (1*n) ' ') ++ showF x
                                      mapM_ (printTree' (n + 1)) ts

printCharHeap = printHeap (:[])
