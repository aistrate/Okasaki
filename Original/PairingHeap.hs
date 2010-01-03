-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module PairingHeap (module Heap, PairingHeap) where
  import Heap

  data PairingHeap a = E | T a [PairingHeap a]

  mergePairs [] = E
  mergePairs [h] = h
  mergePairs (h1 : h2 : hs) = merge (merge h1 h2) (mergePairs hs)

  instance Heap PairingHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T x []) h

    merge h E = h
    merge E h = h
    merge h1@(T x hs1) h2@(T y hs2) =
        if x < y then T x (h2:hs1) else T y (h1:hs2)

    findMin E = error "PairingHeap.findMin: empty heap"
    findMin (T x hs) = x

    deleteMin E = error "PairingHeap.deleteMin: empty heap"
    deleteMin (T x hs) = mergePairs hs
