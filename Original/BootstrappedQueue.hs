-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module BootstrappedQueue (module Queue, BootstrappedQueue) where
  import Prelude hiding (head,tail)
  import Queue

  data BootstrappedQueue a = 
      E | Q Int [a] (BootstrappedQueue [a]) Int [a]

  checkQ,checkF :: Int -> [a] -> (BootstrappedQueue [a]) -> Int -> [a] 
                       -> BootstrappedQueue a

  checkQ lenfm f m lenr r =
      if lenr <= lenfm then checkF lenfm f m lenr r
      else checkF (lenfm+lenr) f (snoc m (reverse r)) 0 []

  checkF lenfm [] E lenr f = E
  checkF lenfm [] m lenr r = Q lenfm (head m) (tail m) lenr r
  checkF lenfm f m lenr r = Q lenfm f m lenr r

  instance Queue BootstrappedQueue where
    empty = E
    isEmpty E = True
    isEmpty _ = False

    snoc E x = Q 1 [x] E 0 []
    snoc (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr+1) (x:r)

   
    head E = error "BootstrappedQueue.head: empty queue"
    head (Q lenfm (x:f') m lenr r) = x

    tail E = error "BootstrappedQueue.tail: empty queue"
    tail (Q lenfm (x:f') m lenr r) = checkQ (lenfm-1) f' m lenr r

