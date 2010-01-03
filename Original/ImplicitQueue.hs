-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module ImplicitQueue (module Queue, ImplicitQueue) where
  import Prelude hiding (head,tail)
  import Queue

  data Digit a = Zero | One a | Two a a
  data ImplicitQueue a = 
         Shallow (Digit a) 
       | Deep (Digit a) (ImplicitQueue (a,a)) (Digit a) 

  instance Queue ImplicitQueue where
    empty = Shallow Zero
    isEmpty (Shallow Zero) = True
    isEmpty _ = False

    snoc (Shallow Zero) y = Shallow (One y)
    snoc (Shallow (One x)) y = Deep (Two x y) empty Zero
    snoc (Deep f m Zero) y = Deep f m (One y)
    snoc (Deep f m (One x)) y = Deep f (snoc m (x,y)) Zero

    head (Shallow Zero) = error "ImplicitQueue.head: empty queue"
    head (Shallow (One x)) = x
    head (Deep (One x) m r) = x
    head (Deep (Two x y) m r) = x

    tail (Shallow Zero) = error "ImplicitQueue.tail: empty queue"
    tail (Shallow (One x)) = empty
    tail (Deep (Two x y) m r) = Deep (One y) m r
    tail (Deep (One x) m r) =
        if isEmpty m then Shallow r else Deep (Two y z) (tail m) r
      where (y,z) = head m
