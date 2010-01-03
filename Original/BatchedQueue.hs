-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module BatchedQueue (module Queue, BatchedQueue) where
  import Prelude hiding (head,tail)
  import Queue

  data BatchedQueue a = BQ [a] [a]

  check [] r = BQ (reverse r) []
  check f r = BQ f r

  instance Queue BatchedQueue where
    empty = BQ [] []
    isEmpty (BQ f r) = null f

    snoc (BQ f r) x = check f (x:r)

    head (BQ [] _) = error "BatchedQueue.head: empty queue"
    head (BQ (x:f) r) = x

    tail (BQ [] _) = error "BatchedQueue.tail: empty queue"
    tail (BQ (x:f) r) = check f r
