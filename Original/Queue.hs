-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module Queue (Queue(..)) where
  import Prelude hiding (head,tail)

  class Queue q where
    empty   :: q a
    isEmpty :: q a -> Bool

    snoc    :: q a -> a -> q a
    head    :: q a -> a
    tail    :: q a -> q a
