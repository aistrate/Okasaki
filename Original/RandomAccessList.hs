-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module RandomAccessList (RandomAccessList(..)) where
  import Prelude hiding (head,tail,lookup)

  class RandomAccessList r where
    empty   :: r a
    isEmpty :: r a -> Bool

    cons    :: a -> r a -> r a
    head    :: r a -> a
    tail    :: r a -> r a

    lookup  :: Int -> r a -> a
    update  :: Int -> a -> r a -> r a
