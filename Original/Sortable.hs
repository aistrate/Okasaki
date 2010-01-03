-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module Sortable (Sortable(..)) where
  class Sortable s where
    empty :: Ord a => s a
    add   :: Ord a => a -> s a -> s a
    sort  :: Ord a => s a -> [a]
