-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module Set (Set(..)) where
  class Set s a where
    emptySet  :: s a
    insertSet :: a -> s a -> s a
    memberSet :: a -> s a -> Bool
