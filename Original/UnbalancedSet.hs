-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module UnbalancedSet (module Set,UnbalancedSet) where
  import Set

  data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)

  instance Ord a => Set UnbalancedSet a where
    emptySet = E

    memberSet x E = False
    memberSet x (T a y b) =
      if x < y then memberSet x a
      else if x > y then memberSet x b
      else True

    insertSet x E = T E x E
    insertSet x s@(T a y b) =
      if x < y then T (insertSet x a) y b
      else if x > y then T a y (insertSet x b)
      else s

