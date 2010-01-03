-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module RedBlackSet (module Set,RedBlackSet) where
  import Set

  data Color = R | B
  data RedBlackSet a = E | T Color (RedBlackSet a) a (RedBlackSet a)

  balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
  balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
  balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
  balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
  balance color a x b = T color a x b

  instance Ord a => Set RedBlackSet a where
    emptySet = E

    memberSet x E = False
    memberSet x (T _ a y b) =
      if x < y then memberSet x a
      else if x > y then memberSet x b
      else True

    insertSet x s = T B a y b
      where ins E = T R E x E
            ins s@(T color a y b) =
              if x < y then balance color (ins a) y b
              else if x > y then balance color a y (ins b)
              else s

            T _ a y b = ins s  -- guaranteed to be non-empty
