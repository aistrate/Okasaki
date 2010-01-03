{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             UndecidableInstances, TypeSynonymInstances #-}

module Ex02_02 where


class Set s a where
  empty :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool

class Ordered t where
  eq :: t -> t -> Bool
  lt :: t -> t -> Bool
  leq :: t -> t -> Bool

instance Ord t => Ordered t where
  eq = (==)
  lt = (<)
  leq = (<=)

data Tree e = E
            | T (Tree e) e (Tree e)
              deriving (Eq, Show)

instance Functor Tree where
  fmap f E = E
  fmap f (T l e r) = T (fmap f l) (f e) (fmap f r)

type UnbalancedSet = Tree


instance Ordered e => Set UnbalancedSet e where
  empty = E
  
  member x E = False
  member x (T a y b) =
    if x `lt` y then member x a
    else if member x b then True
         else x `eq` y
  
  insert x E = T E x E
  insert x s@(T a y b) =
    if x `lt` y then T (insert x a) y b
    else if y `lt` x then T a y (insert x b)
         else s
