{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             UndecidableInstances, TypeSynonymInstances #-}

module Ex02_04 where


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
    else if y `lt` x then member x b
         else True
  
  -- not exactly what was asked, but Haskell exceptions cannot be used here,
  -- as they may only be caught in the IO monad
  insert x s = let (t, found) = insert' x s
               in if found then s else t
     where insert' :: Ordered a => a -> UnbalancedSet a -> (UnbalancedSet a, Bool)
           insert' x E = (T E x E, False)
           insert' x (T a y b) =
             if x `lt` y then let (t, found) = insert' x a
                              in (if found then E else T t y b, found)
             else let (t, found) = insert' x b
                  in (if found then E else T a y t, found || x `eq` y)

             -- else if y `lt` x then let (t, found) = insert' x b
             --                       in (if found then E else T a y t, found)
             --      else (E, True)
