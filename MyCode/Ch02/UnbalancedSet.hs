{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             UndecidableInstances, TypeSynonymInstances #-}

module UnbalancedSet where


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
  
  insert x E = T E x E
  insert x s@(T a y b) =
    if x `lt` y then T (insert x a) y b
    else if y `lt` x then T a y (insert x b)
         else s


{-
signature SET =
sig
  type Elem
  type Set

  val empty  : Set
  val insert : Elem * Set -> Set
  val member : Elem * Set -> bool
end

signature ORDERED =
  (* a totally ordered type and its comparison functions *)
sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

functor UnbalancedSet (Element : ORDERED) : SET =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (a, y, b)) =
        if Element.lt (x, y) then member (x, a)
        else if Element.lt (y, x) then member (x, b)
        else true

  fun insert (x, E) = T (E, x, E)
    | insert (x, s as T (a, y, b)) =
        if Element.lt (x, y) then T (insert (x, a), y, b)
        else if Element.lt (y, x) then T (a, y, insert (x, b))
        else s
end
-}
