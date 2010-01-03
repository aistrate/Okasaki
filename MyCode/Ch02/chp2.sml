(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 2                              *)
(***********************************************************************)

signature STACK =
sig
  type 'a Stack

  val empty   : 'a Stack
  val isEmpty : 'a Stack -> bool

  val cons    : 'a * 'a Stack -> 'a Stack
  val head    : 'a Stack -> 'a        (* raises Empty if stack is empty *)
  val tail    : 'a Stack -> 'a Stack  (* raises Empty if stack is empty *)
end

structure List : STACK =
struct
  type 'a Stack = 'a list

  val empty = []
  fun isEmpty s = null s

  fun cons (x, s) = x :: s
  fun head s = hd s
  fun tail s = tl s
end

structure CustomStack : STACK =
struct
  datatype 'a Stack = Nil | Cons of 'a * 'a Stack

  val empty = Nil
  fun isEmpty Nil = true
    | isEmpty _ = false

  fun cons (x, s) = Cons (x, s)
  fun head Nil = raise Empty
    | head (Cons (x, s)) = x
  fun tail Nil = raise Empty
    | tail (Cons (x, s)) = s
end

local 
  open List
in
  fun xs ++ ys = if isEmpty xs then ys else cons (head xs, tail xs ++ ys)
end

fun [] ++ ys = ys
  | (x :: xs) ++ ys = x :: (xs ++ ys)

fun update ([], i, y) = raise Subscript
  | update (x :: xs, 0, y) = y :: xs
  | update (x :: xs, i, y) = x :: update (xs, i-1, y)

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

signature FINITE_MAP =
sig
  type Key
  type 'a Map

  val empty  : 'a Map
  val bind   : Key * 'a * 'a Map -> 'a Map
  val lookup : Key * 'a Map -> 'a    (* raise NotFound if key is not found *)
end
