(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 6                              *)
(***********************************************************************)

signature QUEUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val tail    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)
end

signature ORDERED =
  (* a totally ordered type and its comparison functions *)
sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature HEAP =
sig
  structure Elem : ORDERED

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap

  val findMin   : Heap -> Elem.T   (* raises Empty if heap is empty *)
  val deleteMin : Heap -> Heap     (* raises Empty if heap is empty *)
end

open Stream

structure BankersQueue : QUEUE =
struct
  type 'a Queue = int * 'a Stream * int * 'a Stream

  val empty = (0, $Nil, 0, $Nil)
  fun isEmpty (lenf, _, _, _) = (lenf = 0)

  fun check (q as (lenf, f, lenr, r)) =
        if lenr <= lenf then q else (lenf+lenr, f ++ reverse r, 0, $Nil)

  fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr+1, $(Cons (x, r)))

  fun head (lenf, $Nil, lenr, r) = raise Empty
    | head (lenf, $(Cons (x, f')), lenr, r) = x
  fun tail (lenf, $Nil, lenr, r) = raise Empty
    | tail (lenf, $(Cons (x, f')), lenr, r) = check (lenf-1, f', lenr, r)
end

functor LazyBinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of int * Elem.T * Tree list
  type Heap = Tree list susp

  val empty = $[]
  fun isEmpty ($ts) = null ts

  fun rank (Node (r, x, c)) = r
  fun root (Node (r, x, c)) = x
  fun link (t1 as Node (r, x1, c1), t2 as Node (_, x2, c2)) =
        if Elem.leq (x1, x2) then Node (r+1, x1, t2 :: c1)
        else Node (r+1, x2, t1 :: c2)
  fun insTree (t, []) = [t]
    | insTree (t, ts as t' :: ts') =
        if rank t < rank t' then t :: ts else insTree (link (t, t'), ts')

  fun mrg (ts1, []) = ts1
    | mrg ([], ts2) = ts2
    | mrg (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
        if rank t1 < rank t2 then t1 :: mrg (ts1', ts2)
        else if rank t2 < rank t1 then t2 :: mrg (ts1, ts2')
        else insTree (link (t1, t2), mrg (ts1', ts2'))

  (* fun lazy *)
  fun insert (x, $ts) = $(insTree (Node (0, x, []), ts))

  (* fun lazy *)
  fun merge ($ts1, $ts2) = $(mrg (ts1, ts2))

  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
        let val (t', ts') = removeMinTree ts
	in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end

  fun findMin ($ts) = let val (t, _) = removeMinTree ts in root t end

  (* fun lazy *)
  fun deleteMin ($ts) =
        let val (Node (_, x, ts1), ts2) = removeMinTree ts
	in $(mrg (rev ts1, ts2)) end
end


structure PhysicistsQueue : QUEUE =
struct
  type 'a Queue = 'a list * int * 'a list susp * int * 'a list

  val empty = ([], 0, $[], 0, [])
  fun isEmpty (_, lenf, _, _, _) = (lenf = 0)

  fun checkw ([], lenf, f, lenr, r) = (force f, lenf, f, lenr, r)
    | checkw q = q
  fun check (q as (w, lenf, f, lenr, r)) =
        if lenr <= lenf then checkw q
        else let val f' = force f
             in checkw (f', lenf+lenr, $(f' @ rev r), 0, []) end

  fun snoc ((w, lenf, f, lenr, r), x) = check (w, lenf, f, lenr+1, x :: r)

  fun head ([], lenf, f, lenr, r) = raise Empty
    | head (x :: w, lenf, f, lenr, r) = x
  fun tail ([], lenf, f, lenr, r) = raise Empty
    | tail (x :: w, lenf, f, lenr, r) = 
        check (w, lenf-1,$(tl (force f)), lenr, r)
end


signature SORTABLE =
sig
  structure Elem : ORDERED

  type Sortable

  val empty : Sortable
  val add   : Elem.T * Sortable -> Sortable
  val sort  : Sortable -> Elem.T list
end


functor BottomUpMergeSort (Element : ORDERED) : SORTABLE =
struct
  structure Elem = Element

  type Sortable = int * Elem.T list list susp

  fun mrg ([], ys) = ys
    | mrg (xs, []) = xs
    | mrg (xs as x :: xs', ys as y :: ys') =
        if Elem.leq (x, y) then x :: mrg (xs', ys) else y :: mrg (xs, ys')

  val empty = (0, $[])
  fun add (x, (size, segs)) =
        let fun addSeg (seg, segs, size) =
                  if size mod 2 = 0 then seg :: segs
		  else addSeg (mrg (seg, hd segs), tl segs, size div 2)
	in (size+1, $(addSeg ([x], force segs, size))) end
  fun sort (size, segs) =
        let fun mrgAll (xs, []) = xs
	      | mrgAll (xs, seg :: segs) = mrgAll (mrg (xs, seg), segs)
	in mrgAll ([], force segs) end
end

functor LazyPairingHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of Elem.T * Heap * Heap susp

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (a, E) = a
    | merge (E, b) = b
    | merge (a as T (x, _, _), b as T (y, _, _)) =
        if Elem.leq (x, y) then link (a, b) else link (b, a)
  and link (T (x, E, m), a) = T (x, a, m)
    | link (T (x, b, m), a) = T (x, E, $(merge (merge (a, b), force m)))

  fun insert (x, a) = merge (T (x, E, $E), a)

  fun findMin E = raise Empty
    | findMin (T (x, a, m)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T (x, a, $b)) = merge (a, b)
end
