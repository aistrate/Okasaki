(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 5                              *)
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


structure BatchedQueue : QUEUE =
struct
  type 'a Queue = 'a list * 'a list

  val empty = ([], [])
  fun isEmpty (f, r) = null f

  fun checkf ([], r) = (rev r, [])
    | checkf q = q

  fun snoc ((f, r), x) = checkf (f, x :: r)

  fun head ([], _) = raise Empty
    | head (x :: f, r) = x
  fun tail ([], _) = raise Empty
    | tail (x :: f, r) = checkf (f, r)
end

signature DEQUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  (* insert, inspect, and remove the front element *)
  val cons    : 'a * 'a Queue -> 'a Queue
  val head    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val tail    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)

  (* insert, inspect, and remove the rear element *)
  val snoc    : 'a Queue * 'a -> 'a Queue
  val last    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val init    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)
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


functor SplayHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of Heap * Elem.T * Heap

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun partition (pivot, E) = (E, E)
    | partition (pivot, t as T (a, x, b)) =
        if Elem.leq (x, pivot) then
          case b of
            E => (t, E)
	  | T (b1, y, b2) => 
              if Elem.leq (y, pivot) then
                let val (small, big) = partition (pivot, b2)
                in (T (T (a, x, b1), y, small), big) end
              else
                let val (small, big) = partition (pivot, b1)
                in (T (a, x, small), T (big, y, b2)) end
	else
          case a of
            E => (E, t)
	  | T (a1, y, a2) => 
              if Elem.leq (y, pivot) then
                let val (small, big) = partition (pivot, a2)
		in (T (a1, y, small), T (big, x, b)) end
	      else
                let val (small, big) = partition (pivot, a1)
		in (small, T (big, y, T (a2, x, b))) end

  fun insert (x, t) = let val (a, b) = partition (x, t) in T (a, x, b) end
  fun merge (E, t) = t
    | merge (T (a, x, b), t) =
        let val (ta, tb) = partition (x, t)
        in T (merge (ta, a), x, merge (tb, b)) end

  fun findMin E = raise Empty
    | findMin (T (E, x, b)) = x
    | findMin (T (a, x, b)) = findMin a
  fun deleteMin E = raise Empty
    | deleteMin (T (E, x, b)) = b
    | deleteMin (T (T (E, x, b), y, c)) = T (b, y, c)
    | deleteMin (T (T (a, x, b), y, c)) = T (deleteMin a, x, T (b, y, c))
end


functor PairingHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of Elem.T * Heap list

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (x, hs1), h2 as T (y, hs2)) =
        if Elem.leq (x, y) then T (x, h2 :: hs1) else T (y, h1 :: hs2)
  fun insert (x, h) = merge (T (x, []), h)

  fun mergePairs [] = E
    | mergePairs [h] = h
    | mergePairs (h1 :: h2 :: hs) = merge (merge (h1, h2), mergePairs hs)

  fun findMin E = raise Empty
    | findMin (T (x, hs)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T (x, hs)) = mergePairs hs
end
