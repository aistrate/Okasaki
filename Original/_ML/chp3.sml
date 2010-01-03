(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 3                              *)
(***********************************************************************)

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

functor LeftistHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun rank E = 0
    | rank (T (r,_,_,_)) = r
  fun makeT (x, a, b) = if rank a >= rank b then T (rank b + 1, x, a, b)
                        else T (rank a + 1, x, b, a)

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
        if Elem.leq (x, y) then makeT (x, a1, merge (b1, h2))
        else makeT (y, a2, merge (h1, b2))

  fun insert (x, h) = merge (T (1, x, E, E), h)

  fun findMin E = raise Empty
    | findMin (T (_, x, a, b)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T (_, x, a, b)) = merge (a, b)
end

functor BinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of int * Elem.T * Tree list
  type Heap = Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun rank (Node (r, x, c)) = r
  fun root (Node (r, x, c)) = x
  fun link (t1 as Node (r, x1, c1), t2 as Node (_, x2, c2)) =
        if Elem.leq (x1, x2) then Node (r+1, x1, t2 :: c1)
        else Node (r+1, x2, t1 :: c2)
  fun insTree (t, []) = [t]
    | insTree (t, ts as t' :: ts') =
        if rank t < rank t' then t :: ts else insTree (link (t, t'), ts')

  fun insert (x, ts) = insTree (Node (0, x, []), ts)
  fun merge (ts1, []) = ts1
    | merge ([], ts2) = ts2
    | merge (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
        if rank t1 < rank t2 then t1 :: merge (ts1', ts2)
        else if rank t2 < rank t1 then t2 :: merge (ts1, ts2')
        else insTree (link (t1, t2), merge (ts1', ts2'))

  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
        let val (t', ts') = removeMinTree ts
	in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end

  fun findMin ts = let val (t, _) = removeMinTree ts in root t end
  fun deleteMin ts =
        let val (Node (_, x, ts1), ts2) = removeMinTree ts
        in merge (rev ts1, ts2) end
end

signature SET =
sig
  type Elem
  type Set

  val empty  : Set
  val insert : Elem * Set -> Set
  val member : Elem * Set -> bool
end

functor RedBlackSet (Element : ORDERED) : SET =
struct
  type Elem = Element.T

  datatype Color = R | B
  datatype Tree = E | T of Color * Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (_, a, y, b)) =
        if Element.lt (x, y) then member (x, a)
        else if Element.lt (y, x) then member (x, b)
        else true

  fun balance (B,T (R,T (R,a,x,b),y,c),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance (B,T (R,a,x,T (R,b,y,c)),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance (B,a,x,T (R,T (R,b,y,c),z,d)) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance (B,a,x,T (R,b,y,T (R,c,z,d))) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance body = T body

  fun insert (x, s) =
        let fun ins E = T (R, E, x, E)
	      | ins (s as T (color, a, y, b)) =
                  if Element.lt (x, y) then balance (color, ins a, y ,b)
                  else if Element.lt (y, x) then balance (color, a, y, ins b)
		  else s
            val T (_, a, y, b) = ins s  (* guaranteed to be non-empty *)
	in T (B, a, y, b) end
end
