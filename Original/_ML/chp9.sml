(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 9                              *)
(***********************************************************************)

structure Dense =
struct
  datatype Digit = Zero | One
  type Nat = Digit list  (* increasing order of significance *)

  fun inc [] = [One]
    | inc (Zero :: ds) = One :: ds
    | inc (One :: ds) = Zero :: inc ds    (* carry *)

  fun dec [One] = []
    | dec (One :: ds) = Zero :: ds
    | dec (Zero :: ds) = One :: dec ds    (* borrow *)

  fun add (ds, []) = ds
    | add ([], ds) = ds
    | add (d :: ds1, Zero :: ds2) = d :: add (ds1, ds2)
    | add (Zero :: ds1, d :: ds2) = d :: add (ds1, ds2)
    | add (One :: ds1, One :: ds2) =
        Zero :: inc (add (ds1, ds2))      (* carry *)
end

structure SparseByWeight =
struct
  type Nat = int list  (* increasing list of weights, each a power of two *)

  fun carry (w, []) = [w]
    | carry (w, ws as w' :: ws') =
        if w < w' then w :: ws else carry (2*w, ws')

  fun borrow (w, ws as w' :: ws') =
        if w = w' then ws' else w :: borrow (2*w, ws)

  fun inc ws = carry (1, ws)
  fun dec ws = borrow (1, ws)

  fun add (ws, []) = ws
    | add ([], ws) = ws
    | add (m as w1 :: ws1, n as w2 :: ws2) =
        if w1 < w2 then w1 :: add (ws1, n)
        else if w2 < w1 then w2 :: add (m, ws2)
        else carry (2*w1, add (ws1, ws2))
end

signature RANDOM_ACCESS_LIST =
sig
  type 'a RList

  val empty   : 'a RList
  val isEmpty : 'a RList -> bool

  val cons    : 'a * 'a RList -> 'a RList
  val head    : 'a RList -> 'a
  val tail    : 'a RList -> 'a RList
        (* head and tail raise Empty if list is empty *)

  val lookup  : int * 'a RList -> 'a
  val update  : int * 'a * 'a RList -> 'a RList
        (* lookup and update raise Subscript if index is out of bounds *)
end

structure BinaryRandomAccessList : RANDOM_ACCESS_LIST =
struct
  datatype 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
  datatype 'a Digit = Zero | One of 'a Tree
  type 'a RList = 'a Digit list

  val empty = []
  fun isEmpty ts = null ts

  fun size (Leaf x) = 1
    | size (Node (w, t1, t2)) = w
  fun link (t1, t2) = Node (size t1+size t2, t1, t2)
  fun consTree (t, []) = [One t]
    | consTree (t, Zero :: ts) = One t :: ts
    | consTree (t1, One t2 :: ts) = Zero :: consTree (link (t1, t2), ts)
  fun unconsTree [] = raise Empty
    | unconsTree [One t] = (t, [])
    | unconsTree (One t :: ts) = (t, Zero :: ts)
    | unconsTree (Zero :: ts) =
        let val (Node (_, t1, t2), ts') = unconsTree ts
	in (t1, One t2 :: ts') end

  fun cons (x, ts) = consTree (Leaf x, ts)
  fun head ts = let val (Leaf x, _) = unconsTree ts in x end
  fun tail ts = let val (_, ts') = unconsTree ts in ts' end

  fun lookupTree (0, Leaf x) = x
    | lookupTree (i, Leaf x) = raise Subscript
    | lookupTree (i, Node (w, t1, t2)) =
        if i < w div 2 then lookupTree (i, t1)
        else lookupTree (i - w div 2, t2)
  fun updateTree (0, y, Leaf x) = Leaf y
    | updateTree (i, y, Leaf x) = raise Subscript
    | updateTree (i, y, Node (w, t1, t2)) =
        if i < w div 2 then Node (w, updateTree (i, y, t1), t2)
        else Node (w, t1, updateTree (i - w div 2, y, t2))

  fun lookup (i, []) = raise Subscript
    | lookup (i, Zero :: ts) = lookup (i, ts)
    | lookup (i, One t :: ts) =
        if i < size t then lookupTree (i, t) else lookup (i - size t, ts)
  fun update (i, y, []) = raise Subscript
    | update (i, y, Zero :: ts) = Zero :: update (i, y, ts)
    | update (i, y, One t :: ts) =
        if i < size t then One (updateTree (i, y, t)) :: ts
        else One t :: update (i - size t, y, ts)
end

structure SkewBinaryRandomAccessList : RANDOM_ACCESS_LIST =
struct
  datatype 'a Tree = Leaf of 'a | Node of 'a * 'a Tree * 'a Tree
  type 'a RList = (int * 'a Tree) list  (* integer is the weight of the tree *)

  val empty = []
  fun isEmpty ts = null ts

  fun cons (x, ts as (w1, t1) :: (w2, t2) :: ts') =
        if w1 = w2 then (1+w1+w2, Node (x, t1, t2)) :: ts'
	else (1, Leaf x) :: ts
    | cons (x, ts) = (1, Leaf x) :: ts
  fun head [] = raise Empty
    | head ((1, Leaf x) :: ts) = x
    | head ((w, Node (x, t1, t2)) :: ts) = x
  fun tail [] = raise Empty
    | tail ((1, Leaf x) :: ts) = ts
    | tail ((w, Node (x, t1, t2)) :: ts) = (w div 2, t1) :: (w div 2, t2) :: ts

  fun lookupTree (1, 0, Leaf x) = x
    | lookupTree (1, i, Leaf x) = raise Subscript
    | lookupTree (w, 0, Node (x, t1, t2)) = x
    | lookupTree (w, i, Node (x, t1, t2)) =
        if i <= w div 2 then lookupTree (w div 2, i-1, t1)
	else lookupTree (w div 2, i - 1 - w div 2, t2)
  fun updateTree (1, 0, y, Leaf x) = Leaf y
    | updateTree (1, i, y, Leaf x) = raise Subscript
    | updateTree (w, 0, y, Node (x, t1, t2)) = Node (y, t1, t2)
    | updateTree (w, i, y, Node (x, t1, t2)) =
        if i <= w div 2 then Node (x, updateTree (w div 2, i-1, y, t1), t2)
        else Node (x, t1, updateTree (w div 2, i - 1 - w div 2, y, t2))

  fun lookup (i, []) = raise Subscript
    | lookup (i, (w, t) :: ts) =
        if i < w then lookupTree (w, i, t)
        else lookup (i-w, ts)
  fun update (i, y, []) = raise Subscript
    | update (i, y, (w, t) :: ts) =
        if i < w then (w, updateTree (w, i, y, t)) :: ts
        else (w, t) :: update (i-w, y, ts)
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

functor SkewBinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of int * Elem.T * Elem.T list * Tree list
  type Heap = Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun rank (Node (r, x, xs, c)) = r
  fun root (Node (r, x, xs, c)) = x
  fun link (t1 as Node (r, x1, xs1, c1), t2 as Node (_, x2, xs2, c2)) =
        if Elem.leq (x1, x2) then Node (r+1, x1, xs1, t2 :: c1)
        else Node (r+1, x2, xs2, t1 :: c2)
  fun skewLink (x, t1, t2) =
        let val Node (r, y, ys, c) = link (t1, t2)
	in
	    if Elem.leq (x, y) then Node (r, x, y :: ys, c)
	    else Node (r, y, x :: ys, c)
        end
  fun insTree (t, []) = [t]
    | insTree (t1, t2 :: ts) =
        if rank t1 < rank t2 then t1 :: t2 :: ts 
        else insTree (link (t1, t2), ts)
  fun mergeTrees (ts1, []) = ts1
    | mergeTrees ([], ts2) = ts2
    | mergeTrees (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
        if rank t1 < rank t2 then t1 :: mergeTrees (ts1', ts2)
        else if rank t2 < rank t1 then t2 :: mergeTrees (ts1, ts2')
        else insTree (link (t1, t2), mergeTrees (ts1', ts2'))
  fun normalize [] = []
    | normalize (t :: ts) = insTree (t, ts)

  fun insert (x, ts as t1 :: t2 :: rest) =
        if rank t1 = rank t2 then skewLink (x, t1, t2) :: rest
        else Node (0, x, [], []) :: ts
    | insert (x, ts) = Node (0, x, [], []) :: ts
  fun merge (ts1, ts2) = mergeTrees (normalize ts1, normalize ts2)

  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
        let val (t', ts') = removeMinTree ts
	in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end

  fun findMin ts = let val (t, _) = removeMinTree ts in root t end
  fun deleteMin ts =
        let val (Node (_, x, xs, ts1), ts2) = removeMinTree ts
	    fun insertAll ([], ts) = ts
	      | insertAll (x :: xs, ts) = insertAll (xs, insert (x, ts))
	in insertAll (xs, merge (rev ts1, ts2)) end
end
