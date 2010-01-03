(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 10                             *)
(***********************************************************************)

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


signature QUEUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val tail    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)
end

(*
structure AltBinaryRandomAccessList : RANDOM_ACCESS_LIST =
  (* assumes polymorphic recursion! *)
struct
  datatype 'a RList = 
      Nil | Zero of ('a * 'a) RList | One of 'a * ('a * 'a) RList

  val empty = Nil
  fun isEmpty Nil = true | isEmpty _ = false

  fun cons (x, Nil) = One (x, Nil)
    | cons (x, Zero ps) = One (x, ps)
    | cons (x, One (y, ps)) = Zero (cons ((x, y), ps))

  fun uncons Nil = raise Empty
    | uncons (One (x, Nil)) = (x, Nil)
    | uncons (One (x, ps)) = (x, Zero ps)
    | uncons (Zero ps) = let val ((x, y), ps') = uncons ps
			 in (x, One (y, ps')) end

  fun head xs = let val (x, _) = uncons xs in x end
  fun tail xs = let val (_, xs') = uncons xs in xs' end

  fun lookup (i, Nil) = raise Subscript
    | lookup (0, One (x, ps)) = x
    | lookup (i, One (x, ps)) = lookup (i-1, Zero ps)
    | lookup (i, Zero ps) = let val (x, y) = lookup (i div 2, ps)
			    in if i mod 2 = 0 then x else y end

  fun fupdate (f, i, Nil) = raise Subscript
    | fupdate (f, 0, One (x, ps)) = One (f x, ps)
    | fupdate (f, i, One (x, ps)) = cons (x, fupdate (f, i-1, Zero ps))
    | fupdate (f, i, Zero ps) =
        let fun f' (x, y) = if i mod 2 = 0 then (f x, y) else (x, f y)
	in Zero (fupdate (f', i div 2, ps)) end

  fun update (i, y, xs) = fupdate (fn x => y, i, xs)
end
*)

structure AltBinaryRandomAccessList : RANDOM_ACCESS_LIST =
  (* assumes polymorphic recursion! *)
struct
  datatype 'a EP = Elem of 'a | Pair of 'a EP * 'a EP
  datatype 'a RList = Nil | Zero of 'a RList | One of 'a EP * 'a RList

  val empty = Nil
  fun isEmpty Nil = true | isEmpty _ = false

  fun consEP (x, Nil) = One (x, Nil)
    | consEP (x, Zero ps) = One (x, ps)
    | consEP (x, One (y, ps)) = Zero (consEP (Pair (x, y), ps))

  fun cons (x, xs) = consEP (Elem x, xs)

  fun unconsEP Nil = raise Empty
    | unconsEP (One (x, Nil)) = (x, Nil)
    | unconsEP (One (x, ps)) = (x, Zero ps)
    | unconsEP (Zero ps) = let val (Pair (x, y), ps') = unconsEP ps
			   in (x, One (y, ps')) end

  fun head xs = let val (Elem x, _) = unconsEP xs in x end
  fun tail xs = let val (_, xs') = unconsEP xs in xs' end

  fun lookupEP (i, Nil) = raise Subscript
    | lookupEP (0, One (x, ps)) = x
    | lookupEP (i, One (x, ps)) = lookupEP (i-1, Zero ps)
    | lookupEP (i, Zero ps) = let val Pair (x, y) = lookupEP (i div 2, ps)
                              in if i mod 2 = 0 then x else y end

  fun lookup (i, xs) = let val Elem x = lookupEP (i, xs) in x end

  fun fupdate (f, i, Nil) = raise Subscript
    | fupdate (f, 0, One (x, ps)) = One (f x, ps)
    | fupdate (f, i, One (x, ps)) = consEP (x, fupdate (f, i-1, Zero ps))
    | fupdate (f, i, Zero ps) =
        let fun f' (Pair (x, y)) =
                  if i mod 2 = 0 then Pair (f x, y) else Pair (x, f y)
	in Zero (fupdate (f', i div 2, ps)) end

  fun update (i, y, xs) = fupdate (fn x => Elem y, i, xs)
end

(*
structure BootstrappedQueue : QUEUE =
  (* assumes polymorphic recursion! *)
struct
  datatype 'a Queue =
      E | Q of int * 'a list * 'a list susp Queue * int * 'a list

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun checkQ (q as (lenfm, f, m, lenr, r)) =
        if lenr <= lenfm then checkF q
        else checkF (lenfm+lenr, f, snoc (m, $(rev r)), 0, [])
  and checkF (lenfm, [], E, lenr, r) = E
    | checkF (lenfm, [], m, lenr, r) =
        Q (lenfm, force (head m), tail m, lenr, r)
    | checkF q = Q q

  and snoc (E, x) = Q (1, [x], E, 0, [])
    | snoc (Q (lenfm, f, m, lenr, r), x) = checkQ (lenfm, f, m, lenr+1, x :: r)
  and head E = raise Empty
    | head (Q (lenfm, x :: f', m, lenr, r)) = x
  and tail E = raise Empty
    | tail (Q (lenfm, x :: f', m, lenr, r)) = checkQ (lenfm-1, f', m, lenr, r)
end
*)

structure BootstrappedQueue : QUEUE =
  (* assumes polymorphic recursion! *)
struct
  datatype 'a EL = Elem of 'a | List of 'a EL list susp
  datatype 'a Queue =
      E | Q of int * 'a EL list * 'a Queue * int * 'a EL list

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun checkQ (q as (lenfm, f, m, lenr, r)) =
        if lenr <= lenfm then checkF q
        else checkF (lenfm+lenr, f, snocEL (m, List ($(rev r))), 0, [])
  and checkF (lenfm, [], E, lenr, r) = E
    | checkF (lenfm, [], m, lenr, r) =
        let val List ($f) = headEL m
	in Q (lenfm, f, tail m, lenr, r) end
    | checkF q = Q q

  and snocEL (E, x) = Q (1, [x], E, 0, [])
    | snocEL (Q (lenfm, f, m, lenr, r), x) = 
        checkQ (lenfm, f, m, lenr+1, x :: r)
  and headEL E = raise Empty
    | headEL (Q (lenfm, x :: f', m, lenr, r)) = x
  and tail E = raise Empty
    | tail (Q (lenfm, x :: f', m, lenr, r)) =
        checkQ (lenfm-1, f', m, lenr, r)

  fun snoc (xs, x) = snocEL (xs, Elem x)
  fun head xs = let val Elem x = headEL xs in x end
end


signature CATENABLE_LIST =
sig
  type 'a Cat

  val empty   : 'a Cat
  val isEmpty : 'a Cat -> bool

  val cons    : 'a * 'a Cat -> 'a Cat
  val snoc    : 'a Cat * 'a -> 'a Cat
  val ++      : 'a Cat * 'a Cat -> 'a Cat

  val head    : 'a Cat -> 'a      (* raises Empty if list is empty *)
  val tail    : 'a Cat -> 'a Cat  (* raises Empty if list is empty *)
end

functor CatenableList (Q : QUEUE) : CATENABLE_LIST =
struct
  datatype 'a Cat = E | C of 'a * 'a Cat susp Q.Queue

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun link (C (x, q), s) = C (x, Q.snoc (q, s))
  fun linkAll q = let val $t = Q.head q
                      val q' = Q.tail q
                  in if Q.isEmpty q' then t else link (t, $(linkAll q')) end

  fun xs ++ E = xs
    | E ++ xs = xs
    | xs ++ ys = link (xs, $ys)
  fun cons (x, xs) = C (x, Q.empty) ++ xs
  fun snoc (xs, x) = xs ++ C (x, Q.empty)

  fun head E = raise Empty
    | head (C (x, _)) = x
  fun tail E = raise Empty
    | tail (C (x, q)) = if Q.isEmpty q then E else linkAll q
end

(*
functor Bootstrap (functor MakeH (Element : ORDERED) 
                             : HEAP where type Elem.T = Element.T)
                  (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  (* recursive structures not supported in Standard ML! *)
  structure rec BootstrappedElem =
    struct
      datatype T = E | H of Elem.T * PrimH.Heap
      fun leq (H (x, _), H (y, _))= Elem.leq (x, y)
      val eq = stub
      val lt = stub
    end
  and PrimH = MakeH (BootstrappedElem)

  open BootstrappedElem  (* expose E and H constructors *)

  type Heap = BootstrappedElem.T

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (E, h) = h
    | merge (h, E) = h
    | merge (h1 as H (x, p1), h2 as H (y, p2)) =
        if Elem.leq (x, y) then H (x, PrimH.insert (h2, p1))
        else H (y, PrimH.insert (h1, p2))
  fun insert (x, h) = merge (H (x, PrimH.empty), h)

  fun findMin E = raise Empty
    | findMin (H (x, _)) = x
  fun deleteMin E = raise Empty
    | deleteMin (H (x, p)) =
        if PrimH.isEmpty p then E
        else let val (H (y, p1)) = PrimH.findMin p
                 val p2 = PrimH.deleteMin p
             in H (y, PrimH.merge (p1, p2)) end 
end
*)

signature ORDERED =
  (* a totally ordered type and its comparison functions *)
sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature HEAP_WITH_INFO =
sig
  structure Priority : ORDERED

  type 'a Heap

  val empty     : 'a Heap
  val isEmpty   : 'a Heap -> bool

  val insert    : Priority.T * 'a * 'a Heap -> 'a Heap
  val merge     : 'a Heap * 'a Heap -> 'a Heap

  val findMin   : 'a Heap -> Priority.T * 'a
  val deleteMin : 'a Heap -> 'a Heap
        (* findMin and deleteMin raise Empty if heap is empty *)
end

(*
functor Bootstrap (functor MakeH (Key : ORDERED) 
                             : HEAP_WITH_INFO where type Priority.T = Key.T)
                  (Key : ORDERED) : HEAP_WITH_INFO =
struct
  structure Priority = Key
  structure PrimH = MakeH (Key)

  datatype 'a PrimHeap = Prim of 'a * 'a PrimHeap PrimH.Heap
  datatype 'a Heap = E | H of Key.T * 'a * 'a PrimHeap PrimH.Heap

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (E, h) = h
    | merge (h, E) = h
    | merge (h1 as H (x, v1, p1), h2 as H (y, v2, p2)) =
        if Key.leq (x, y) then H (x, v1, PrimH.insert (y, Prim (v2, p2), p1))
        else H (y, v2, PrimH.insert (x, Prim (v1, p1), p2))
  fun insert (x, v, h) = merge (H (x, v, PrimH.empty), h)

  fun findMin E = raise Empty
    | findMin (H (x, v, _)) = (x, v)
  fun deleteMin E = raise Empty
    | deleteMin (H (x, v, p)) =
        if PrimH.isEmpty p then E
        else let val (y, Prim (v, p1)) = PrimH.findMin p
                 val p2 = PrimH.deleteMin p
             in H (y, v, PrimH.merge (p1, p2)) end 
end
*)

signature FINITE_MAP =
sig
  type Key
  type 'a Map

  val empty  : 'a Map
  val bind   : Key * 'a * 'a Map -> 'a Map
  val lookup : Key * 'a Map -> 'a    (* raise NotFound if key is not found *)
end

exception NotFound

functor Trie (M : FINITE_MAP) : FINITE_MAP =
struct
  type Key = M.Key list

  datatype 'a Map = Trie of 'a option * 'a Map M.Map

  val empty = Trie (NONE, M.empty)

  fun lookup ([], Trie (NONE, m)) = raise NotFound
    | lookup ([], Trie (SOME x, m)) = x
    | lookup (k :: ks, Trie (v, m)) = lookup (ks, M.lookup (k, m))

  fun bind ([], x, Trie (_, m)) = Trie (SOME x, m)
    | bind (k :: ks, x, Trie (v, m)) =
        let val t = M.lookup (k, m) handle NotFound => empty
	    val t' = bind (ks, x, t)
	in Trie (v, M.bind (k, t', m)) end
end

datatype 'a Tree = E | T of 'a * 'a Tree * 'a Tree

(*
functor TrieOfTrees (M : FINITE_MAP) : FINITE_MAP =
  (* assumes polymorphic recursion! *)
struct
  type Key = M.Key Tree

  datatype 'a Map = Trie of 'a option * 'a Map Map M.Map

  val empty = Trie (NONE, M.empty)

  fun lookup (E, Trie (NONE, m)) = raise NotFound
    | lookup (E, Trie (SOME x, m)) = x
    | lookup (T (k, a, b), Trie (v, m)) =
        lookup (b, lookup (a, M.lookup (k, m)))

  fun bind (E, x, Trie (_, m)) = Trie (SOME x, m)
    | bind (T (k, a, b), x, Trie (v, m)) =
        let val tt = M.lookup (k, m) handle NotFound => empty
            val t = lookup (a, tt) handle NotFound => empty
            val t' = bind (b, x, t)
	    val tt' = bind (a, t', tt)
	in Trie (v, M.bind (k, tt', m)) end
end
*)

functor TrieOfTrees (M : FINITE_MAP) : FINITE_MAP =
  (* assumes polymorphic recursion! *)
struct
  type Key = M.Key Tree

  datatype 'a Map = Trie of 'a EM option * 'a Map M.Map
       and 'a EM = Elem of 'a | Map of 'a Map

  val empty = Trie (NONE, M.empty)

  fun lookupEM (E, Trie (NONE, m)) = raise NotFound
    | lookupEM (E, Trie (SOME x, m)) = x
    | lookupEM (T (k, a, b), Trie (v, m)) =
        let val Map m' = lookupEM (a, M.lookup (k, m))
        in lookupEM (b, m') end

  fun lookup (t, trie) =
        let val Elem x = lookupEM (t, trie) in x end

  fun bindEM (E, x, Trie (_, m)) = Trie (SOME x, m)
    | bindEM (T (k, a, b), x, Trie (v, m)) =
        let val tt = M.lookup (k, m) handle NotFound => empty
            val Map t = lookupEM (a, tt) handle NotFound => Map empty
            val t' = bindEM (b, x, t)
	    val tt' = bindEM (a, Map t', tt)
	in Trie (v, M.bind (k, tt', m)) end

  fun bind (t, x, trie) = bindEM (t, Elem x, trie)
end
