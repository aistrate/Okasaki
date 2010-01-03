(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 7                              *)
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

signature SORTABLE =
sig
  structure Elem : ORDERED

  type Sortable

  val empty : Sortable
  val add   : Elem.T * Sortable -> Sortable
  val sort  : Sortable -> Elem.T list
end

open Stream

structure RealTimeQueue : QUEUE =
struct
  type 'a Queue = 'a Stream * 'a list * 'a Stream

  val empty = ($Nil, [], $Nil)
  fun isEmpty ($Nil, _, _) = true
    | isEmpty _ = false

  fun rotate ($Nil, y :: _, a) = $(Cons (y, a))
    | rotate ($(Cons (x, xs)), y :: ys, a) =
        $(Cons (x, rotate (xs, ys, $(Cons (y, a)))))

  fun exec (f, r, $(Cons (x, s))) = (f, r, s)
    | exec (f, r, $Nil) = let val f' = rotate (f, r, $Nil) in (f', [], f') end

  fun snoc ((f, r, s), x) = exec (f, x :: r, s)

  fun head ($Nil, r, s) = raise Empty
    | head ($(Cons (x, f)), r, s) = x
  fun tail ($Nil, r, s) = raise Empty
    | tail ($(Cons (x, f)), r, s) = exec (f, r, s)
end

fun listToStream [] = $Nil
  | listToStream (x :: xs) = $(Cons (x, listToStream xs))

functor ScheduledBinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of Elem.T * Tree list
  datatype Digit = Zero | One of Tree
  type Schedule = Digit Stream list
  type Heap = Digit Stream * Schedule

  val empty = ($Nil, [])
  fun isEmpty ($Nil, _) = true | isEmpty _ = false

  fun link (t1 as Node (x1, c1), t2 as Node (x2, c2)) =
        if Elem.leq (x1, x2) then Node (x1, t2 :: c1) else Node (x2, t1 :: c2)
  fun insTree (t, $Nil) = $(Cons (One t, $Nil))
    | insTree (t, $(Cons (Zero, ds))) = $(Cons (One t, ds))
    | insTree (t, $(Cons (One t', ds))) =
        $(Cons (Zero, insTree (link (t, t'), ds)))
  fun mrg (ds1, $Nil) = ds1
    | mrg ($Nil, ds2) = ds2
    | mrg ($(Cons (Zero, ds1)), $(Cons (d, ds2))) = $(Cons (d, mrg (ds1, ds2)))
    | mrg ($(Cons (d, ds1)), $(Cons (Zero, ds2))) = $(Cons (d, mrg (ds1, ds2)))
    | mrg ($(Cons (One t1, ds1)), $(Cons (One t2, ds2))) =
        $(Cons (Zero, insTree (link (t1, t2), mrg (ds1, ds2))))

  fun normalize (ds as $Nil) = ds
    | normalize (ds as $(Cons (_, ds'))) = (normalize ds'; ds)
  fun exec [] = []
    | exec (($(Cons (Zero, job))) :: sched) = job :: sched
    | exec (_ :: sched) = sched

  fun insert (x, (ds, sched)) = 
        let val ds' = insTree (Node (x, []), ds)
	in (ds', exec (exec (ds' :: sched))) end
  fun merge ((ds1, _), (ds2, _)) =
        let val ds = normalize (mrg (ds1, ds2)) in (ds, []) end

  fun removeMinTree ($Nil) = raise Empty
    | removeMinTree ($(Cons (One t, $Nil))) = (t, $Nil)
    | removeMinTree ($(Cons (Zero, ds))) =
        let val (t', ds') = removeMinTree ds in (t', $(Cons (Zero, ds'))) end
    | removeMinTree ($(Cons (One (t as Node (x, _)), ds))) =
        case removeMinTree ds of
          (t' as Node (x', _), ds') =>
             if Elem.leq (x, x') then (t, $(Cons (Zero, ds)))
             else (t', $(Cons (One t, ds')))

  fun findMin (ds, _) = let val (Node (x, _), _) = removeMinTree ds in x end
  fun deleteMin (ds, _) =
        let val (Node (x, c), ds') = removeMinTree ds
            val ds'' = mrg (listToStream (map One (rev c)), ds')
	in (normalize ds'', []) end
end

fun streamToList ($Nil) = []
  | streamToList ($(Cons (x, xs))) = x :: streamToList xs

functor ScheduledBottomUpMergeSort (Element : ORDERED) : SORTABLE =
struct
  structure Elem = Element

  type Schedule = Elem.T Stream list
  type Sortable = int * (Elem.T Stream * Schedule) list

  (* fun lazy *)
  fun mrg ($Nil, ys) = ys
    | mrg (xs, $Nil) = xs
    | mrg (xs as $(Cons (x, xs')), ys as $(Cons (y, ys'))) =
        if Elem.leq (x, y) then $(Cons (x, mrg (xs', ys)))
        else $(Cons (y, mrg (xs, ys')))

  fun exec1 [] = []
    | exec1 (($Nil) :: sched) = exec1 sched
    | exec1 (($(Cons (x, xs))) :: sched) = xs :: sched
  fun exec2 (xs, sched) = (xs, exec1 (exec1 sched))

  val empty = (0, [])
  fun add (x, (size, segs)) =
        let fun addSeg (xs, segs, size, rsched) =
              if size mod 2 = 0 then (xs, rev rsched) :: segs
              else let val ((xs', []) :: segs') = segs
                       val xs'' = mrg (xs, xs')
		   in addSeg (xs'', segs', size div 2, xs'' :: rsched) end
	    val segs' = addSeg ($(Cons (x, $Nil)), segs, size, [])
	in (size+1, map exec2 segs') end
  fun sort (size, segs) =
        let fun mrgAll (xs, []) = xs
	      | mrgAll (xs, (xs', _) :: segs) = mrgAll (mrg (xs, xs'), segs)
	in streamToList (mrgAll ($Nil, segs)) end
end
