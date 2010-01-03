(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 8                              *)
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

open Stream

structure HoodMelvilleQueue : QUEUE =
struct
  datatype 'a RotationState =
      Idle
    | Reversing of int * 'a list * 'a list * 'a list * 'a list
    | Appending of int * 'a list * 'a list
    | Done of 'a list

  type 'a Queue = int * 'a list * 'a RotationState * int * 'a list

  fun exec (Reversing (ok, x :: f, f', y :: r, r')) =
        Reversing (ok+1, f, x :: f', r, y :: r')
    | exec (Reversing (ok, [], f', [y], r')) = Appending (ok, f', y :: r')
    | exec (Appending (0, f', r')) = Done r'
    | exec (Appending (ok, x :: f', r')) = Appending (ok-1, f', x :: r')
    | exec state = state

  fun invalidate (Reversing (ok, f, f', r, r')) =
        Reversing (ok-1, f, f', r, r')
    | invalidate (Appending (0, f', x :: r')) = Done r'
    | invalidate (Appending (ok, f', r')) = Appending (ok-1, f', r')
    | invalidate state = state

  fun exec2 (lenf, f, state, lenr, r) =
        case exec (exec state) of
            Done newf => (lenf, newf, Idle, lenr, r)
	  | newstate => (lenf, f, newstate, lenr, r)

  fun check (q as (lenf, f, state, lenr, r)) =
        if lenr <= lenf then exec2 q
        else let val newstate = Reversing (0, f, [], r, [])
             in exec2 (lenf+lenr, f, newstate, 0, []) end

  val empty = (0, [], Idle, 0, [])
  fun isEmpty (lenf, f, state, lenr, r) = (lenf = 0)

  fun snoc ((lenf, f, state, lenr, r), x) = 
        check (lenf,f,state,lenr+1,x::r)
  fun head (lenf, [], state, lenr, r) = raise Empty
    | head (lenf, x :: f, state, lenr, r) = x
  fun tail (lenf, [], state, lenr, r) = raise Empty
    | tail (lenf, x :: f, state, lenr, r) =
        check (lenf-1, f, invalidate state, lenr, r)
end

functor BankersDeque (val c : int) : DEQUE =  (* c > 1 *)
struct
  type 'a Queue = int * 'a Stream * int * 'a Stream

  val empty = (0, $Nil, 0, $Nil)
  fun isEmpty (lenf, f, lenr, r) = (lenf+lenr = 0)

  fun check (q as (lenf, f, lenr, r)) =
        if lenf > c*lenr+1 then
          let val i = (lenf+lenr) div 2   val j = lenf + lenr - i
              val f' = take (i, f)        val r' = r ++ reverse (drop (i, f))
          in (i, f', j, r') end
        else if lenr > c*lenf + 1 then
	  let val j = (lenf+lenr) div 2   val i = lenf + lenr - j
              val r' = take (j, r)        val f' = f ++ reverse (drop (j, r))
          in (i, f', j, r') end
        else q

  fun cons (x, (lenf, f, lenr, r)) = check (lenf+1, $(Cons (x, f)), lenr, r)
  fun head (lenf, $Nil, lenr, $Nil) = raise Empty
    | head (lenf, $Nil, lenr, $(Cons (x, _))) = x
    | head (lenf, $(Cons (x, f')), lenr, r) = x
  fun tail (lenf, $Nil, lenr, $Nil) = raise Empty
    | tail (lenf, $Nil, lenr, $(Cons (x, _))) = empty
    | tail (lenf, $(Cons (x, f')), lenr, r) = check (lenf-1, f', lenr, r)

  val snoc = stub
  val last = stub
  val init = stub
end

functor RealTimeDeque (val c : int) : DEQUE =  (* c = 2 or c = 3 *)
struct
  type 'a Queue =
            int * 'a Stream * 'a Stream * int * 'a Stream * 'a Stream

  val empty = (0, $Nil, $Nil, 0, $Nil, $Nil)
  fun isEmpty (lenf, f, sf, lenr, r, sr) = (lenf+lenr = 0)

  fun exec1 ($(Cons (x, s))) = s
    | exec1 s = s
  fun exec2 s = exec1 (exec1 s)

  fun rotateRev ($Nil, r, a) = reverse r ++ a
    | rotateRev ($(Cons (x, f)), r, a) =
        $(Cons (x, rotateRev (f, drop (c, r), reverse (take (c, r)) ++ a)))
  fun rotateDrop (f, j, r) =
        if j < c then rotateRev (f, drop (j, r), $Nil)
	else let val ($(Cons (x, f'))) = f
	     in $(Cons (x, rotateDrop (f', j-c, drop (c,r)))) end

  fun check (q as (lenf, f, sf, lenr, r, sr)) =
        if lenf > c*lenr+1 then
          let val i = (lenf+lenr) div 2   val j = lenf+lenr-i
              val f' = take (i, f)        val r' = rotateDrop (r, i, f)
          in (i, f', f', j, r', r') end
        else if lenr > c*lenf+1 then
          let val j = (lenf+lenr) div 2   val i = lenf+lenr-j
              val r' = take (j, r)        val f' = rotateDrop (f, j, r)
          in (i, f', f', j, r', r') end
        else q

  fun cons (x, (lenf, f, sf, lenr, r, sr)) =
        check (lenf+1, $(Cons (x, f)), exec1 sf, lenr, r, exec1 sr)
  fun head (lenf, $Nil, sf, lenr, $Nil, sr) = raise Empty
    | head (lenf, $Nil, sf, lenr, $(Cons (x, _)), sr) = x
    | head (lenf, $(Cons (x, f')), sf, lenr, r, sr) = x
  fun tail (lenf, $Nil, sf, lenr, $Nil, sr) = raise Empty
    | tail (lenf, $Nil, sf, lenr, $(Cons (x, _)), sr) = empty
    | tail (lenf, $(Cons (x, f')), sf, lenr, r, sr) =
        check (lenf-1, f', exec2 sf, lenr, r, exec2 sr)

  val snoc = stub
  val last = stub
  val init = stub
end
