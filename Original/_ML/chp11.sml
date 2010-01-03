(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 11                             *)
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

(*
structure ImplicitQueue : QUEUE =
  (* assumes polymorphic recursion! *)
struct
  datatype 'a Digit = Zero | One of 'a | Two of 'a * 'a
  datatype 'a Queue =
      Shallow of 'a Digit
    | Deep of 'a Digit * ('a * 'a) Queue susp * 'a Digit

  val empty = Shallow Zero
  fun isEmpty (Shallow Zero) = true | isEmpty _ = false

  fun snoc (Shallow Zero, y) = Shallow (One y)
    | snoc (Shallow (One x), y) = Deep (Two (x, y), $empty, Zero)
    | snoc (Deep (f, m, Zero), y) = Deep (f, m, One y)
    | snoc (Deep (f, m, One x), y) =
        Deep (f, $(snoc (force m, (x, y))), Zero)

  fun head (Shallow Zero) = raise Empty
    | head (Shallow (One x)) = x
    | head (Deep (One x, m, r)) = x
    | head (Deep (Two (x, y), m, r)) = x

  fun tail (Shallow Zero) = raise Empty
    | tail (Shallow (One x)) = empty
    | tail (Deep (Two (x, y), m, r)) = Deep (One y, m, r)
    | tail (Deep (One x, $q, r)) =
        if isEmpty q then Shallow r
        else let val (y,z) = head q
             in Deep (Two (y,z), $(tail q), r) end
end
*)


structure ImplicitQueue : QUEUE =
struct
  datatype 'a EP = Elem of 'a | Pair of 'a EP * 'a EP
  datatype 'a Digit = Zero | One of 'a | Two of 'a * 'a
  datatype 'a Queue =
      Shallow of 'a EP Digit
    | Deep of 'a EP Digit * 'a Queue susp * 'a EP Digit

  val empty = Shallow Zero
  fun isEmpty (Shallow Zero) = true | isEmpty _ = false

  fun snocEP (Shallow Zero, y) = Shallow (One y)
    | snocEP (Shallow (One x), y) = Deep (Two (x, y), $empty, Zero)
    | snocEP (Deep (f, m, Zero), y) = Deep (f, m, One y)
    | snocEP (Deep (f, m, One x), y) =
        Deep (f, $(snocEP (force m, Pair (x, y))), Zero)

  fun snoc (q, y) = snocEP (q, Elem y)

  fun headEP (Shallow Zero) = raise Empty
    | headEP (Shallow (One x)) = x
    | headEP (Deep (One x, m, r)) = x
    | headEP (Deep (Two (x, y), m, r)) = x

  fun head q = let val Elem x = headEP q in x end

  fun tail (Shallow Zero) = raise Empty
    | tail (Shallow (One x)) = empty
    | tail (Deep (Two (x, y), m, r)) = Deep (One y, m, r)
    | tail (Deep (One x, $q, r)) =
        if isEmpty q then Shallow r
        else let val Pair (y,z) = headEP q
             in Deep (Two (y,z), $(tail q), r) end
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

signature CATENABLE_DEQUE =
sig
  type 'a Cat

  val empty   : 'a Cat
  val isEmpty : 'a Cat -> bool

  val cons    : 'a * 'a Cat -> 'a Cat
  val head    : 'a Cat -> 'a       (* raises Empty if deque is empty *)
  val tail    : 'a Cat -> 'a Cat   (* raises Empty if deque is empty *)

  val snoc    : 'a Cat * 'a -> 'a Cat
  val last    : 'a Cat -> 'a       (* raises Empty if deque is empty *)
  val init    : 'a Cat -> 'a Cat   (* raises Empty if deque is empty *)

  val ++      : 'a Cat * 'a Cat -> 'a Cat
end


(*
functor SimpleCatenableDeque (D : DEQUE) : CATENABLE_DEQUE =
  (* assumes polymorphic recursion! *)
struct
  datatype 'a Cat =
      Shallow of 'a D.Queue
    | Deep of 'a D.Queue * 'a D.Queue Cat susp * 'a D.Queue

  fun tooSmall d = D.isEmpty d orelse D.isEmpty (D.tail d)

  fun dappendL (d1, d2) =
        if D.isEmpty d1 then d2 else D.cons (D.head d1, d2)
  fun dappendR (d1, d2) =
        if D.isEmpty d2 then d1 else D.snoc (d1, D.head d2)

  val empty = Shallow D.empty
  fun isEmpty (Shallow d) = D.isEmpty d
    | isEmpty _ = false

  fun cons (x, Shallow d) = Shallow (D.cons (x, d))
    | cons (x, Deep (f, m, r)) = Deep (D.cons (x, f), m, r)
  fun head (Shallow d) = D.head d
    | head (Deep (f, m, r)) = D.head f
  fun tail (Shallow d) = Shallow (D.tail d)
    | tail (Deep (f, m, r)) =
        let val f' = D.tail f
        in
            if not (tooSmall f') then Deep (f', m, r)
            else if isEmpty (force m) then Shallow (dappendL (f', r))
            else Deep (dappendL (f', head (force m)), $(tail (force m)), r)
        end

  val snoc = stub
  val last = stub
  val init = stub

  fun (Shallow d1) ++ (Shallow d2) =
        if tooSmall d1 then Shallow (dappendL (d1, d2))
        else if tooSmall d2 then Shallow (dappendR (d1, d2))
        else Deep (d1, $empty, d2)
    | (Shallow d) ++ (Deep (f, m, r)) =
        if tooSmall d then Deep (dappendL (d, f), m, r)
        else Deep (d, $(cons (f, force m)), r)
    | (Deep (f, m, r)) ++ (Shallow d) =
        if tooSmall d then Deep (f, m, dappendR (r, d))
        else Deep (f, $(snoc (force m, r)), d)
    | (Deep (f1, m1, r1)) ++ (Deep (f2, m2, r2)) =
        Deep (f1, $(snoc (force m1, r1) ++ cons (f2, force m2)), r2)
end
*)


functor SimpleCatenableDeque (D : DEQUE) : CATENABLE_DEQUE =
struct
  datatype 'a ED = Elem of 'a | Deque of 'a ED D.Queue

  datatype 'a Cat =
      Shallow of 'a ED D.Queue
    | Deep of 'a ED D.Queue * 'a Cat susp * 'a ED D.Queue

  fun tooSmall d = D.isEmpty d orelse D.isEmpty (D.tail d)

  fun dappendL (d1, d2) =
        if D.isEmpty d1 then d2 else D.cons (D.head d1, d2)
  fun dappendR (d1, d2) =
        if D.isEmpty d2 then d1 else D.snoc (d1, D.head d2)

  val empty = Shallow D.empty
  fun isEmpty (Shallow d) = D.isEmpty d
    | isEmpty _ = false

  fun consED (x, Shallow d) = Shallow (D.cons (x, d))
    | consED (x, Deep (f, m, r)) = Deep (D.cons (x, f), m, r)

  fun cons (x, d) = consED (Elem x, d)

  fun headED (Shallow d) = D.head d
    | headED (Deep (f, m, r)) = D.head f

  fun head d = let val Elem x = headED d in x end

  fun tail (Shallow d) = Shallow (D.tail d)
    | tail (Deep (f, m, r)) =
        let val f' = D.tail f
        in
            if not (tooSmall f') then Deep (f', m, r)
            else if isEmpty (force m) then Shallow (dappendL (f', r))
            else let val Deque d = headED (force m)
                 in Deep (dappendL (f', d), $(tail (force m)), r) end
        end

  val snoc = stub
  val last = stub
  val init = stub

  fun (Shallow d1) ++ (Shallow d2) =
        if tooSmall d1 then Shallow (dappendL (d1, d2))
        else if tooSmall d2 then Shallow (dappendR (d1, d2))
        else Deep (d1, $empty, d2)
    | (Shallow d) ++ (Deep (f, m, r)) =
        if tooSmall d then Deep (dappendL (d, f), m, r)
        else Deep (d, $(consED (Deque f, force m)), r)
    | (Deep (f, m, r)) ++ (Shallow d) =
        if tooSmall d then Deep (f, m, dappendR (r, d))
        else Deep (f, $(snoc (force m, Deque r)), d)
    | (Deep (f1, m1, r1)) ++ (Deep (f2, m2, r2)) =
        Deep (f1, $(snoc (force m1, Deque r1) ++ consED (Deque f2, force m2)), r2)
end



signature SIZED_DEQUE =
sig
  include DEQUE
  val size : 'a Queue -> int
end

(*
functor ImplicitCatenableDeque(D : SIZED_DEQUE) : CATENABLE_DEQUE =
struct
  datatype 'a Cat =
      Shallow of 'a D.Queue
    | Deep of 'a D.Queue * 'a CmpdElem Cat susp * 'a D.Queue
                         * 'a CmpdElem Cat susp * 'a D.Queue

  and 'a CmpdElem =
      Simple of 'a D.Queue
    | Cmpd of 'a D.Queue * 'a CmpdElem Cat susp * 'a D.Queue

  val empty = Shallow D.empty
  fun isEmpty (Shallow d) = D.isEmpty d
    | isEmpty _ = false

  fun cons (x, Shallow d) = Shallow (D.cons (x, d))
    | cons (x, Deep (f, a, m, b, r)) = Deep (D.cons (x, f), a, m, b, r)

  fun head (Shallow d) = D.head d
    | head (Deep (f, a, m, b, r)) = D.head f

  val snoc = stub
  val last = stub

  fun share (f, r) =
        let val m = D.cons (D.last f, D.cons (D.head r, D.empty))
        in (D.init f, m, D.tail r) end
  fun dappendL (d1, d2) =
        if D.isEmpty d1 then d2
        else dappendL (D.init d1, D.cons (D.last d1, d2))
  fun dappendR (d1, d2) = 
        if D.isEmpty d2 then d1
        else dappendR (D.snoc (d1, D.head d2), D.tail d2)

  fun (Shallow d1) ++ (Shallow d2) =
        if D.size d1 < 4 then Shallow (dappendL (d1, d2))
        else if D.size d2 < 4 then Shallow (dappendR (d1, d2))
        else let val (f, m, r) = share (d1, d2)
             in Deep (f, $empty, m, $empty, r) end
    | (Shallow d) ++ (Deep (f, a, m, b, r)) =
        if D.size d < 4 then Deep (dappendL (d, f), a, m, b, r)
        else Deep (d, $(cons (Simple f, force a)), m, b, r)
    | (Deep (f, a, m, b, r)) ++ (Shallow d) =
        if D.size d < 4 then Deep (f, a, m, b, dappendR (r, d))
        else Deep (f, a, m, $(snoc (force b, Simple r)), d)
    | (Deep (f1, a1, m1, b1, r1)) ++ (Deep (f2, a2, m2, b2, r2)) =
        let val (r1',m,f2') = share (r1, f2)
            val a1' = $(snoc (force a1, Cmpd (m1, b1, r1')))
            val b2' = $(cons (Cmpd (f2', a2, m2), force b2))
        in Deep (f1, a1', m, b2', r2) end

  fun replaceHead (x, Shallow d) = Shallow (D.cons (x, D.tail d))
    | replaceHead (x, Deep (f, a, m, b, r)) =
        Deep (D.cons (x, D.tail f), a, m, b, r)

  fun tail (Shallow d) = Shallow (D.tail d)
    | tail (Deep (f, a, m, b, r)) =
        if D.size f > 3 then Deep (D.tail f, a, m, b, r)
        else if not (isEmpty (force a)) then
          case head (force a) of
            Simple d =>
              let val f' = dappendL (D.tail f, d)
              in Deep (f', $(tail (force a)), m, b, r) end
          | Cmpd (f', c', r') =>
              let val f'' = dappendL (D.tail f, f')
                  val a'' = $(force c' ++ replaceHead (Simple r', force a))
              in Deep (f'', a'', m, b, r) end
        else if not (isEmpty (force b)) then
          case head (force b) of
            Simple d =>
              let val f' = dappendL (D.tail f, m)
              in Deep (f', $empty, d, $(tail (force b)), r) end
          | Cmpd (f', c', r') =>
              let val f'' = dappendL (D.tail f, m)
                  val a'' = $(cons (Simple f', force c'))
              in Deep (f'', a'', r', $(tail (force b)), r) end
        else Shallow (dappendL (D.tail f, m)) ++ Shallow r

  val init = stub
end
*)

functor ImplicitCatenableDeque(D : SIZED_DEQUE) : CATENABLE_DEQUE =
struct
  datatype 'a Cat =
      Shallow of 'a CmpdElem D.Queue
    | Deep of 'a CmpdElem D.Queue * 'a Cat susp * 'a CmpdElem D.Queue
                                  * 'a Cat susp * 'a CmpdElem D.Queue

  and 'a CmpdElem =
      Elem of 'a
    | Simple of 'a CmpdElem D.Queue
    | Cmpd of 'a CmpdElem D.Queue * 'a Cat susp * 'a CmpdElem D.Queue

  val empty = Shallow D.empty
  fun isEmpty (Shallow d) = D.isEmpty d
    | isEmpty _ = false

  fun consCE (x, Shallow d) = Shallow (D.cons (x, d))
    | consCE (x, Deep (f, a, m, b, r)) = Deep (D.cons (x, f), a, m, b, r)

  fun cons (x, d) = consCE (Elem x, d)

  fun headCE (Shallow d) = D.head d
    | headCE (Deep (f, a, m, b, r)) = D.head f

  fun head d = let val Elem x = headCE d in x end

  val snocCE = stub
  val snoc = stub

  val lastCE = stub
  val last = stub

  fun share (f, r) =
        let val m = D.cons (D.last f, D.cons (D.head r, D.empty))
        in (D.init f, m, D.tail r) end
  fun dappendL (d1, d2) =
        if D.isEmpty d1 then d2
        else dappendL (D.init d1, D.cons (D.last d1, d2))
  fun dappendR (d1, d2) = 
        if D.isEmpty d2 then d1
        else dappendR (D.snoc (d1, D.head d2), D.tail d2)

  fun (Shallow d1) ++ (Shallow d2) =
        if D.size d1 < 4 then Shallow (dappendL (d1, d2))
        else if D.size d2 < 4 then Shallow (dappendR (d1, d2))
        else let val (f, m, r) = share (d1, d2)
             in Deep (f, $empty, m, $empty, r) end
    | (Shallow d) ++ (Deep (f, a, m, b, r)) =
        if D.size d < 4 then Deep (dappendL (d, f), a, m, b, r)
        else Deep (d, $(consCE (Simple f, force a)), m, b, r)
    | (Deep (f, a, m, b, r)) ++ (Shallow d) =
        if D.size d < 4 then Deep (f, a, m, b, dappendR (r, d))
        else Deep (f, a, m, $(snocCE (force b, Simple r)), d)
    | (Deep (f1, a1, m1, b1, r1)) ++ (Deep (f2, a2, m2, b2, r2)) =
        let val (r1',m,f2') = share (r1, f2)
            val a1' = $(snocCE (force a1, Cmpd (m1, b1, r1')))
            val b2' = $(consCE (Cmpd (f2', a2, m2), force b2))
        in Deep (f1, a1', m, b2', r2) end

  fun replaceHead (x, Shallow d) = Shallow (D.cons (x, D.tail d))
    | replaceHead (x, Deep (f, a, m, b, r)) =
        Deep (D.cons (x, D.tail f), a, m, b, r)

  fun tail (Shallow d) = Shallow (D.tail d)
    | tail (Deep (f, a, m, b, r)) =
        if D.size f > 3 then Deep (D.tail f, a, m, b, r)
        else if not (isEmpty (force a)) then
          case headCE (force a) of
            Simple d =>
              let val f' = dappendL (D.tail f, d)
              in Deep (f', $(tail (force a)), m, b, r) end
          | Cmpd (f', c', r') =>
              let val f'' = dappendL (D.tail f, f')
                  val a'' = $(force c' ++ replaceHead (Simple r', force a))
              in Deep (f'', a'', m, b, r) end
        else if not (isEmpty (force b)) then
          case headCE (force b) of
            Simple d =>
              let val f' = dappendL (D.tail f, m)
              in Deep (f', $empty, d, $(tail (force b)), r) end
          | Cmpd (f', c', r') =>
              let val f'' = dappendL (D.tail f, m)
                  val a'' = $(consCE (Simple f', force c'))
              in Deep (f'', a'', r', $(tail (force b)), r) end
        else Shallow (dappendL (D.tail f, m)) ++ Shallow r

  val init = stub
end

