(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 4                              *)
(***********************************************************************)

signature STREAM =
sig
  datatype 'a StreamCell = Nil | Cons of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  val ++      : 'a Stream * 'a Stream -> 'a Stream  (* stream append *)
  val take    : int * 'a Stream -> 'a Stream
  val drop    : int * 'a Stream -> 'a Stream
  val reverse : 'a Stream -> 'a Stream
end

structure Stream : STREAM =
struct
  datatype 'a StreamCell = Nil | Cons of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  (* fun lazy *)
  fun ($Nil) ++ t = t
    | ($(Cons (x, s))) ++ t = $(Cons (x, s ++ t))
  (* fun lazy *)
  fun take (0, s) = $Nil
    | take (n, $Nil) = $Nil
    | take (n, $(Cons (x, s))) = $(Cons (x, take (n-1, s)))
  (* fun lazy *)
  fun drop (n, s) =
        let fun drop' (0, s) = s
	      | drop' (n, $Nil) = $Nil
	      | drop' (n, $(Cons (x, s))) = drop' (n-1, s)
	in drop' (n, s) end
  (* fun lazy *)
  fun reverse s =
        let fun reverse' ($Nil, r) = r
	      | reverse' ($(Cons (x, s)), r) = reverse' (s, $(Cons (x, r)))
	in reverse' (s, $Nil) end
end
