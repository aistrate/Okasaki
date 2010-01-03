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
