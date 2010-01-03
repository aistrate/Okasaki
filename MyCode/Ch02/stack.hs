{-
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
-}


class Stack stk where
  empty :: stk a
  isEmpty :: stk a -> Bool
  
  consS :: a -> stk a -> stk a
  headS :: stk a -> a
  tailS :: stk a -> stk a


instance Stack [] where
  empty = []
  
  isEmpty [] = True
  isEmpty (_:_) = False
  
  consS x xs = x:xs
  
  headS [] = error "Stack is empty"
  headS (x:_) = x        -- or: headS = head

  tailS [] = error "Stack is empty"
  tailS (_:xs) = xs      -- or tailS = tail


data CustomStack a = Nil 
                   | Cons a (CustomStack a)
                   deriving (Show)

instance Stack CustomStack where
  empty = Nil
  
  isEmpty Nil = True
  isEmpty _   = False
  
  consS x xs = Cons x xs
  
  headS Nil = error "Stack is empty"
  headS (Cons x _) = x
  
  tailS Nil = error "Stack is empty"
  tailS (Cons _ xs) = xs
