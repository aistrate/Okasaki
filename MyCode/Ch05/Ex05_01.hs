module Ex05_01 (module Deque, BatchedDeque) where
  
import Prelude hiding (head, tail, last, init)
import Deque


data BatchedDeque a = BD [a] [a]
                      deriving (Eq, Show)


-- check [] [] = empty
-- check f  [] = let (f', r') = split f in BD f' (reverse r')
-- check [] r  = let (r', f') = split r in BD (reverse f') r'
-- check f  r  = BD f r

check f@(_:_:_) [] = let (f', r') = split f in BD f' (reverse r')
check [] r@(_:_:_) = let (r', f') = split r in BD (reverse f') r'
check f r = BD f r

split xs = splitAt ((length xs + 1) `div` 2) xs


instance Deque BatchedDeque where
  empty = BD [] []
  isEmpty (BD f r) = null f && null r
  
  cons x (BD f r) = check (x:f) r
  
  head (BD [] []) = error "BatchedDeque.head: empty deque"
  head (BD [] [x]) = x
  head (BD (x:f) r) = x
  
  tail (BD [] []) = error "BatchedDeque.tail: empty deque"
  tail (BD [] [x]) = empty
  tail (BD (x:f) r) = check f r
  
  
  snoc (BD f r) x = check f (x:r)
  
  last (BD [] []) = error "BatchedDeque.last: empty deque"
  last (BD [x] []) = x
  last (BD f (x:r)) = x
  
  init (BD [] []) = error "BatchedDeque.init: empty deque"
  init (BD [x] []) = empty
  init (BD f (x:r)) = check f r
