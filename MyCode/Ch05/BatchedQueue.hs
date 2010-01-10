module BatchedQueue (module Queue, BatchedQueue) where
  
import Prelude hiding (head, tail)
import Queue


data BatchedQueue a = BQ [a] [a]
                      deriving (Eq, Show)


check [] r = BQ (reverse r) []
check f r = BQ f r


instance Queue BatchedQueue where
  empty = BQ [] []
  isEmpty (BQ f r) = null f
  
  snoc (BQ f r) x = check f (x:r)
  
  head (BQ [] _) = error "BatchedQueue.head: empty queue"
  head (BQ (x:f) r) = x
  
  tail (BQ [] _) = error "BatchedQueue.tail: empty queue"
  tail (BQ (x:f) r) = check f r
