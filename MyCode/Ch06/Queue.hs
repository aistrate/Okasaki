module Queue (Queue(..), fromList, toList) where
  
import Prelude hiding (head, tail)


class Queue q where
  empty   :: q a
  isEmpty :: q a -> Bool
  
  snoc    :: q a -> a -> q a
  head    :: q a -> a
  tail    :: q a -> q a


fromList :: Queue q => [a] -> q a
fromList = foldl snoc empty


toList :: Queue q => q a -> [a]
toList q | isEmpty q = []
         | otherwise = head q : toList (tail q)
