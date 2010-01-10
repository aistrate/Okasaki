module Deque (Deque(..), list2Front, list2Rear, front2List, rear2List) where

import Prelude hiding (head, tail, last, init)


class Deque q where
  empty   :: q a
  isEmpty :: q a -> Bool
  
  cons    :: a -> q a -> q a
  head    :: q a -> a
  tail    :: q a -> q a
  
  snoc    :: q a -> a -> q a
  last    :: q a -> a
  init    :: q a -> q a


list2Front, list2Rear :: Deque q => [a] -> q a
list2Front = foldl (flip cons) empty
list2Rear = foldl snoc empty


front2List, rear2List :: Deque q => q a -> [a]
front2List q | isEmpty q = []
             | otherwise = head q : front2List (tail q)
rear2List q | isEmpty q = []
            | otherwise = last q : rear2List (init q)
