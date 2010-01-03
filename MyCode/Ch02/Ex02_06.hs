{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Ex02_06 where

import UnbalancedSet


class FiniteMap m k a where
  emptyFM :: m k a
  lookupFM :: k -> m k a -> Maybe a
  bindFM :: k -> a -> m k a -> m k a


newtype UnbalancedMap k a = M { getTree :: Tree (k, a) }
                          deriving (Eq, Show)


instance Ordered k => FiniteMap UnbalancedMap k a where
  emptyFM = M E
  
  lookupFM x (M E) = Nothing
  lookupFM x (M (T a (y, v) b)) =
    if x `lt` y then lookupFM x (M a)
    else if y `lt` x then lookupFM x (M b)
         else Just v
  
  bindFM x v (M E) = M (T E (x, v) E)
  bindFM x v (M (T a p@(y, w) b)) =
    if x `lt` y then M (T (getTree $ bindFM x v (M a)) p b)
    else if y `lt` x then M (T a p (getTree $ bindFM x v (M b)))
         else M (T a (x, v) b)


makeFmap :: Ordered k => [(k, a)] -> UnbalancedMap k a
makeFmap = foldl (\m (k, v) -> bindFM k v m) emptyFM
