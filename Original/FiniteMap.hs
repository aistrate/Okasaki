-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module FiniteMap (FiniteMap(..)) where
  class FiniteMap m k where
    emptyFM  :: m k a
    bindFM   :: k -> a -> m k a -> m k a
    lookupFM :: k -> m k a -> Maybe a
