-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module Trie (module FiniteMap, Trie) where
  import FiniteMap

{-
  data Trie m k ks a = Trie (Maybe a) (m k (Trie m k ks a))

  instance FiniteMap m k => FiniteMap (Trie m k) [k] where
    emptyFM = Trie Nothing emptyFM

    lookupFM [] (Trie b m) = b
    lookupFM (k : ks) (Trie b m) = lookupFM k m >>= \m' -> lookupFM ks m'

    bindFM [] x (Trie b m) = Trie (Just x) m
    bindFM (k : ks) x (Trie b m) =
      let t = case lookupFM k m of
                Just t -> t
                Nothing -> emptyFM
          t' = bindFM ks x t
      in Trie b (bindFM k t' m)
-}
          
  data Trie mk ks a = Trie (Maybe a) (mk (Trie mk ks a))

  instance FiniteMap m k => FiniteMap (Trie (m k)) [k] where
    emptyFM = Trie Nothing emptyFM

    lookupFM [] (Trie b m) = b
    lookupFM (k : ks) (Trie b m) = lookupFM k m >>= \m' -> lookupFM ks m'

    bindFM [] x (Trie b m) = Trie (Just x) m
    bindFM (k : ks) x (Trie b m) =
      let t = case lookupFM k m of
                Just t -> t
                Nothing -> emptyFM
          t' = bindFM ks x t
      in Trie b (bindFM k t' m)
