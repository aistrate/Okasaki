-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module TrieOfTrees (module FiniteMap, Tree (..), Trie) where
  import FiniteMap

  data Tree a = E | T a (Tree a) (Tree a)

  data Trie m k ks a = Trie (Maybe a) (m k (Trie m k ks (Trie m k ks a)))

  instance FiniteMap m k => FiniteMap (Trie m k) (Tree k) where
    emptyFM = Trie Nothing emptyFM

    lookupFM E (Trie v m) = v
    lookupFM (T k a b) (Trie v m) = 
      lookupFM k m >>= \m' -> 
      lookupFM a m' >>= \m'' ->
      lookupFM b m''

    bindFM E x (Trie v m) = Trie (Just x) m
    bindFM (T k a b) x (Trie v m) =
      let tt = case lookupFM k m of
                 Just tt -> tt
                 Nothing -> emptyFM
          t = case lookupFM a tt of
                Just t -> t
                Nothing -> emptyFM
          t' = bindFM b x t
          tt' = bindFM a t' tt
      in Trie v (bindFM k tt' m)


          
