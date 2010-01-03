-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module CatList (module CatenableList, Queue, CatList) where
  import Prelude hiding (head,tail,(++))
  import CatenableList
  import Queue (Queue)
  import qualified Queue

  data Queue q => CatList q a = E | C a (q (CatList q a))

  link (C x q) s = C x (Queue.snoc q s)

  instance Queue q => CatenableList (CatList q) where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    xs ++ E = xs
    E ++ xs = xs
    xs ++ ys = link xs ys

    cons x xs = C x Queue.empty ++ xs
    snoc xs x = xs ++ C x Queue.empty

    head E = error "CatList.head: empty list"
    head (C x q) = x

    tail E = error "CatList.tail: empty list"
    tail (C x q) = if Queue.isEmpty q then E else linkAll q
      where linkAll q = if Queue.isEmpty q' then t else link t (linkAll q')
              where t = Queue.head q
                    q' = Queue.tail q
