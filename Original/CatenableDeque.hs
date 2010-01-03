-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
--
-- Copyright (c) 1998 Cambridge University Press

module CatenableDeque (module Deque,CatenableDeque(..)) where
  import Prelude hiding (head,tail,last,init,(++))
  import Deque

  class Deque q => CatenableDeque q where
    (++) :: q a -> q a -> q a
