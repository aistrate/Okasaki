import Test.HUnit
import TestHelper
import UnbalancedSet
import Ex02_06


main = do printTime $ runTestTT tests
          return ()

tests = test [
  "lookupFM" ~: test
  [
    lookupFM 'a' fmap1 ~?= Just 1,
    lookupFM 'b' fmap1 ~?= Just 2,
    lookupFM 'c' fmap1 ~?= Just 3,
    lookupFM 'd' fmap1 ~?= Just 4,
    lookupFM 'f' fmap1 ~?= Just 6,
    lookupFM 'g' fmap1 ~?= Just 7,
    lookupFM 'h' fmap1 ~?= Just 8,
    lookupFM 'e' fmap1 ~?= Nothing,
    lookupFM 'x' fmap1 ~?= Nothing,
    lookupFM 'A' fmap1 ~?= Nothing
  ],
  
  "bindFM" ~: test
  [
    lookupFM 'x' (bindFM 'x' 101 emptyFmap1) ~?= Just 101,
    lookupFM 'y' (bindFM 'x' 101 emptyFmap1) ~?= Nothing,
    lookupFM 'e' (bindFM 'e' 5 fmap1) ~?= Just 5,
    lookupFM 'k' (bindFM 'k' 51 fmap1) ~?= Just 51,
    bindFM 'a' 1 fmap1 ~?= fmap1,
    bindFM 'b' 2 fmap1 ~?= fmap1,
    bindFM 'c' 3 fmap1 ~?= fmap1,
    bindFM 'd' 4 fmap1 ~?= fmap1,
    bindFM 'f' 6 fmap1 ~?= fmap1,
    bindFM 'g' 7 fmap1 ~?= fmap1,
    bindFM 'h' 8 fmap1 ~?= fmap1,
    bindFM 'Z' 26 fmap2 ~?= fmap2,
    bindFM 'z' 52 fmap2 ~?= fmap2
  ]]

emptyFmap1 :: UnbalancedMap Char Int
emptyFmap1 = emptyFM

fmap1 = M (T (T (T E ('a', 1) E) ('b', 2) (T E ('c', 3) E)) 
          ('d', 4) 
          (T (T E ('f', 6) E) ('g', 7) (T E ('h', 8) E)))

fmap2 :: UnbalancedMap Char Int
fmap2 = let keys = ['A'..'Z'] ++ ['a'..'z']
            vals = [1..length keys]
        in makeFmap $ zip keys vals
