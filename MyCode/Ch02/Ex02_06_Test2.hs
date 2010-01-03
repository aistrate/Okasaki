import Test.QuickCheck
import Text.Printf
import Data.List
import TestHelper
import Ex02_06


main  = printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests


tests = [ 
  ("(Int, Double)", verboseCheck (testBindLookupFM::[(Int, Float)] -> Bool)) ]


testBindLookupFM ps = let rs = nubBy (\(a, _) (b, _) -> a == b) ps
                          m = makeFmap rs
                      in all (\(k, v) -> lookupFM k m == Just v) rs
