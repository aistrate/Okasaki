import Test.HUnit
import Text.Printf
import TestHelper
import UnbalancedSet
import Ex02_05


main = do printTime $ runTestTT tests
          return ()


tests = test [
  "completeTree" ~: map testCompleteTree [0, 1, 2, 3, 4, 9, 12],
  "completeTree/large" ~: depthRight (completeTree 'x' 100000) ~?= 100000,
  
  "balancedTree" ~: map testBalancedTree [0, 1, 2, 3, 4, 5, 6, 7, 10, 23, 37, 43, 
                                          101, 200, 881, 1111, 8761],
  "balancedTree/large" ~: let n = 10^300
                              d = ceiling (logBase 2 $ fromIntegral n)
                          in depthRight (balancedTree 'x' n) ~?= d
  ]


testCompleteTree d = printf "(depth=%d)" d ~:
                     testTree (2^d - 1) (completeTree 'e' d)

testBalancedTree n = printf "(n=%d)" n ~:
                     testTree n (balancedTree 'e' n)

testTree :: Integer -> Tree a -> Test
testTree n t = test [ "countNodes" ~: countNodes t ~?= n,
                      isBalanced t ~? "isBalanced failed" ]

depthRight E = 0
depthRight (T _ _ r) = 1 + depthRight r
