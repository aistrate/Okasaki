module Ex02_05 where

import UnbalancedSet


completeTree :: a -> Integer -> Tree a
completeTree _ 0 = E
completeTree x n = let t = completeTree x (n - 1)
                   in T t x t


balancedTree :: a -> Integer -> Tree a
balancedTree _ 0 = E
balancedTree x n = let (q, r) = quotRem n 2
                   in if r == 1 then let t = balancedTree x q 
                                     in T t x t
                      else T (balancedTree x (q - 1)) x (balancedTree x q)
   --                    else let (l, r) = createTwo x (q - 1)
   --                         in T l x r
   -- where createTwo x m = (balancedTree x m, balancedTree x (m + 1))


countNodes :: Tree a -> Integer
countNodes E = 0
countNodes (T a x b) = 1 + countNodes a + countNodes b


isBalanced :: Tree a -> Bool
isBalanced E = True
isBalanced (T a x b) = abs (countNodes a - countNodes b) <= 1 &&
                       isBalanced a && isBalanced b



printTree :: (a -> String) -> Tree a -> IO ()
printTree showF = printTree' 0
  where printTree' _ E = return ()
        printTree' n (T a x b) = do printTree' (n + 1) a
                                    putStrLn $ (replicate (1*n) ' ') ++ showF x
                                    printTree' (n + 1) b

printBalancedTree = printTree (:[]) . balancedTree 'x'


-- partial results are NOT cached
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))
