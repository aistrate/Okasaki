import Test.HUnit
import Timer
import Prelude hiding (head, tail)
import Queue
import qualified BankersQueue as BQ1
import qualified Ex06_02 as BQ2


main = do let xs = [1..100000]
          putStrLn "snocs/tails:"
          printTime . print $ testSnocsTails (empty :: BQ1.BankersQueue Int) xs
          printTime . print $ testSnocsTails (empty :: BQ2.BankersQueue Int) xs
          putStrLn "snocs/heads/tails:"
          printTime . print $ toList (bq1FromList xs) == xs
          printTime . print $ toList (bq2FromList xs) == xs
-- snocs/tails:
-- True
-- Time: 0.736938 sec.
-- True
-- Time: 0.797520 sec.
-- snocs/heads/tails:
-- True
-- Time: 0.748294 sec.
-- True
-- Time: 0.837748 sec.


testSnocsTails :: Queue q => q a -> [a] -> Bool
testSnocsTails emp xs = let q1 = foldl snoc emp xs
                            q2 = last . take (length xs + 1) $ iterate tail q1
                        in isEmpty q2


bq1FromList :: [a] -> BQ1.BankersQueue a
bq1FromList = fromList

bq2FromList :: [a] -> BQ2.BankersQueue a
bq2FromList = fromList
