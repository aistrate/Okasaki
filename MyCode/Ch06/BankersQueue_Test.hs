import Test.HUnit
import Test.QuickCheck
import Text.Printf (printf)
import Timer
import Prelude hiding (head, tail)
import BankersQueue


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  testQueue "" ~? "empty",
  testQueue "a" ~? "length 1",
  testQueue "ab" ~? "length 2",
  testQueue "abc" ~? "length 3",
  testQueue ['a'..'z'] ~? "small"
  ]

qcheckTests = [ 
  ("random", qcheck (testQueue::[Int] -> Bool)) ]

qcheck = --quickCheck
         --verboseCheck
         check $ defaultConfig { configMaxTest = 1000 }


testQueue xs = toList (fromList' xs) == xs
  where fromList' :: [a] -> BankersQueue a
        fromList' = fromList
