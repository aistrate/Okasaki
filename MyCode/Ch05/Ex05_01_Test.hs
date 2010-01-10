import Test.HUnit
import qualified Test.QuickCheck as QC
import Text.Printf (printf)
import TestHelper
import Prelude hiding (head, tail, last, init)
import Ex05_01


main = do printTime $ runTestTT hunitTests
          printTime $ mapM_ (\(s,a) -> printf "%-25s: " s >> a) qcheckTests

hunitTests = TestList [
  testDeque "" ~? "empty",
  testDeque "a" ~? "length 1",
  testDeque "ab" ~? "length 2",
  testDeque "abc" ~? "length 3",
  testDeque ['a'..'z'] ~? "small",
  testDeque [1..1000] ~? "medium"
  ]

qcheckTests = [ 
  ("one end", qcheck (testDeque :: [Int] -> Bool)), 
  ("both ends", qcheck (testDeque2 :: ([Int], [Int]) -> Bool))
  ]

qcheck :: QC.Testable a => a -> IO ()
qcheck = --QC.quickCheck
         --QC.verboseCheck
         QC.check $ QC.defaultConfig { QC.configMaxTest = 500 }


testDeque xs = let fs = list2Front' xs
                   rs = list2Rear' xs
               in front2List fs == reverse xs && rear2List fs == xs &&
                  front2List rs == xs && rear2List rs == reverse xs

list2Front', list2Rear' :: [a] -> BatchedDeque a
list2Front' = list2Front
list2Rear' = list2Rear


testDeque2 (xs, ys) = let frs = foldl snoc (list2Front' xs) ys
                          rfs = foldl (flip cons) (list2Rear' ys) xs
                          es = reverse xs ++ ys
                      in front2List frs == es && rear2List frs == reverse es &&
                         front2List rfs == es && rear2List rfs == reverse es
