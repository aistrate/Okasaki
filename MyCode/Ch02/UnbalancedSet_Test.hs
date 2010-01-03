import Test.HUnit
import TestHelper
--import System.Random
--import UnbalancedSet
--import Ex02_02
--import Ex02_03
--import Ex02_03a
import Ex02_04


--main = do printTime . runTestTT . TestList $ replicate 100 tests
main = do printTime $ runTestTT tests
          return ()


tests = test [
  "membership" ~: test
  [
    member 'a' set1 ~? "'a' member",
    member 'b' set1 ~? "'b' member",
    member 'c' set1 ~? "'c' member",
    member 'd' set1 ~? "'d' member",
    member 'f' set1 ~? "'f' member",
    member 'g' set1 ~? "'g' member",
    member 'h' set1 ~? "'h' member",
    not (member 'e' set1) ~? "'e' not member", 
    not (member 'x' set1) ~? "'x' not member", 
    not (member 'A' set1) ~? "'A' not member"
  ],
  
  "inserting" ~: test
  [
    insert 'e' set1 ~?= T (T (T E 'a' E) 'b' (T E 'c' E))
                          'd'
                          (T (T (T E 'e' E) 'f' E) 'g' (T E 'h' E)),
    insert 'i' set1 ~?= T (T (T E 'a' E) 'b' (T E 'c' E))
                          'd'
                          (T (T E 'f' E) 'g' (T E 'h' (T E 'i' E))), 
    member 'e' (insert 'e' set1) ~? "'e' member after insert", 
    member 'k' (insert 'k' set1) ~? "'k' member after insert", 
    insert 'x' E    ~?= T E 'x' E,
    insert 'a' set1 ~?= set1,
    insert 'b' set1 ~?= set1,
    insert 'c' set1 ~?= set1,
    insert 'd' set1 ~?= set1,
    insert 'f' set1 ~?= set1,
    insert 'g' set1 ~?= set1,
    insert 'h' set1 ~?= set1, 
    insert 'z' set2 ~?= set2, 
    insert 500 set3 ~?= set3,
    insert 2100 set4 ~?= set4,
    insert 15157 set4 ~?= set4
  ]
  ]


set1 = T (T (T E 'a' E) 'b' (T E 'c' E)) 'd' (T (T E 'f' E) 'g' (T E 'h' E))

set2 = insertList (['A'..'Z'] ++ ['a'..'z']) E

set3 = insertList numberList1 E

set4 = insertList numberList2 E


insertList :: Ordered a => [a] -> UnbalancedSet a -> UnbalancedSet a
insertList [] t = t
insertList (x:xs) t = insertList xs (insert x t)

numberList1 = [
  700,594,122,402,782,49,19,187,349,175,216,546,966,849,659,723,36,558,477,289,
  868,932,398,203,206,330,621,23,235,826,604,353,584,178,784,252,206,524,408,966,
  665,945,522,702,348,845,423,466,594,104,991,57,238,87,272,15,33,626,865,555,
  109,706,893,99,270,411,439,807,862,104,348,942,96,769,832,473,306,92,482,946,
  575,949,161,438,79,588,915,456,976,74,886,31,727,522,86,377,105,881,463,87,
  412,415,372,828,663,500,136,827,499,787,903,456,430,408,348,807,900,145,632,577,
  147,100,632,830,145,883,642,169,202,417,980,390,827,864,574,233,335,323,588,37,
  262,396,505,4,935,988,556,691,946,270,567,18,232,284,797,462,683,451,639,313,
  989,743,551,650,447,63,212,254,351,471,11,654,601,700,822,441,543,786,943,908,
  766,824,793,580,929,319,910,315,103,365,300,254,967,919,159,207,48,98,566,218]

numberList2 = map (*3) numberList1 ++
              map (*17) numberList1 ++
              map (*5) numberList1 ++
              map (*11) numberList1 ++
              map (*23) numberList1 ++
              map (*7) numberList1 ++
              map (*31) numberList1 ++
              map (*1) numberList1 ++
              map (*29) numberList1 ++
              map (*13) numberList1

-- gen = do g <- newStdGen
--          let rs = randomRs (1, 1000) g :: [Int]
--          print (take 20 rs)

depth :: Ordered a => UnbalancedSet a -> Int
depth E = 0
depth (T a x b) = 1 + max (depth a) (depth b)

count :: Ordered a => UnbalancedSet a -> Int
count E = 0
count (T a x b) = 1 + count a + count b

memberDepth :: Ordered a => a -> UnbalancedSet a -> (Bool, Int)
memberDepth x E = (False, 0)
memberDepth x (T a y b) =
    if x `lt` y then let (found, d) = memberDepth x a
                     in (found, d + 1)
    else if y `lt` x then let (found, d) = memberDepth x b
                          in (found, d + 1)
         else (True, 1)
