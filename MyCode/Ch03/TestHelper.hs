module TestHelper (printTime) where

import System.Time
import Text.Printf


printTime :: IO a -> IO a
printTime f = do t1 <- getClockTime
                 r <- f
                 t2 <- getClockTime
                 let td = diffClockTimes t2 t1
                     sec = fromIntegral (tdSec td) +
                           fromIntegral (tdPicosec td) / 1e12 :: Double
                 printf "Time: %.6f sec.\n" sec
                 return r
