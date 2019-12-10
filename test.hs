{-# OPTIONS_GHC -Wall #-}

import System.Process (readProcess)
import Control.Monad (unless)
import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime)

main :: IO ()
main = do
  times <- mapM testDayN [1..10]
  putStrLn $ "Total time is " ++ show (sum times)
  return ()

testDayN :: Int -> IO NominalDiffTime
testDayN n = do
  let hsName = "day" ++ show n ++ ".hs"
  _ <- readProcess "ghc" [hsName] ""
  let inName = "io/day" ++ show n ++ ".in"
  let outName = "io/day" ++ show n ++ ".out"
  input <- readFile inName
  expectedOutput <- readFile outName

  start <- getCurrentTime
  realOutput <- readProcess ("./day" ++ show n) [] input
  end <- getCurrentTime

  unless (expectedOutput == realOutput) (error $ "Day " ++ show n ++ " failed.")

  let timeElapsed = end `diffUTCTime` start
  putStrLn $ "Day " ++ show n ++ " passed in " ++ show timeElapsed
  return timeElapsed
