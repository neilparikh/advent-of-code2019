{-# OPTIONS_GHC -Wall #-}

import System.Process (readProcess)
import Control.Monad (unless)

main :: IO ()
main = do
  mapM_ testDayN [1..9]
  return ()

testDayN :: Int -> IO ()
testDayN n = do
  let hsName = "day" ++ show n ++ ".hs"
  let inName = "io/day" ++ show n ++ ".in"
  let outName = "io/day" ++ show n ++ ".out"
  input <- readFile inName
  expectedOutput <- readFile outName
  realOutput <- readProcess "runhaskell" [hsName] input
  unless (expectedOutput == realOutput) (error $ "Day " ++ show n ++ " failed.")
  putStrLn $ "Day " ++ show n ++ " passed."
