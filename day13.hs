{-# OPTIONS_GHC -Wall #-}
import Util (split, chunksOf)
import Intcode (exec, toProgram, Program)
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = toProgram input
  -- part 1
  let output = exec program []
  print $ length . filter (== 2) . fmap snd . M.toList . foldl insertCell M.empty . chunksOf 3 $ output
  -- part 2
  let program' = IM.insert 0 2 program
  print $ solve program'
  return ()

solve :: Program -> Int
solve program = let
  input = computeInput output 0 0
  output = chunksOf 3 $ exec program input
  in last input

computeInput :: [[Int]] -> Int -> Int -> [Int]
computeInput []  _ score = [score]
computeInput ([x, _, 4]:xs) paddleX score = dir : computeInput xs paddleX score  -- ball update
  where
  dir = signum (x - paddleX)
computeInput ([-1, 0, score]:xs) p _ = computeInput xs p score
computeInput ([x, _, 3]:xs) _ score = computeInput xs x score  -- paddle update
computeInput (_:xs) b c = computeInput xs b c

insertCell :: M.Map (Int, Int) Int -> [Int] -> M.Map (Int, Int) Int
insertCell m [a, b, c] = M.insert (a, b) c m
insertCell _ _ = error "should not happen"
