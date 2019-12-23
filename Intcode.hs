{-# OPTIONS_GHC -Wall #-}
module Intcode (Program, exec, toProgram) where

import qualified Data.IntMap.Strict as IM

-- It might be more efficient to use IM.IntMap (V.Vector Int)
-- ie. chunks of vectors
-- so lookup within a chunk is fast
type Program = IM.IntMap Int

-- memory, input, pc, base
data Machine = Running Program [Int] Int Int
             | Stopped Program

toProgram :: [Int] -> Program
toProgram = IM.fromList . zip [0..]

parseInsn :: Int -> [Int]
parseInsn i = [i `mod` 100, (i `mod` 100000) `div` 10000, (i `mod` 10000) `div` 1000, (i `mod` 1000) `div` 100]

step :: Machine -> (Machine, Maybe Int)
step (Running prog input i base) = let
  set :: Int -> Int -> Program
  set j val = if j < 0 then error "negative index to set" else IM.insert j val prog
  get :: Int -> Int
  get j = IM.findWithDefault 0 j prog
  getR j mode = if j < 0 then error "negative index to getR" else case mode of
    0 -> get (get j)
    1 -> get j
    2 -> get (get j + base)
    _ -> error "unknown mode"
  getW j mode = if j < 0 then error "negative index to getW" else case mode of
    0 -> get j
    1 -> error "invalid mode for getW"
    2 -> get j + base
    _ -> error "unknown mode"
  [opcode, mode3, mode2, mode1] = parseInsn (get i)
  arg1 = getR (i + 1) mode1
  arg2 = getR (i + 2) mode2
  arg3 = getW (i + 3) mode3
  in case opcode of
    1 -> (Running (set arg3 (arg1 + arg2)) input (i + 4) base, Nothing)
    2 -> (Running (set arg3 (arg1 * arg2)) input (i + 4) base, Nothing)
    3 -> let
      dest = getW (i + 1) mode1
      newProg = set dest (head input)
      in (Running newProg (tail input) (i + 2) base, Nothing)
    4 -> (Running prog input (i + 2) base, Just arg1)
    5 -> (Running prog input (if arg1 == 0 then i+3 else arg2) base, Nothing)
    6 -> (Running prog input (if arg1 /= 0 then i+3 else arg2) base, Nothing)
    7 -> (Running (set arg3 (if arg1 < arg2 then 1 else 0)) input (i + 4) base, Nothing)
    8 -> (Running (set arg3 (if arg1 == arg2 then 1 else 0)) input (i + 4) base, Nothing)
    9 -> (Running prog input (i + 2) (base + arg1), Nothing)
    99 -> (Stopped prog, Nothing)
    _ -> error "Unknown op"
step (Stopped p) = (Stopped p, Nothing)

exec :: Program -> [Int] -> [Int]
exec p input' = go (Running p input' 0 0)
  where
  go :: Machine -> [Int]
  go m = case step m of
    (Stopped _, Just _) -> error "Should not happen"
    (Stopped _, Nothing) -> []
    (m'@Running{}, Nothing) -> go m'
    (m'@Running{}, Just newOut) -> newOut : go m'
