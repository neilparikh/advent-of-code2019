{-# OPTIONS_GHC -Wall #-}
module Intcode (Program, exec, toProgram) where

import qualified Data.IntMap.Strict as IM

-- It might be more efficient to use IM.IntMap (V.Vector Int)
-- ie. chunks of vectors
-- so lookup within a chunk is fast
type Program = IM.IntMap Int

toProgram :: [Int] -> Program
toProgram = IM.fromList . zip [0..]

parseInsn :: Int -> [Int]
parseInsn i = opcode : map (read . return) modes
  where
  s = show i
  opcode = read $ drop (length s - 2) s
  modes = case take (length s - 2) s of
    [mode3, mode2, mode1] -> [mode3, mode2, mode1]
    [mode2, mode1] -> ['0', mode2, mode1]
    [mode1] -> ['0', '0', mode1]
    [] -> "000"
    _ -> error "error parsing insn"

exec :: Program -> [Int] -> [Int]
exec p input' = go p input' 0 0
  where
  go :: Program -> [Int] -> Int -> Int -> [Int]
  go prog input i base = let
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
      1 -> go (set arg3 (arg1 + arg2)) input (i + 4) base
      2 -> go (set arg3 (arg1 * arg2)) input (i + 4) base
      3 -> let
        dest = getW (i + 1) mode1
        newProg = set dest (head input)
        in go newProg (tail input) (i + 2) base
      4 -> arg1 : go prog input (i + 2) base
      5 -> go prog input (if arg1 == 0 then i+3 else arg2) base
      6 -> go prog input (if arg1 /= 0 then i+3 else arg2) base
      7 -> go (set arg3 (if arg1 < arg2 then 1 else 0)) input (i + 4) base
      8 -> go (set arg3 (if arg1 == arg2 then 1 else 0)) input (i + 4) base
      9 -> go prog input (i + 2) (base + arg1)
      99 -> []
      _ -> error "Unknown op"
