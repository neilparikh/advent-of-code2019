{-# OPTIONS_GHC -Wall #-}
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VM
import Util (split)
import Data.List (permutations)

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = V.fromList input :: V.Vector Int
  -- part 1
  print $ maximum . fmap (tryPhase program) . permutations $ [0,1,2,3,4]
  -- part 2
  print $ maximum . fmap (tryPhase program) . permutations $ [5,6,7,8,9]
  return ()

type Program = V.Vector Int

tryPhase :: Program -> [Int] -> Int
tryPhase prog [a, b, c, d, e] = let
  out1 = exec prog (a:(0:out5))
  out2 = exec prog (b:out1)
  out3 = exec prog (c:out2)
  out4 = exec prog (d:out3)
  out5 = exec prog (e:out4) in last out5
tryPhase _ _ = error "phase list invalid"

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
exec p input' = go p input' 0
  where
  go :: Program -> [Int] -> Int ->[Int]
  go prog input i = let
    set :: Int -> Int -> Program
    set j val = V.modify (\v -> VM.write v j val) prog
    get :: Int -> Int -> Int
    get j mode = if mode == 1 then prog V.! j else prog V.! (prog V.! j)
    [opcode, _, mode2, mode1] = parseInsn (get i 1)
    arg1 = get (i + 1) mode1
    arg2 = get (i + 2) mode2
    arg3 = get (i + 3) 1
    in case opcode of
      1 -> go (set arg3 (arg1 + arg2)) input (i + 4)
      2 -> go (set arg3 (arg1 * arg2)) input (i + 4)
      3 -> let
        dest = get (i + 1) 1
        newProg = set dest (head input)
        in go newProg (tail input) (i + 2)
      4 -> arg1 : go prog input (i + 2)
      5 -> if arg1 == 0 then go prog input (i+3) else go prog input arg2
      6 -> if arg1 /= 0 then go prog input (i+3) else go prog input arg2
      7 -> go (set arg3 (if arg1 < arg2 then 1 else 0)) input (i + 4)
      8 -> go (set arg3 (if arg1 == arg2 then 1 else 0)) input (i + 4)
      99 -> []
      _ -> error "Unknown op"
