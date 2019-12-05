import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VM
import Util
import Control.Monad (unless)
import Debug.Trace (trace)

type Program = V.Vector Int

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

exec :: Program -> [Int] -> [Int]
exec p input = go p input [] 0
  where
  go prog input output i = let
    set j val = V.modify (\v -> VM.write v j val) prog
    get j mode = if mode == 1 then prog V.! j else prog V.! (prog V.! j)
    (opcode:mode3:mode2:mode1:xs) = parseInsn (get i 1)
    arg1 = get (i + 1) mode1
    arg2 = get (i + 2) mode2
    arg3 = get (i + 3) 1
    in case opcode of
      1 -> go (set arg3 (arg1 + arg2)) input output (i + 4)
      2 -> go (set arg3 (arg1 * arg2)) input output (i + 4)
      3 -> let
        dest = get (i + 1) 1
        newProg = set dest (head input)
        in go newProg (tail input) output (i + 2)
      4 -> go prog input (arg1:output) (i + 2)
      5 -> if arg1 == 0 then go prog input output (i+3) else go prog input output arg2
      6 -> if arg1 /= 0 then go prog input output (i+3) else go prog input output arg2
      7 -> go (set arg3 (if arg1 < arg2 then 1 else 0)) input output (i + 4)
      8 -> go (set arg3 (if arg1 == arg2 then 1 else 0)) input output (i + 4)
      99 -> output

main = do
  input <- fmap (fmap read . split ',') getContents
  let program = V.fromList input :: V.Vector Int
  -- part 1
  let ouput1 = exec program [1]
  unless (all (== 0) (tail ouput1)) (error "part 1 failed")
  print . head $ ouput1
  -- part 2
  let output2 = exec program [5]
  unless (length output2 == 1) (error "part 2 failed")
  print . head $ output2
  return ()
