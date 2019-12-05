import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VM
import Util
import Control.Monad (unless)

type Program = V.Vector Int

parseInsn :: Int -> [Int]
parseInsn i = opcode : map (read . return) modes
  where
  s = show i
  opcode = read $ drop (length s - 2) s
  modes = case take (length s - 2) s of
    [x, y, z] -> [x, y, z]
    [y, z] -> ['0', y, z]
    [z] -> ['0', '0', z]
    [] -> "000"

exec :: Program -> [Int] -> [Int]
exec p input = go p input [] 0
  where
  go prog input output i = case prog V.! i of
    3 -> let
      dest = prog V.! (i+1)
      newProg = V.modify (\v -> VM.write v dest (head input)) prog
      in go newProg (tail input) output (i + 2)
    99 -> output
    insn -> let
      (opcode:x:y:z:xs) = parseInsn insn
      cIndex = prog V.! (i + 3)
      a' = prog V.! (i + 1)
      a  = if z == 1 then a' else prog V.! a'
      b' = prog V.! (i + 2)
      b  = if y == 1 then b' else prog V.! b'
      newProg newVal = V.modify (\v -> VM.write v cIndex newVal) prog
      cond' = prog V.! (i+1)
      cond = if z == 1 then cond' else prog V.! cond'
      ip' = prog V.! (i+2)
      ip = if y == 1 then ip' else prog V.! ip'
      in case opcode of 
        1 -> go (newProg (a + b)) input output (i + 4)
        2 -> go (newProg (a * b)) input output (i + 4)
        4 -> let
          val' = prog V.! (i+1)
          val = if z == 1 then val' else prog V.! val'
          in go prog input (val:output) (i + 2)
        5 -> if cond == 0 then go prog input output (i+3) else go prog input output ip
        6 -> if cond /= 0 then go prog input output (i+3) else go prog input output ip
        7 -> go (newProg (if a < b then 1 else 0)) input output (i + 4)
        8 -> go (newProg (if a == b then 1 else 0)) input output (i + 4)

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
