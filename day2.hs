import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VM
import Util (split, prod)

type Program = V.Vector Int

main = do
  input <- fmap (fmap read . split ',') getContents
  let program = V.fromList input
  -- part 1
  print $ exec (replaceProgram program (12, 2))
  -- part 2
  let possibleInputs = prod [0..99] [0..99]
  let (a, b) = head . filter ((== 19690720) . exec . replaceProgram program) $ possibleInputs
  print $ 100 * a + b
  return ()

replaceProgram :: Program -> (Int, Int) -> Program
replaceProgram p (a, b) = p V.// [(1, a), (2, b)]

exec :: Program -> Int
exec = (V.! 0) . flip go 0
  where
  go prog i = case prog V.! i of
    1 -> go (newProg (a + b)) (i + 4)
    2 -> go (newProg (a * b)) (i + 4)
    99 -> prog
    where
    a = prog V.! (prog V.! (i + 1))
    b = prog V.! (prog V.! (i + 2))
    cIndex = prog V.! (i + 3)
    newProg newVal = V.modify (\v -> VM.write v cIndex newVal) prog
