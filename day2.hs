{-# OPTIONS_GHC -Wall #-}
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VM
import Util (split, prod)

type Program = V.Vector Int

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = V.fromList input
  -- part 1
  print $ exec (replaceProgram program (12, 2))
  -- part 2
  let possibleInputs = prod [0..99] [0..99]
  let [(a, b)] = filter ((== 19690720) . exec . replaceProgram program) possibleInputs
  print $ 100 * a + b
  return ()

replaceProgram :: Program -> (Int, Int) -> Program
replaceProgram p (a, b) = p V.// [(1, a), (2, b)]

exec :: Program -> Int
exec = (V.! 0) . flip go 0
  where
  go prog i = let
    set j val = V.modify (\v -> VM.write v j val) prog
    get :: Int -> Int -> Int
    get j mode = if mode == 1 then prog V.! j else prog V.! (prog V.! j)
    arg1 = get (i + 1) 0
    arg2 = get (i + 2) 0
    arg3 = get (i + 3) 1
    in case get i 1 of
      1 -> go (set arg3 (arg1 + arg2)) (i + 4)
      2 -> go (set arg3 (arg1 * arg2)) (i + 4)
      99 -> prog
      _ -> error "unknown op"
