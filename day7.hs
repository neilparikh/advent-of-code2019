{-# OPTIONS_GHC -Wall #-}
import Util (split)
import Data.List (permutations)
import Intcode (Program, exec, toProgram)

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = toProgram input
  -- part 1
  print $ maximum . fmap (tryPhase program) . permutations $ [0,1,2,3,4]
  -- part 2
  print $ maximum . fmap (tryPhase program) . permutations $ [5,6,7,8,9]
  return ()

tryPhase :: Program -> [Int] -> Int
tryPhase prog [a, b, c, d, e] = let
  out1 = exec prog (a:(0:out5))
  out2 = exec prog (b:out1)
  out3 = exec prog (c:out2)
  out4 = exec prog (d:out3)
  out5 = exec prog (e:out4) in last out5
tryPhase _ _ = error "phase list invalid"
