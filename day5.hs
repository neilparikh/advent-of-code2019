{-# OPTIONS_GHC -Wall #-}
import Util (split)
import Control.Monad (unless)
import Intcode (exec, toProgram)

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = toProgram input
  -- part 1
  let ouput1 = reverse $ exec program [1]
  unless (all (== 0) (tail ouput1)) (error "part 1 failed")
  print . head $ ouput1
  -- part 2
  let output2 = exec program [5]
  unless (length output2 == 1) (error "part 2 failed")
  print . head $ output2
  return ()
