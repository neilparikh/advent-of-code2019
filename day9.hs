{-# OPTIONS_GHC -Wall #-}
import Util (split)
import Intcode (exec, toProgram)
import Control.Monad (unless)

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = toProgram input
  -- part 1
  let output1 = exec program [1]
  unless (length output1 == 1) (error "day 9 part 1 failed")
  print $ head output1
  -- part 2
  let output2 = exec program [2]
  unless (length output2 == 1) (error "day 9 part 2 failed")
  print $ head output2
