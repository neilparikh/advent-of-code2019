{-# OPTIONS_GHC -Wall #-}
import Util (split)
import Intcode (exec, toProgram)

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = toProgram input
  -- part 1
  print $ exec program [1]
  -- part 2
  print $ exec program [2]
