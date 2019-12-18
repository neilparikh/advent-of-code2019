{-# OPTIONS_GHC -Wall #-}
import Util (applyNTimes)

main :: IO ()
main = do
  input <- fmap (fmap (read . (:[]))) getLine
  -- part 1
  print $ take 8 . applyNTimes 100 step $ input
  return ()

getPattern :: Int -> [Int]
getPattern i = tail . cycle . concatMap (replicate i) $ [0, 1, 0, -1]

step :: [Int] -> [Int]
step input = fmap ((`mod` 10) . abs . ith) [1..(length input)]
  where
  ith i = sum $ zipWith (*) input (getPattern i)
