import Util (count, chunksOf)
import Data.List (minimumBy, transpose)
import Data.Ord (comparing)
import Data.Function (on)

width :: Int
width = 25

height :: Int
height = 6

main = do
  layers <- fmap (chunksOf (width * height)) getLine
  -- part 1
  print $ part1 layers
  -- part 2
  putStrLn $ toImage layers
  return ()

part1 :: [String] -> Int
part1 layers = let leastZero = minimumBy (comparing (count '0')) layers
               in ((*) `on` ($ leastZero)) (count '1') (count '2')

toImage :: [String] -> String
toImage = unlines . chunksOf width . fmap (drawPixel . getPixel) . transpose
  where
  getPixel = head . dropWhile (== '2')
  drawPixel '1' = '1'
  drawPixel  _  = ' '
