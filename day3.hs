import Util (intersectBy, split)
import Data.List (minimum)

main = do
  wire1 <- fmap (concatMap parseOne . split ',') getLine
  wire2 <- fmap (concatMap parseOne . split ',') getLine
  let a = points wire1
  let b = points wire2
  -- part 1
  print . minimum . fmap (op . fst) $ intersectBy fst const a b
  -- part 2
  print . minimum . fmap snd $ intersectBy fst (\(a, x) (b, y) -> (a, x + y)) a b
  return ()

op (x, y) = x + y

parseOne :: String -> String
parseOne (dir:num) = replicate (read num) dir

points :: String -> [((Int, Int), Int)]
points = (`zip` [1..]) . (`pointsForWire` (0, 0))

pointsForWire :: String -> (Int, Int) -> [(Int, Int)]
pointsForWire [] _ = []
pointsForWire (dir:xs) (x, y) = newPoint : pointsForWire xs newPoint
  where
  newPoint = case dir of
    'U' -> (x, y + 1)
    'D' -> (x, y - 1)
    'R' -> (x + 1, y)
    'L' -> (x - 1, y)
