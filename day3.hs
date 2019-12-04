import Util
import Data.List (minimumBy)
import Data.Ord (comparing)

main = do
  wire1 <- fmap (concatMap parseOne . split ',') getLine
  wire2 <- fmap (concatMap parseOne . split ',') getLine
  let a = points wire1
  let b = points wire2
  -- part 1
  print . fst . minimumBy (comparing fst) . fmap op $ intersectBy fst const a b
  -- part 2
  print . snd . minimumBy (comparing snd) . fmap op $ intersectBy fst (\(a, x) (b, y) -> (a, x + y)) a b
  return ()

op ((x, y), s) = (x + y, s)

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
