import Util
import Data.List (minimumBy)
import Data.Function (on)

main = do
  wire1 <- fmap (fmap parseOne . split ',') getLine
  wire2 <- fmap (fmap parseOne . split ',') getLine
  let a = pointsForWire wire1 (0, 0) 0
  let b = pointsForWire wire2 (0, 0) 0
  -- part 1
  print . minimumBy (compare `on` fst) . fmap op $ intersectBy fst const a b
  -- part 2
  print . minimumBy (compare `on` snd) . fmap op $ intersectBy fst (\(a, x) (b, y) -> (a, x + y)) a b
  return ()

op ((x, y), s) = (x + y, s)

parseOne :: String -> (Char, Int)
parseOne (dir:num) = (dir, read num)

pointsForWire :: [(Char, Int)] -> (Int, Int) -> Int -> [((Int, Int), Int)]
pointsForWire [] _ _ = []
pointsForWire ((dir, num):xs) (x, y) steps = case dir of
  'U' -> fmap (\i -> ((x, y + i), steps + i)) [1..num] ++ rest
  'D' -> fmap (\i -> ((x, y - i), steps + i)) [1..num] ++ rest
  'R' -> fmap (\i -> ((x + i, y), steps + i)) [1..num] ++ rest
  'L' -> fmap (\i -> ((x - i, y), steps + i)) [1..num] ++ rest
  where
  rest = case dir of
    'U' -> pointsForWire xs (x, y + num) (steps + num)
    'D' -> pointsForWire xs (x, y - num) (steps + num)
    'R' -> pointsForWire xs (x + num, y) (steps + num)
    'L' -> pointsForWire xs (x - num, y) (steps + num)
