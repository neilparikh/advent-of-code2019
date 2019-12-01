import Util

main = do
  input <- allLines
  let input = map read rawInput
  -- part 1
  print . sum . fmap op $ input
  -- part 2
  print . sum . fmap fuel $ input
  return ()

op :: Int -> Int
op i = (i `div` 3) - 2

fuel :: Int -> Int
fuel i = if next < 0 then curr else curr + fuel curr
  where
  curr = op i
  next = op curr
