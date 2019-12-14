{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
  input <- fmap (fmap read . lines) getContents
  -- part 1
  print . sum . fmap fuel $ input
  -- part 2
  print . sum . fmap fuelRecur $ input
  return ()

fuel :: Int -> Int
fuel i = (i `div` 3) - 2

fuelRecur :: Int -> Int
fuelRecur (fuel -> i) = if i < 0 then 0 else i + fuelRecur i
