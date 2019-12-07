import Data.List (group, sort)
import Util (split)

main = do
  [a, b] <- fmap (fmap read . split '-') getLine :: IO [Int]
  print . length . filter isValid1 $ map show [a..b]
  print . length . filter isValid2 $ map show [a..b]
  return ()

isValid1 :: String -> Bool
isValid1 x = isNotDecreasing x && hasAtLeastTwoDouble x

isValid2 :: String -> Bool
isValid2 x = isNotDecreasing x && hasExactlyTwoDouble x

isNotDecreasing :: String -> Bool
isNotDecreasing x = sort x == x

hasAtLeastTwoDouble :: String -> Bool
hasAtLeastTwoDouble = common (>= 2)

hasExactlyTwoDouble :: String -> Bool
hasExactlyTwoDouble = common (== 2)

common :: (Int -> Bool) -> String -> Bool
common f = (not . null) . filter (f . length) . group
