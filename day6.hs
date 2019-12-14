{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
import Util (split, listToTuple, allLines)
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Data.List (unfoldr)

newtype Len a = Len { unLen :: Int } deriving Num
safeLen :: [a] -> Len a
safeLen = Len . length

main :: IO ()
main = do
  input <- fmap (M.fromList . fmap (swap . listToTuple . split ')')) allLines
  -- part 1
  print $ numOrbits input
  -- part 2
  print $ pathLength input
  return ()

pathLength :: M.Map String String -> Int
pathLength m = let
  youPath = pathToRoot m "YOU"
  sanPath = pathToRoot m "SAN"
  in go youPath sanPath
  where
  go :: [String] -> [String] -> Int
  go (x:xs) (y:ys)
    | x == y = go xs ys
    | otherwise = unLen (safeLen xs + safeLen ys)
  go _ _ = error "should not happen"

pathToRoot :: M.Map String String -> String -> [String]
pathToRoot m = reverse . unfoldr (\k -> fmap (k,) (M.lookup k m))

numOrbits :: M.Map String String -> Int
numOrbits m = sum $ fmap (length . pathToRoot m) (M.keys m)
