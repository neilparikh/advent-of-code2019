{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Util (split, listToTuple, allLines)
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Data.Maybe (catMaybes, isJust)

newtype Len a = Len { unLen :: Int } deriving Num
safeLen :: [a] -> Len a
safeLen = Len . length

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
pathToRoot m = reverse . catMaybes . takeWhile isJust . iterate ((`M.lookup` m) =<<) . Just

numOrbits :: M.Map String String -> Int
numOrbits m = go (M.keys m) 0
  where
  go :: [String] -> Int -> Int
  go [] acc = acc
  go keys acc = let
    newKeys = filter (/= "COM") . catMaybes . fmap (`M.lookup` m) $ keys in go newKeys (acc + length keys)
