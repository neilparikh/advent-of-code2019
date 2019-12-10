{-# LANGUAGE TupleSections #-}
import Text.Parsec hiding (parse)
import Util
import Data.Maybe
import Data.Ratio
import Debug.Trace
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow ((&&&))

maxX = 10
maxY = 10

a (ax, ay) (bx, by) = (ax + bx, ay + by)

main = do
  input <- fmap lines getContents
  let pts = findAll $ zip [0..] (fmap (zip [0..]) input)
  let maxX = length (head input)
  let maxY = length input
  let pts_set = S.fromList pts
  let allPaths = nub $ reduce <$> prod [(-maxX)..maxX] [(-maxY)..maxY]
  let forP p = sortOnMultiple [snd, fst] . filter (/= p) . nub . catMaybes $ fmap (\dir -> walkPath (maxX, maxY) pts_set (a dir p) dir) allPaths
  print $ maximum . fmap (length . forP) $ pts
  return ()

walkPath :: (Int, Int) -> Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
walkPath m@(maxX, maxY) allPts (x, y) (dx, dy)
  | x < 0 || x > maxX || y < 0 || y > maxY = Nothing
  | (x, y) `S.member` allPts = Just (x, y)
  | otherwise = walkPath m allPts (x + dx, y + dy) (dx, dy)

reduce :: (Int, Int) -> (Int, Int)
reduce (0, 0) = (0, 0)
reduce (x, 0) = (signum x, 0)
reduce (0, y) = (0, signum y)
reduce frac@(x, y) = ((s x . numerator) &&& (s y . denominator)) . uncurry (%) $ frac
  where
  s x a
    | x > 0 && a > 0 = a
    | x < 0 && a < 0 = a
    | otherwise = -a

findAll :: [(Int, [(Int, Char)])] -> [(Int, Int)]
findAll [] = []
findAll ((y, row):xs) = let
  rowPts = fmap fst . filter ((== '#') . snd) $ row
  in fmap (,y) rowPts ++ findAll xs
