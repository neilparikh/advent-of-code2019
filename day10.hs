{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf, TupleSections #-}
import Util (prod, filterMap)
import Data.Maybe (catMaybes)
import Data.Ratio ((%), numerator, denominator, Ratio)
import Data.List (nub, maximumBy, sortBy)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.Monoid ((<>))

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (ax, ay) (bx, by) = (ax + bx, ay + by)

main :: IO ()
main = do
  input <- getContents
  let pts = parse input
  let maxX = maximum $ fmap fst pts
  let maxY = maximum $ fmap snd pts
  let pts_set = S.fromList pts
  let allPaths = nub $ reduce <$> prod [(-maxX)..maxX] [(-maxY)..maxY]
  let forP p = filter (/= p) . nub . catMaybes $ fmap (\dir -> walkPath (maxX, maxY) pts_set (move dir p) dir) allPaths
  let (index, a) = maximumBy (comparing (length . snd)) $ zip [0..] (fmap forP pts)
  print $ length a
  let sourcePoint = pts !! index
  let (x, y) = (!! 199) $ sortBy (sorter sourcePoint) a
  print $ x * 100 + y
  return ()

sorter :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
sorter o a b = comparing (quad o) a b <> comparing (slope o) a b

quad :: (Int, Int) -> (Int, Int) -> Int
quad (ax, ay) (bx, by) = let x = bx - ax
                             y = by - ay
                         in if | x >= 0 && y < 0 -> 1
                               | y >= 0 && x > 0 -> 2
                               | x <= 0 && y > 0 -> 3
                               | otherwise -> 4

slope :: (Int, Int) -> (Int, Int) -> Ratio Int
slope (ax, ay) (bx, by)
  | ax == bx = (100000 * signum (by - ay)) % 1
  | otherwise = (by - ay) % (bx - ax)

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
  -- ensures that n and a have the same sign
  s n a
    | signum n /= signum a = -a
    | otherwise = a

parse :: String -> [(Int, Int)]
parse = findAll . zip [0..] . fmap (zip [0..]) . lines

findAll :: [(Int, [(Int, Char)])] -> [(Int, Int)]
findAll = concatMap pointsForRow
  where
  pointsForRow (y, row) = filterMap ((== '#') . snd) ((,y) . fst) row
