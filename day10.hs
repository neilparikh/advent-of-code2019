{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf, TupleSections #-}
import Util (filterMap, groupOn, fmapWithTag)
import Data.Ratio ((%), Ratio)
import Data.List (maximumBy, sortBy, sortOn, minimumBy, delete)
import Data.Ord (comparing)
import Data.Monoid ((<>))

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (ax, ay) (bx, by) = (ax + bx, ay + by)

neg :: (Int, Int) -> (Int, Int)
neg (a, b) = (-a, -b)

main :: IO ()
main = do
  input <- getContents
  let pts = parse input
  let groupsByAngleFor o = groupOn (angle o) . sortOn (angle o) . delete o $ pts
  let (bestAsteroid, groupsByAngle) = maximumBy (comparing (length . snd)) . fmapWithTag groupsByAngleFor $ pts
  -- part 1
  print (length groupsByAngle)
  -- part 2
  let (x, y) = minimumBy (comparing (distance bestAsteroid)) . (!! 199) . sortBy (comparing (angle bestAsteroid . head)) $ groupsByAngle
  print $ x * 100 + y
  return ()

-- no need to sqrt, since compare a b == compare (sqrt a) (sqrt b), I hope?
distance :: (Int, Int) -> (Int, Int) -> Int
distance (ax, ay) (bx, by) = (ax - bx)^(2 :: Int) + (ay - by)^(2 :: Int)

-- an angle is a combination of its quadrant and its absolute slope
data Angle = Angle { q :: Int, s :: Ratio Int }  deriving Eq

instance Ord Angle where
  compare (Angle q1 slope1) (Angle q2 slope2) = compare q1 q2 <> compare slope1 slope2

angle :: (Int, Int) -> (Int, Int) -> Angle
angle origin other = Angle (quad delta) (slope delta)
  where
  delta = move other (neg origin)

-- returns the quadrant of a point
-- the checks for y are flipped, since y is increasing in the downward direction
quad :: (Int, Int) -> Int
quad (x, y) = if | x >= 0 && y < 0 -> 1
                 | y >= 0 && x > 0 -> 2
                 | x <= 0 && y > 0 -> 3
                 | otherwise -> 4

slope :: (Int, Int) -> Ratio Int
slope (x, y)
  | x == 0 = (100000 * signum y) % 1 -- represent inf as 100000, since that slope isn't going to occur
  | otherwise = y % x

parse :: String -> [(Int, Int)]
parse = findAll . zip [0..] . fmap (zip [0..]) . lines

findAll :: [(Int, [(Int, Char)])] -> [(Int, Int)]
findAll = concatMap pointsForRow
  where
  pointsForRow (y, row) = filterMap ((== '#') . snd) ((,y) . fst) row
