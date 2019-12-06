module Util where

import Data.List (sortBy, groupBy, sortOn)
import Data.Monoid ((<>))
import Data.Function (on)
import Data.Ord (comparing)
import Control.Arrow ((&&&))

allLines :: IO [String]
allLines = fmap lines getContents

prod :: [a] -> [b] -> [(a, b)]
prod a b = [(x, y) | x <- a, y <- b]

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

sortOnMultiple :: (Ord b) => [a -> b] -> [a] -> [a]
sortOnMultiple fs = sortBy (\a b -> mconcat . map (\f -> comparing f a b) $ fs)

myMaximum [] = 0
myMaximum xs = maximum xs

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:_) 1 = Just x
at (_:xs) n = at xs (pred n)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)

split :: (Eq a) => a -> [a] -> [[a]]
split c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : split c s''
    where
    (w, s'') = break (== c) s'

-- based on https://stackoverflow.com/a/25760740
-- intersects by f, and then merges the element using g
intersectBy :: Ord b => (a -> b) -> (a -> a -> a) -> [a] -> [a] -> [a]
intersectBy f g xs ys = intersectSorted f g (sortOn f xs) (sortOn f ys)
  where
  intersectSorted :: Ord b => (a -> b) -> (a -> a -> a) -> [a] -> [a] -> [a]
  intersectSorted f g (x:xs) (y:ys)
   | f x == f y    = g x y : intersectSorted f g xs ys
   | f x < f y     = intersectSorted f g xs (y:ys)
   | f x > f y     = intersectSorted f g (x:xs) ys
  intersectSorted _ _ _ _ = []

fmapWithTag :: Functor f => (a -> b) -> f a -> f (a, b)
fmapWithTag f = fmap (id &&& f)

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)
listToTuple _ = error "list does not have exactly 2 elems"
