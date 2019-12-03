module Util where

import Data.List (sortBy, groupBy)
import Data.Monoid ((<>))
import Data.Function (on)

allLines :: IO [String]
allLines = fmap lines getContents

prod :: [a] -> [b] -> [(a, b)]
prod a b = [(x, y) | x <- a, y <- b]

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

sortOnMultiple :: (Ord b) => [a -> b] -> [a] -> [a]
sortOnMultiple fs = sortBy (\a b -> foldl (<>) EQ $ map (\f -> f a b) fs')
  where
    fs' = map (compare `on`) fs

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
