{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
import Text.Parsec hiding (parse)
import Util (groupOn)
import Data.List (sortOn, nub)
import Data.Either (rights)
-- import qualified Data.Map.Strict as M
import Control.Monad (unless)
import Control.Arrow ((&&&), (***), second)
-- import Debug.Trace (trace)

type Chemical = (String, Int)
type Reaction = (Chemical, [Chemical])
type Dep = (String, [String])

getDeps :: [Reaction] -> [Dep]
getDeps reactions = let
  deps = fmap (fst *** fmap fst) reactions
  in fmap ((id &&& depsFor deps) . fst) deps

depsFor :: [Dep] -> String -> [String]
depsFor deps x = let
  currDeps = x `lookup` deps
  in case currDeps of
    Nothing -> []
    Just currDeps' -> nub (concatMap (depsFor deps) currDeps' ++ currDeps')

main :: IO ()
main = do
  input <- fmap lines (readFile "day14.in")
  let reactions = rights . fmap parseReaction $ input
  unless (length reactions == length input) (error "parse error")
  let deps = getDeps reactions
  print $ iterateUntil (\curr -> simplify . concatMap (expand reactions deps curr) $ curr) [("FUEL", 1)] (all ((== "ORE") . fst))

lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe (a, b)
lookupBy _ [] =  Nothing
lookupBy f ((x,y):xs)
    | f x = Just (x, y)
    | otherwise = lookupBy f xs

iterateUntil :: (Show a) => (a -> a) -> a -> (a -> Bool) -> a
iterateUntil next x0 done = if done x0 then x0 else iterateUntil next (next x0) done

expand :: [Reaction] -> [Dep] -> [Chemical] -> Chemical -> [Chemical]
expand _ _ _ c@(name, _) | name == "ORE" = [c]
expand xs deps rest (name, num) = if otherLeadsToCurr then [(name, num)] else fmap (second (* factor)) inputs
  where
  otherLeadsToCurr = any (\x -> case x `lookup` deps of
    Nothing -> False
    Just leadsTo -> name `elem` leadsTo) (fmap fst rest)
  Just ((_, num'), inputs) = lookupBy ((== name) . fst) xs
  factor = if num' > num then 1 else case num `divMod` num' of
    (q, r) -> if r > 0 then q + 1 else q

simplify :: [Chemical] -> [Chemical]
simplify = fmap ((fst . head) &&& (sum . fmap snd)) . groupOn fst . sortOn fst

-- parser
type Parser a = Parsec String () a

parseReaction :: String -> Either ParseError Reaction
parseReaction = runParser reactionParser () ""

chemicalParser :: Parser Chemical
chemicalParser = do
  num <- many1 digit
  space
  name <- many1 letter
  return (name, read num)

reactionParser :: Parser Reaction
reactionParser = do
  inputs <- chemicalParser `sepBy` string ", "
  string " => "
  output <- chemicalParser
  return (output, inputs)
