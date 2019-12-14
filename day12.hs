{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
import Text.Parsec hiding (parse)
import Util (prod, applyNTimes)
import Data.Either (rights)
import Control.Monad (unless)
import qualified Data.IntMap as IM
import Data.List (nubBy)

type Coord = (Int, Int, Int)

data Moon = Moon Int Coord Coord deriving (Eq, Show)

type MoonMap = IM.IntMap Moon

map1 :: (a -> d) -> (a, b, c) -> (d, b, c)
map1 f (a, b, c) = (f a, b, c)

map2 :: (b -> d) -> (a, b, c) -> (a, d, c)
map2 f (a, b, c) = (a, f b, c)

map3 :: (c -> d) -> (a, b, c) -> (a, b, d)
map3 f (a, b, c) = (a, b, f c)

main :: IO ()
main = do
  input <- fmap lines getContents
  let moons = rights $ zipWith parseMoon input [1..]
  unless (length moons == length input) (error "parse error")
  let moonMap = IM.fromList (zip [1..] moons)
  print $ sum . fmap energy . applyNTimes 1000 step $ moonMap
  -- let states = tail $ iterate step moonMap
  -- print . length . fst . break (== moonMap) $ states
  return ()

energy :: Moon -> Int
energy (Moon _ (a, b, c) (d, e, f)) = sum (fmap abs [a, b, c]) * sum (fmap abs [d, e, f])

step :: MoonMap -> MoonMap
step moonMap = IM.map applyVelocity . foldl applyGravity moonMap $ pairs
  where
  moons = IM.keys moonMap
  pairs = nubBy (\(a, b) (c, d) -> a == d && b == c) . filter (uncurry (/=)) $ prod moons moons

updateMoon :: MoonMap -> Moon -> MoonMap
updateMoon moonMap moon@(Moon k _ _) = IM.insert k moon moonMap

applyGravity :: MoonMap -> (Int, Int) -> MoonMap
applyGravity moonMap (a, b) = updateMoon (updateMoon moonMap (Moon ka posA newA)) (Moon kb posB newB)
  where
  (Moon ka posA@(xa, xb, xc) velA) = moonMap IM.! a
  (Moon kb posB@(ya, yb, yc) velB) = moonMap IM.! b
  newA = map3 (+ signum (yc - xc)) . map2 (+ signum (yb - xb)) . map1 (+ signum (ya - xa)) $ velA
  newB = map3 (+ signum (xc - yc)) . map2 (+ signum (xb - yb)) . map1 (+ signum (xa - ya)) $ velB

applyVelocity :: Moon -> Moon
applyVelocity (Moon k (a, b, c) v@(d, e, f)) = Moon k (a + d, b + e, c + f) v

-- parser
type Parser a = Parsec String () a

parseMoon :: String -> Int -> Either ParseError Moon
parseMoon str k = runParser (moonParser k) () "" str

numParser :: Parser Int
numParser = do
  many space
  sym <- option '_' (char '-')
  num <- many digit
  return $ read num * (if sym == '-' then -1 else 1)

moonParser :: Int -> Parser Moon
moonParser k = do
  string "<x="
  x <- numParser
  string ", y="
  y <- numParser
  string ", z="
  z <- numParser
  string ">"
  return $ Moon k (x, y, z) (0, 0, 0)
