{-# OPTIONS_GHC -Wall #-}
import Util (split, groupOn, prod, sortOnMultiple)
import Intcode (Program, exec, toProgram)
import qualified Data.Map.Strict as M
-- import Control.Monad.State

type Board = M.Map (Int, Int) Int
data Direction = U | R | D | L

data World = World {
  location :: (Int, Int)
  ,board :: Board
  ,facing :: Direction
  -- ,numChanges :: Int
}

newWorld :: World
newWorld = World { location = (0, 0), board = M.empty, facing = U }

main :: IO ()
main = do
  input <- fmap (fmap read . split ',') getContents
  let program = toProgram input
  print $ part1 program
  putStrLn $ part2 program
  return ()

part1 :: Program -> Int
part1 program = length . M.keys $ runAndGetBoard program 0

part2 :: Program -> String
part2 program = displayBoard $ runAndGetBoard program 1

displayBoard :: Board -> String
displayBoard b = unlines . fmap (concatMap (\p -> if M.findWithDefault 0 p b == 0 then " " else "*")) . groupOn snd . sortOnMultiple [snd, fst] $ points
  where
  coords = M.keys b
  minX = minimum . fmap fst $ coords
  minY = minimum . fmap snd $ coords
  maxX = maximum . fmap fst $ coords
  maxY = maximum . fmap snd $ coords
  points = prod [minX..maxX] [minY..maxY]

runAndGetBoard :: Program -> Int -> Board
runAndGetBoard program firstTile = let
  robotOutput = runRobot newWorld programOutput
  programOutput = exec program (firstTile : fmap snd robotOutput)
  in fst . last $ robotOutput

runRobot :: World -> [Int] -> [(Board, Int)]
runRobot _ [] = []
runRobot _ [_] = error "should not happen"
runRobot World { location=l, board = b, facing = f } (color:turn:rest) = let 
  newB = M.insert l color b
  newF = (if turn == 0 then turnLeft else turnRight) f
  newL = move l newF
  nextSquare = M.findWithDefault 0 newL newB
  newW = World newL newB newF
  in (newB, nextSquare) : runRobot newW rest

turnRight :: Direction -> Direction
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

turnLeft :: Direction -> Direction
turnLeft = turnRight . turnRight . turnRight

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) U = (x, y - 1)
move (x, y) R = (x + 1, y)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
