import Data.List
import Data.Ord (clamp)
import Lib
import Prelude hiding (Left, Right)

data Direction = Up | Down | Left | Right deriving (Show)

type Position = (Int, Int)

type Rope = [Position]

type State = (Rope, [Position])

type Motion = (Direction, Int)

type Input = [Motion]

main :: IO ()
main = aoc 2022 9 setup solve1 solve2 ["1", "2"]

solve1 :: Input -> Int
solve1 = solve 2

solve2 :: Input -> Int
solve2 = solve 10

solve :: Int -> Input -> Int
solve n = length . nub . snd . foldl step (initialRope n, []) . directions

step :: State -> Direction -> State
step (rope, visited) m = (newRope, head newRope : visited)
  where
    newRope = stepRope rope m

stepRope :: Rope -> Direction -> Rope
stepRope r m = scanr follow (move m $ last r) $ init r

follow :: Position -> Position -> Position
follow (x, y) (x', y')
  | abs (x - x') <= 1 && abs (y - y') <= 1 = (x, y)
  | otherwise = (x + clamp (-1, 1) (x' - x), y + clamp (-1, 1) (y' - y))

directions :: [Motion] -> [Direction]
directions = concatMap $ uncurry $ flip replicate

setup :: String -> Input
setup = map parseMotion . lines

parseMotion :: String -> Motion
parseMotion (d : ' ' : n) = (parseDirection d, read n)

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Left
parseDirection 'R' = Right

move :: Direction -> Position -> Position
move Up (x, y) = (x, y - 1)
move Down (x, y) = (x, y + 1)
move Left (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)

initialRope :: Int -> Rope
initialRope = flip replicate (0, 0)
