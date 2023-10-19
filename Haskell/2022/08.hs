import Lib

data Direction = North | East | South | West

type Coord = (Int, Int)

type Size = (Int, Int)

type Grid = [[Int]]

type Input = (Size, Grid)

main :: IO ()
main = aoc 2022 8 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 input@(size, _) = length $ filter (isVisible input) $ coords size

solve2 input@(size, _) = maximum $ map (scenicScore input) $ coords size

setup :: String -> Input
setup inp = ((length grid, length $ head grid), grid)
  where
    grid = map parseLine $ lines inp

parseLine :: String -> [Int]
parseLine = map (read . (: []))

isVisible :: Input -> Coord -> Bool
isVisible input p = any (canSeeEdge input p) [North, East, South, West]

scenicScore :: Input -> Coord -> Int
scenicScore input p = product $ map (visibleTrees input p) [North, East, South, West]

canSeeEdge :: Input -> Coord -> Direction -> Bool
canSeeEdge (size, grid) p d = all ((< at grid p) . at grid) $ steps d size p

visibleTrees :: Input -> Coord -> Direction -> Int
visibleTrees (size, grid) p d = length $ takeWhileInclusive ((< at grid p) . at grid) $ steps d size p

steps :: Direction -> Size -> Coord -> [Coord]
steps d size = takeWhile (inGrid size) . iterate (step d) . step d

step :: Direction -> Coord -> Coord
step North (i, j) = (i - 1, j)
step East (i, j) = (i, j + 1)
step South (i, j) = (i + 1, j)
step West (i, j) = (i, j - 1)

inGrid :: Size -> Coord -> Bool
inGrid (h, w) (i, j) = 0 <= i && i < h && 0 <= j && j < w

coords :: Size -> [Coord]
coords (h, w) = [(i, j) | i <- [0 .. h - 1], j <- [0 .. w - 1]]

at :: Grid -> Coord -> Int
at grid (i, j) = grid !! i !! j

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive f [] = []
takeWhileInclusive f (x : xs)
  | f x = x : takeWhileInclusive f xs
  | otherwise = [x]
