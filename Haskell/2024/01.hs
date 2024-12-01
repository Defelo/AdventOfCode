import Data.Function
import Data.List
import Lib

type Input = [(Int, Int)]

main :: IO ()
main = aoc 2024 1 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = sum . map abs . fork (on (zipWith (-)) sort) (map fst) (map snd)

solve2 :: Input -> Int
solve2 inp = sum $ map ((*) <*> (`count` right)) left
  where
    (left, right) = fork (,) (map fst) (map snd) inp
    count a = length . filter (== a)

setup :: String -> Input
setup = map (listToTuple . map read . words) . lines
