import Data.List
import Lib

type Input = [[Int]]

main :: IO ()
main = aoc 2022 1 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = maximum . map sum

solve2 :: Input -> Int
solve2 = sum . take 3 . reverse . sort . map sum

setup :: String -> Input
setup = foldr add [[]] . lines
  where
    add "" acc = [] : acc
    add x (a : as) = (read x : a) : as
