import Data.Char
import Lib

type Input = [(Int, Int)]

main :: IO ()
main = aoc 2022 2 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = sum . map (\(x, y) -> (y - x + 1) `mod` 3 * 3 + y)

solve2 :: Input -> Int
solve2 = sum . map (\(x, y) -> (y + x) `mod` 3 + y * 3 - 2)

setup :: String -> Input
setup = map (\[a, _, b] -> (ord a - 64, ord b - 87)) . lines
