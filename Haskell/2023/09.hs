import Lib

type Input = [[Int]]

main :: IO ()
main = aoc 2023 9 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = sum . map solve

solve2 :: Input -> Int
solve2 = sum . map (solve . reverse)

solve :: [Int] -> Int
solve = sum . map last . takeWhile (any (/= 0)) . iterate step

step :: [Int] -> [Int]
step = zipWith (-) =<< tail

setup :: String -> Input
setup = map (map read . words) . lines
