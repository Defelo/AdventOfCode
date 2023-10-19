import Data.List
import Data.Maybe
import Lib

type Input = String

main :: IO ()
main = aoc 2022 6 setup solve1 solve2 ["1", "2", "3", "4", "5"]

solve1 :: Input -> Int
solve1 = solve 4

solve2 :: Input -> Int
solve2 = solve 14

solve :: Int -> Input -> Int
solve n = (+ n) . fromJust . findIndex unique . slices n

setup :: String -> Input
setup = id

slices :: Int -> [a] -> [[a]]
slices n lst
  | null $ drop (n - 1) lst = []
  | otherwise = take n lst : slices n (tail lst)

unique :: (Eq a) => [a] -> Bool
unique lst = length (nub lst) == length lst
