import Lib
import Text.Regex.TDFA

type Assignment = (Int, Int)

type Input = [(Assignment, Assignment)]

main :: IO ()
main = aoc 2022 4 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = length . filter (\(a, b) -> contains a b || contains b a)

solve2 :: Input -> Int
solve2 = length . filter (uncurry overlaps)

setup :: String -> Input
setup = map ((\[a, b, c, d] -> ((a, b), (c, d))) . (map read . getAllTextMatches . (=~ "[0-9]+"))) . lines

contains :: Assignment -> Assignment -> Bool
contains (a, b) (c, d) = a <= c && d <= b

overlaps :: Assignment -> Assignment -> Bool
overlaps (a, b) (c, d) = not $ b < c || d < a
