import Data.Bifunctor
import Data.List
import Data.List.Split
import Lib

type Rule = (Int, Int)

type Input = ([Rule], [[Int]])

main :: IO ()
main = aoc 2024 5 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = uncurry ((.) (sum . map mid) . (filter . (((==) <*>) . sortUpdate)))

solve2 :: Input -> Int
solve2 = uncurry (((.) . ((.) sum . (map . ((.) mid . sortUpdate)))) <*> (filter . (((/=) <*>) . sortUpdate)))

setup :: String -> Input
setup = fork (,) (parseRules . takeWhile (not . null)) (parseUpdates . dropWhile (not . null)) . lines

parseRules :: [String] -> [Rule]
parseRules = map (listToTuple . map read . splitOn "|")

parseUpdates :: [String] -> [[Int]]
parseUpdates = map (map read . splitOn ",") . filter (not . null)

sortUpdate :: [Rule] -> [Int] -> [Int]
sortUpdate = sortBy . cmpPages

cmpPages :: [Rule] -> Int -> Int -> Ordering
cmpPages rules a b
  | (a, b) `elem` rules = LT
  | (b, a) `elem` rules = GT
  | otherwise = EQ

mid :: [a] -> a
mid = (!!) <*> (`div` 2) . length
