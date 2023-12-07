import Data.Bifunctor
import Data.List
import Data.Maybe
import Lib

type Card = Int

type Hand = ([Card], Int)

type Input = [Hand]

main :: IO ()
main = aoc 2023 7 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = sum . zipWith (*) [1 ..] . map snd . sortOn sortKey

solve2 :: Input -> Int
solve2 = solve1 . map (first replaceJokers)

replaceJokers :: [Card] -> [Card]
replaceJokers = map ((*) <*> fromEnum . (/= 11))

sortKey :: Hand -> [Int]
sortKey = fork (:) ty id . fst

ty :: [Card] -> Int
ty = ty . count . filter (/= 0)
  where
    ty [] = 6
    ty [_] = 6
    ty [1, _] = 5
    ty [2, _] = 4
    ty [1, 1, _] = 3
    ty [1, 2, _] = 2
    ty [1, 1, 1, _] = 1
    ty _ = 0

count :: (Eq a) => [a] -> [Int]
count = sort . (map . (.) length . flip elemIndices <*> nub)

setup :: String -> Input
setup = map parseHand . lines

parseHand :: String -> Hand
parseHand = bimap parseCards read . listToTuple . words

parseCards :: String -> [Card]
parseCards = map ((+ 2) . fromJust . (`elemIndex` "23456789TJQKA"))
