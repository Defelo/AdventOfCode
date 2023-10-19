import Data.Char
import Data.List
import Data.Maybe
import Lib

type Input = [String]

main :: IO ()
main = aoc 2022 3 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = sum . map (prio . fromJust . uncurry (find . flip elem) . splitHalf)

solve2 :: Input -> Int
solve2 = sum . map (prio . fromJust . \(x : xs) -> find (flip all xs . elem) x) . join3

setup :: String -> Input
setup = lines

splitHalf :: String -> (String, String)
splitHalf s = splitAt (length s `div` 2) s

join3 :: Input -> [[String]]
join3 = foldr add [[]]
  where
    add x (a : as)
      | length a == 3 = [x] : a : as
      | otherwise = (x : a) : as

prio :: Char -> Int
prio c = ord c - if c >= 'a' then 96 else 38
