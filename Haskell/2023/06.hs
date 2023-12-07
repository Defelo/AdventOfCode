import Control.Monad
import Data.Bifunctor
import Lib

type Time = Int

type Distance = Int

type Input = ([Time], [Distance])

main :: IO ()
main = aoc 2023 6 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = product . map (uncurry solve) . uncurry zip

solve2 :: Input -> Int
solve2 = uncurry solve . join bimap joinInts

solve :: Time -> Distance -> Int
solve time distance =
  let x = time * time - distance * 4
      root = floor . sqrt . fromIntegral $ x
      root_is_int = fromEnum (root * root == x)
   in root + (time + root + 1 + root_is_int) `mod` 2 - root_is_int

setup :: String -> Input
setup = listToTuple . map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . tail . words

joinInts :: [Int] -> Int
joinInts = read . concatMap show
