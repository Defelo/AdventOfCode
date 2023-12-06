import Control.Applicative
import Control.Arrow
import Control.Monad
import Lib

type Time = Int

type Distance = Int

type Input = ([Time], [Distance])

main :: IO ()
main = aoc 2023 6 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = product . map (uncurry solve) . uncurry zip

solve2 :: Input -> Int
solve2 = uncurry solve . join (***) joinInts

solve :: Time -> Distance -> Int
solve time distance =
  let x = time * time - distance * 4
      root = floor . sqrt . fromIntegral $ x
      root_is_int = fromEnum (root * root == x)
   in root + (time + root + 1 + root_is_int) `mod` 2 - root_is_int

setup :: String -> Input
setup = liftA2 (curry id) head last . map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . tail . words

joinInts :: [Int] -> Int
joinInts = read . concatMap show
