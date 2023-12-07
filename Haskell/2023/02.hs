import Data.List
import Data.List.Split
import Lib

type CubeCounts = [Int]

type Game = [CubeCounts]

type Input = [Game]

main :: IO ()
main = aoc 2023 2 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = sum . map fst . filter (and . zipWith (>=) [12, 13, 14] . snd) . zip [1 ..] . map minConfig

solve2 :: Input -> Int
solve2 = sum . map (product . minConfig)

minConfig :: Game -> CubeCounts
minConfig = foldl1 (zipWith max)

setup :: String -> Input
setup = map parseGame . lines

parseGame :: String -> Game
parseGame = map parseCubeCounts . splitOn "; " . last . splitOn ": "

parseCubeCounts :: String -> CubeCounts
parseCubeCounts = collectCounts . map (fork (curry id) (read . head) last . words) . splitOn ", "

collectCounts :: [(Int, String)] -> CubeCounts
collectCounts = flip map ["red", "green", "blue"] . flip getCount

getCount :: String -> [(Int, String)] -> Int
getCount = (.) (maybe 0 fst) . (find . ((. snd) . (==)))
