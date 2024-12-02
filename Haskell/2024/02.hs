import Lib

type Input = [[Int]]

main :: IO ()
main = aoc 2024 2 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = length . filter check

solve2 :: Input -> Int
solve2 = length . filter (any check . subLists)

setup :: String -> Input
setup = map (map read . words) . lines

check :: [Int] -> Bool
check = fork (&&) checkMonotone checkRange . diffs

checkMonotone :: [Int] -> Bool
checkMonotone = (((==) . head) >>= all) . map (< 0)

checkRange :: [Int] -> Bool
checkRange = all ((`elem` [1 .. 3]) . abs)

diffs :: [Int] -> [Int]
diffs = zipWith (-) <*> tail

subLists :: [a] -> [[a]]
subLists = fork map (flip removeAt) indices

indices :: [a] -> [Int]
indices = flip take [0 ..] . length

removeAt :: Int -> [a] -> [a]
removeAt = fork (fork (++)) take (drop . (+ 1))
