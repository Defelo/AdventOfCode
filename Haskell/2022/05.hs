import Lib
import Text.Regex.TDFA

type Stack = [Char]

type Instruction = (Int, Int, Int)

type Input = ([Stack], [Instruction])

main :: IO ()
main = aoc 2022 5 setup solve1 solve2 ["1"]

solve1 :: Input -> String
solve1 = solve reverse

solve2 :: Input -> String
solve2 = solve id

solve :: ([Char] -> [Char]) -> Input -> String
solve f = map head . (uncurry . simulate) f

setup :: String -> Input
setup = (\l -> (stacks l, instructions l)) . lines
  where
    stacks = parseStacks . init . takeWhile (not . null)
    instructions = parseInstructions . tail . dropWhile (not . null)

parseStacks :: [String] -> [Stack]
parseStacks l = map ((dropWhile (== ' ') . flip map l . flip (!!)) . (+ 1) . (* 4)) [0 .. n - 1]
  where
    n = (`div` 4) $ (+ 1) $ length $ head l

parseInstructions :: [String] -> [Instruction]
parseInstructions = map ((\[a, b, c] -> (a, b - 1, c - 1)) . map read . getAllTextMatches . (=~ "[0-9]+"))

simulate :: ([Char] -> [Char]) -> [Stack] -> [Instruction] -> [Stack]
simulate f stacks [] = stacks
simulate f stacks ((n, a, b) : xs) = simulate f (newStacks 0) xs
  where
    crates = take n $ stacks !! a
    newA = drop n $ stacks !! a
    newB = f crates ++ stacks !! b
    newStacks i
      | i == length stacks = []
      | i == a = newA : newStacks (i + 1)
      | i == b = newB : newStacks (i + 1)
      | otherwise = stacks !! i : newStacks (i + 1)
