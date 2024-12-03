import Lib
import Text.Regex.TDFA

type Input = String

main :: IO ()
main = aoc 2024 3 id solve1 solve2 ["1", "2"]

solve1 :: Input -> Int
solve1 = sum . map handle . matches
  where
    handle :: Match -> Int
    handle (Mul a b) = a * b
    handle _ = 0

solve2 :: Input -> Int
solve2 = fst . foldl handle (0, True) . matches
  where
    handle :: (Int, Bool) -> Match -> (Int, Bool)
    handle (sum, True) (Mul a b) = (sum + a * b, True)
    handle (sum, False) (Mul _ _) = (sum, False)
    handle (sum, _) Enable = (sum, True)
    handle (sum, _) Disable = (sum, False)

data Match = Mul Int Int | Enable | Disable deriving (Show)

matches :: String -> [Match]
matches = map parseMatch . (=~ "mul\\(([0-9]+),([0-9]+)\\)|do(n't)?\\(\\)")

parseMatch :: [String] -> Match
parseMatch ("do()" : _) = Enable
parseMatch ("don't()" : _) = Disable
parseMatch (_ : a : b : _) = Mul (read a) (read b)
