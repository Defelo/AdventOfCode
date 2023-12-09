module Lib (aoc, listToTuple, fork) where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Functor
import qualified Data.Text
import System.Environment
import System.Exit

aoc :: (Print b, Print c) => Int -> Int -> (String -> a) -> (a -> b) -> (a -> c) -> [String] -> IO ()
aoc year day setup part1 part2 examples = do
  let run f = f year day setup part1 part2
  args <- getArgs
  if args == ["test"]
    then run test examples >>= bool exitFailure exitSuccess
    else run main >> exitSuccess

main :: (Print b, Print c) => Int -> Int -> (String -> a) -> (a -> b) -> (a -> c) -> IO ()
main year day setup part1 part2 = do
  args <- getArgs
  let path = if length args == 1 then head args else inputPath year day
  input <- readFile path <&> setup
  (putStrLn . toString . part1) input
  (putStrLn . toString . part2) input

test :: (Print b, Print c) => Int -> Int -> (String -> a) -> (a -> b) -> (a -> c) -> [String] -> IO Bool
test year day setup part1 part2 examples = do
  let inp = inputPath year day
  let out = outputPath year day
  a <- testInput "test_input_part1" inp (out 1) setup part1
  b <- testInput "test_input_part2" inp (out 2) setup part2
  cs <- sequence [testExample year day ex setup part1 part2 | ex <- examples]
  pure $ a && b && and cs

testExample :: (Print b, Print c) => Int -> Int -> String -> (String -> a) -> (a -> b) -> (a -> c) -> IO Bool
testExample year day ex setup part1 part2 = do
  let inp = exampleInputPath year day ex
  let out = exampleOutputPath year day ex
  a <- testInput ("test_ex" ++ ex ++ "_part1") inp (out 1) setup part1
  b <- testInput ("test_ex" ++ ex ++ "_part2") inp (out 2) setup part2
  pure $ a && b

testInput :: (Print b) => String -> String -> String -> (String -> a) -> (a -> b) -> IO Bool
testInput name inputPath outputPath setup solve = do
  actual <- readFile inputPath <&> strip . toString . solve . setup
  expected <- readFile outputPath <&> strip
  let ok = actual == expected
  if ok
    then putStrLn $ name ++ ": ok"
    else putStrLn $ name ++ ": fail: " ++ show actual ++ " /= " ++ show expected
  pure ok

inputPath :: Int -> Int -> String
inputPath year day = "../.cache/" ++ show year ++ "/" ++ show day

outputPath :: Int -> Int -> Int -> String
outputPath year day part = inputPath year day ++ "." ++ show part

exampleInputPath :: Int -> Int -> String -> String
exampleInputPath year day ex = "../examples/" ++ show year ++ "/" ++ show day ++ "/" ++ ex

exampleOutputPath :: Int -> Int -> String -> Int -> String
exampleOutputPath year day ex part = exampleInputPath year day ex ++ "." ++ show part

strip :: String -> String
strip = Data.Text.unpack . Data.Text.strip . Data.Text.pack

listToTuple :: [a] -> (a, a)
listToTuple = fork (curry id) head last

fork :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
fork = liftA2

class Print a where
  toString :: a -> String

instance Print String where
  toString = id

instance Print Int where
  toString = show
