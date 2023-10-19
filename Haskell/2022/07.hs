import Data.List
import Lib

type Filename = String

type Filesize = Int

data File = Dir Filename | File Filename Filesize deriving (Show)

data Command = Cd String | Ls [File] deriving (Show)

data FileTree = TreeDir Filesize [FileTree] | TreeFile Filesize deriving (Show)

type Input = [Int]

main :: IO ()
main = aoc 2022 7 setup solve1 solve2 ["1"]

solve1 :: Input -> Int
solve1 = sum . filter (<= 100000)

solve2 :: Input -> Int
solve2 dirs = minimum $ filter (>= 30000000 - 70000000 + head dirs) dirs

setup :: String -> Input
setup = flattenTree . (: []) . buildTree . collectFiles [] . parseCommands . lines

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands (x : xs)
  | "$ cd" `isPrefixOf` x = Cd (drop 5 x) : parseCommands xs
  | "$ ls" `isPrefixOf` x = Ls (map parseFile $ takeWhile inLs xs) : parseCommands (dropWhile inLs xs)
  where
    inLs = not . isPrefixOf "$"

parseFile :: String -> File
parseFile ('d' : 'i' : 'r' : ' ' : x) = Dir x
parseFile x = File (drop 1 $ dropWhile (/= ' ') x) (read $ takeWhile (/= ' ') x)

collectFiles :: [Filename] -> [Command] -> [([String], Int)]
collectFiles path [] = []
collectFiles path (Cd "/" : xs) = collectFiles [] xs
collectFiles [] (Cd ".." : xs) = collectFiles [] xs
collectFiles path (Cd ".." : xs) = collectFiles (init path) xs
collectFiles path (Cd name : xs) = collectFiles (path ++ [name]) xs
collectFiles path (Ls x : xs) = [(path ++ [name], size) | (File name size) <- x] ++ collectFiles path xs

buildTree :: [([String], Int)] -> FileTree
buildTree xs = TreeDir (treeSize $ dirs ++ files) $ dirs ++ files
  where
    files = [TreeFile size | ([name], size) <- xs]
    dirNames = nub [name | (name : _ : _, _) <- xs]
    dirEntries dir = [(ps, size) | (p : ps, size) <- xs, p == dir]
    dirs = [buildTree $ dirEntries name | name <- dirNames]

treeSize :: [FileTree] -> Int
treeSize [] = 0
treeSize ((TreeDir size entries) : xs) = treeSize entries + treeSize xs
treeSize ((TreeFile size) : xs) = size + treeSize xs

flattenTree :: [FileTree] -> [Int]
flattenTree [] = []
flattenTree (TreeDir size entries : xs) = size : (flattenTree entries ++ flattenTree xs)
flattenTree (_ : xs) = flattenTree xs
