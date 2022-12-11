#!/usr/bin/env runhaskell

import System.IO
import Data.List (sortBy, sort, reverse)
import Data.Function (on)

-- Tokenized input lines
data InputLine = CommandCdTop 
               | CommandCdBack
               | CommandCdDir           String
               | CommandLs
               | CommandOutputDirectory String
               | CommandOutputFile      Int String
                 deriving (Show, Eq)

-- Directories and files with full paths
data Listing = Directory [String]
             | File      [String] Int
               deriving (Show, Eq)

-- Filesystem tree with directory size cache
data DirTree = TDirectory [String] Int [DirTree]
             | TFile      [String] Int
               deriving (Show, Eq)


-- Full path to file or directory
path :: Listing -> [String]
path (Directory path)   = path
path (File      path _) = path

-- Size of file or directory in filesystem tree
sizeOf :: DirTree -> Int
sizeOf (TDirectory _ size _) = size
sizeOf (TFile      _ size)   = size

-- Convert input line to token
tokenize :: [String] -> InputLine
tokenize ["$",   "cd", "/" ] = CommandCdTop
tokenize ["$",   "cd", ".."] = CommandCdBack
tokenize ["$",   "cd", path] = CommandCdDir           path
tokenize ["$",   "ls"      ] = CommandLs
tokenize ["dir", name      ] = CommandOutputDirectory name
tokenize [size,  name      ] = CommandOutputFile      (read size) name

-- Convert tokenized input to listings
-- readListing :: lsMode -> currentDirectory -> input
-- lsMode => Ensures that directories and files can only be after ls command
readListing :: Bool -> [String] -> [InputLine] -> [Listing]
readListing _    _          []                                 =                                              []
readListing _    _          ((CommandCdTop):lt)                =                                              readListing False []                lt
readListing _    currentDir ((CommandCdBack):lt)               =                                              readListing False (tail currentDir) lt
readListing _    currentDir ((CommandCdDir dir):lt)            =                                              readListing False (dir:currentDir)  lt
readListing _    currentDir ((CommandLs):lt)                   =                                              readListing True  currentDir        lt
readListing True currentDir ((CommandOutputDirectory name):lt) = Directory (reverse (name:currentDir))      : readListing True  currentDir        lt
readListing True currentDir ((CommandOutputFile size name):lt) = File      (reverse (name:currentDir)) size : readListing True  currentDir        lt

-- Helper function to resolve all sub-directories and files of a directory
resolveSubdirs :: [String] -> [Listing] -> ([DirTree], [Listing])
resolveSubdirs pathPrefix list = dirs
    where -- Create make file/directory entry
          (rest, tree) = makeTree pathPrefix list
          -- Resolve next file/directory
          (nextDirs, restNext) = resolveSubdirs pathPrefix rest
          -- If there are no more directories, end recursion
          dirs = maybe ([], rest) (\t -> (t:nextDirs, restNext)) tree

-- Create DirTree entry from listings, returns also unused listings
makeTree :: [String] -> [Listing] -> ([Listing], Maybe DirTree)
-- No more listings
makeTree _ [] = ([], Nothing)
-- Handle creating file
makeTree pathPrefix l@((File path size):lt)
    -- Does this file belong into current directory?
    | init path == pathPrefix = (lt, Just (TFile path size))
    -- No? Then leave it in listings and return it as unused
    | otherwise = (l, Nothing)
-- Handle directory
makeTree pathPrefix l@((Directory path):lt)
    -- Does this directory belong into current directory?
    | init path == pathPrefix = (rest, Just (TDirectory path size subDirs))
    -- No? Then leave it in listings and return it as unused
    | otherwise = (l, Nothing)
    where -- Resolve sub-directories and files
          (subDirs, rest) = resolveSubdirs path lt
          -- Compute directory size
          size = sum $ map (sizeOf) subDirs

-- Create filesystem root directory from listings
makeRootTree :: [Listing] -> DirTree
makeRootTree list = TDirectory [] size subDirs
    where -- Make sure list is sorted correctly based on directory structure
          sortedList = sortBy (compare `on` path) list
          -- Resolve sub-directories and files of root directory
          (subDirs, _) = resolveSubdirs [] sortedList
          -- Compute directory size
          size = sum $ map (sizeOf) subDirs

-- Makes list of sizes of all directories in filesystem structure
dirSizes :: DirTree -> [Int]
-- Skip files
dirSizes (TFile _ size) = []
-- Directory and all subdirectories
dirSizes (TDirectory _ size trees) = size : concat (map dirSizes trees)

-- Parse input and create filesystem root directory structure
parseInput :: String -> DirTree
parseInput = makeRootTree . readListing False [] . map (tokenize.words) . lines

-- Sum of size of all directories with size smaller then sizeLimit
resultPart1 :: DirTree -> Int
resultPart1 root = sum $ filter (<=sizeLimit) $ dirSizes root
    where -- Only sum files smaller then limit
          sizeLimit = 100000

-- Size of smallest directory big enough to free enough space
-- such that there is more free space then requiredSpace
resultPart2 :: DirTree -> Int
resultPart2 root = head $ sort $ filter (>=mustFree) $ dirSizes root
    where -- Filesystem free space
          freeSpace = 70000000 - sizeOf root
          -- Required free space
          requiredSpace = 30000000
          -- Space to be freed
          mustFree = requiredSpace - freeSpace

printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let root = parseInput contents
        -- Part #1
        resultP1 = resultPart1 root
        -- Part #2
        resultP2 =  resultPart2 root

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++ show resultP1 ++ "' | '" ++ show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal07-1.test.txt" -- 95437 | 24933642
    printResult "./data/cal07-1.orig.txt"
