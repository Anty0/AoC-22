#!/usr/bin/env runhaskell

import System.IO  
import Data.List (sort)
import Data.List.Split (splitOn)


-- Split into blocks by empty lines
splitBlocks :: String -> [[String]]
splitBlocks fileContents = splitOn [""] $ lines fileContents


-- Sum blocks as ints and sort them
sumBlocks :: (Read a, Ord a, Num a) => [[String]] -> [a]
sumBlocks blocks = reverse $ sort $ map (sum.map read) blocks


main = do
    -- Reading file
    handle <- openFile "cal01-1.test.txt" ReadMode
    contents <- hGetContents handle

    -- Main logic
    let blocks = splitBlocks contents
        blocksSum = sumBlocks blocks
        -- Part #1 - top block
        blocksMax_part1 = head blocksSum
        -- Part #2 - sum of 3 top blocks
        blocksMax_part2 = sum $ take 3 blocksSum
    
    -- Output handling
    print blocksMax_part1
    print blocksMax_part2

    -- Closing file
    hClose handle
