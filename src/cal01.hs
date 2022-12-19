#!/usr/bin/env runhaskell

import Data.List (sort)
import Data.List.Split (splitOn)


-- Split into blocks by empty lines
splitBlocks :: String -> [[String]]
splitBlocks = splitOn [""] . lines


-- Sum blocks as ints and sort them
sumBlocks :: (Read a, Ord a, Num a) => [[String]] -> [a]
sumBlocks blocks = reverse $ sort $ map (sum.map read) blocks


main = do
    -- Reading file
    contents <- readFile "./data/cal01-1.test.txt" -- 24000 | 45000
    -- contents <- readFile "./data/cal01-1.orig.txt"

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
