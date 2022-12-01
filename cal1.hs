#!/usr/bin/env runhaskell

import System.IO  
import Control.Monad
import Data.List (sort)
import Data.List.Split (splitOn)


-- Split into blocks by empty lines
splitBlocks :: String -> [[String]]
splitBlocks fileContents = splitOn [""] $ splitOn "\n" fileContents


-- Sum blocks as ints and sort them
sumBlocks :: (Read a, Ord a, Num a) => [[String]] -> [a]
sumBlocks blocks = reverse $ sort $ map (sum.map read) blocks


main = do
    -- Reading file
    handle <- openFile "cal1-1.test.txt" ReadMode
    contents <- hGetContents handle

    -- Main logic
    let blocks = splitBlocks contents
        blocks_sum = sumBlocks blocks
        -- Part #1 - top block
        blocks_max_1 = blocks_sum !! 0
        -- Part #2 - sum of 3 top blocks
        blocks_max_2 = sum $ take 3 blocks_sum
    
    -- Output handling
    print blocks_max_1
    print blocks_max_2

    -- Closing file
    hClose handle
