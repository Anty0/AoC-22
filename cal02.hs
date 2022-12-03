#!/usr/bin/env runhaskell

import System.IO
import Data.List.Split (splitOn)

-- Score of played character
charScore :: Char -> Int
charScore 'A' = 1
charScore 'B' = 2
charScore 'C' = 3
charScore 'X' = 1
charScore 'Y' = 2
charScore 'Z' = 3

-- Score of game result
compareScore :: (Char, Char) -> Int
compareScore ('A', 'X') = 3
compareScore ('A', 'Y') = 6
compareScore ('A', 'Z') = 0
compareScore ('B', 'Y') = 3
compareScore ('B', 'Z') = 6
compareScore ('B', 'X') = 0
compareScore ('C', 'Z') = 3
compareScore ('C', 'X') = 6
compareScore ('C', 'Y') = 0

-- Matching character to loose round
loseCh :: Char -> Char
loseCh 'A' = 'Z'
loseCh 'B' = 'X'
loseCh 'C' = 'Y'

-- Matching character to win round
winCh :: Char -> Char
winCh 'A' = 'Y'
winCh 'B' = 'Z'
winCh 'C' = 'X'

-- Matching character to draw round
drawCh :: Char -> Char
drawCh 'A' = 'X'
drawCh 'B' = 'Y'
drawCh 'C' = 'Z'

-- Modify round to fit as solution of part 2
correctRound :: (Char, Char) -> (Char, Char)
correctRound (a, 'X') = (a, loseCh a)
correctRound (a, 'Y') = (a, drawCh a)
correctRound (a, 'Z') = (a, winCh a)

-- Split input into rounds
splitRounds :: String -> [(Char, Char)]
splitRounds fileContents = map (\[[a],[x]] -> (a,x)) $ map (splitOn " ") $ filter (/= "") $ splitOn "\n" fileContents

-- Calculate score of round
roundScore :: (Char, Char) -> Int
roundScore g@(a, x) = charScore x + compareScore g


main = do
    -- Reading file
    handle <- openFile "cal02-1.test.txt" ReadMode
    contents <- hGetContents handle

    -- Main logic
    let rounds = splitRounds contents
        -- Part #1 - score form input
        score_part1 = sum $ map (roundScore) rounds
        -- Part #2 - transformed score form input
        score_part2 = sum $ map (roundScore.correctRound) rounds
    
    -- Output handling
    -- print rounds
    print score_part1
    print score_part2

    -- Closing file
    hClose handle
