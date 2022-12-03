#!/usr/bin/env runhaskell

import System.IO
import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (splitOn, chunksOf)


-- Split list into two halves
halveList :: [a] -> ([a], [a])
halveList l = (take len l, drop len l)
    where len = (length l) `div` 2

-- Calculate score of item
itemScore :: Char -> Int
itemScore c
    | ord c >= ord 'a' = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

-- Split input into rucksacks
splitRucksacks :: String -> [String]
splitRucksacks fileContents = filter (/= "") $ splitOn "\n" fileContents

-- Calculate rucksack intersection
intersectRucksack :: String -> String
intersectRucksack rucksack = intersect l r
    where (l, r) = halveList rucksack

-- Calculate score of rucksack
scoreRucksack :: String -> Int
scoreRucksack rucksack = itemScore $ head $ intersectRucksack rucksack

-- Calculate team intersection
intersectTeam :: [String] -> String
intersectTeam [t] = t
intersectTeam (t:tx) = intersect t (intersectTeam tx)
intersectTeam [] = []

-- Calculate score of team
scoreTeam :: [String] -> Int
scoreTeam team = itemScore $ head $ intersectTeam team


main = do
    -- Reading file
    handle <- openFile "cal03-1.test.txt" ReadMode
    contents <- hGetContents handle

    -- Main logic
    let rucksacks = splitRucksacks contents
        -- Teams of three rucksacks
        rucksacks_teams = chunksOf 3 rucksacks
        -- Part #1 - score for each rucksack
        rucksacks_score_part1 = sum $ map (scoreRucksack) rucksacks
        -- Part #2 - score for each team
        rucksacks_score_part2 = sum $ map (scoreTeam) rucksacks_teams

    -- Output handling
    -- print rucksacks
    print rucksacks_score_part1
    print rucksacks_score_part2

    -- Closing file
    hClose handle
