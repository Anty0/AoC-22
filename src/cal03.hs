#!/usr/bin/env runhaskell

import System.IO
import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)


-- Split list into two halves
halveList :: [a] -> ([a], [a])
halveList l = splitAt len l
    where len = (length l) `div` 2

-- Calculate score of item
itemScore :: Char -> Int
itemScore c
    | ord c >= ord 'a' = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

-- Split input into rucksacks
splitRucksacks :: String -> [String]
splitRucksacks fileContents = lines fileContents

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
    contents <- readFile "./data/cal03-1.test.txt" -- 157 | 70
    -- contents <- readFile "./data/cal03-1.orig.txt"

    -- Main logic
    let rucksacks = splitRucksacks contents
        -- Teams of three rucksacks
        rucksacksTeams = chunksOf 3 rucksacks
        -- Part #1 - score for each rucksack
        rucksacksScore_part1 = sum $ map (scoreRucksack) rucksacks
        -- Part #2 - score for each team
        rucksacksScore_part2 = sum $ map (scoreTeam) rucksacksTeams

    -- Output handling
    -- print rucksacks
    print rucksacksScore_part1
    print rucksacksScore_part2
