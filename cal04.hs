#!/usr/bin/env runhaskell

import System.IO
import Data.Bool (bool)
import Data.List (intersect)
import Data.List.Split (splitOn)

type AssignmentPair a = ((a, a), (a, a))

-- Split input into assignment pairs
splitAssignmentPairs :: (Read a) => String -> [AssignmentPair a]
splitAssignmentPairs fileContents =
    map (
        (\[[xs,xe],[ys,ye]] -> ((xs,xe),(ys,ye))).  -- make pairs from ranges
        map (map read.splitOn "-"). -- split pair range and convert it to int
        splitOn "," -- split pairs
    ) $ lines fileContents

-- Is an assignment fully inside the other assignment?
redundantAssignmentPair :: (Ord a) => AssignmentPair a -> Bool
redundantAssignmentPair ((xs,xe),(ys,ye)) = (xs >= ys && xe <= ye) || (ys >= xs && ye <= xe)

-- Does an assignment overlap with the other assignment?
overlappingAssignmentPair :: (Enum a, Eq a) => AssignmentPair a -> Bool
overlappingAssignmentPair ((xs,xe),(ys,ye)) = not $ null $ intersect x y
    where x = [xs..xe]
          y = [ys..ye]

main = do
    -- Reading file
    contents <- readFile "cal04-1.test.txt"
    -- contents <- readFile "cal04-1.orig.txt"

    -- Main logic
    let assignmentPairs = splitAssignmentPairs contents :: [AssignmentPair Int]
        -- Part #1 - completely overlapping assignment pairs
        redundantAssignmentPairs_part1 = length $ filter (redundantAssignmentPair) assignmentPairs
        -- Part #2 - completely and partially overlapping assignment pairs
        overlappingAssignmentPairs_part2 = length $ filter (overlappingAssignmentPair) assignmentPairs

    -- Output handling
    -- print assignmentPairs
    print redundantAssignmentPairs_part1
    print overlappingAssignmentPairs_part2
