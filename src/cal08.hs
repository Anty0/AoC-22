#!/usr/bin/env runhaskell

import Data.Array (Array, (!), listArray, indices)
import Data.List (sortBy)
import Data.List.Extra (headDef)
import Data.Function (on)


-- Index in a 2D grid
type Index = (Int, Int)

-- 2D grid
type Grid a = Array Index a


-- Parse single character
readChar :: Read a => Char -> a
readChar c = read [c]

-- Parse input and 2D grid
parseInput :: Read a => String -> Grid a
parseInput input = listArray range values
    where inputLines = lines input
          -- Width of grid
          width = length $ head inputLines
          -- Height of grid
          height = length $ inputLines
          -- Range of grid indices
          range = ((0, 0), (height-1, width-1))
          -- List of values for indices
          values = map readChar $ concat inputLines

-- Is index directly on top of another index?
top :: Index -> Index -> Bool
top (sx, sy) (tx, ty) = tx < sx && ty == sy

-- Is index directly on bottom of another index?
bottom :: Index -> Index -> Bool
bottom (sx, sy) (tx, ty) = tx > sx && ty == sy

-- Is index directly to the left of another index?
left :: Index -> Index -> Bool
left (sx, sy) (tx, ty) = tx == sx && ty < sy

-- Is index directly to the right of another index?
right :: Index -> Index -> Bool
right (sx, sy) (tx, ty) = tx == sx && ty > sy

-- Is index directly on any side of another index?
line :: Index -> Index -> Bool
line s t = or $ sequence (sequence [top, bottom, left, right] s) t

-- Coordinate distance between indexes
distance :: Index -> Index -> Int
distance (sx, sy) (tx, ty) = abs (sx - tx) + abs (sy - ty)

-- One list for each side of grid, contains indices on that side
lineIndices :: Grid a -> Index -> [[Index]]
lineIndices grid index = map select filters
    where gridIndices = indices grid
          -- Filter indices by given rule
          select f = filter (f) gridIndices
          -- List of filter functions one for each side of index
          filters = sequence [top, bottom, left, right] index

-- Predicate, True if index is visible from any side.
-- Visibility is defined as: all numbers on side are lower then index
-- number, then index is visible from that side
isVisible :: (Ord a) => Grid a -> Index -> Bool
isVisible grid index = or $ map (all shorter) $ lineIndices grid index
    where
        -- Index number in grid
        self = grid ! index
        -- Is index number lower then number of i
        shorter i = (grid ! i) < self

-- Calculate score of index in grid - product of distances to higher number on each side
score :: (Ord a) => Grid a -> Index -> Int
score grid index = product $ map evaluate weights
    where
        -- Indices for each side of index
        indexLineIndices = lineIndices grid index
        -- Weights for indices - sort indices by distance from index and use their index in list as their value
        weights = map (zip [1..] . sortBy (compare `on` (distance index))) indexLineIndices
        -- Evaluate weights for one side - first (lowest) found weight or length of list as fallback
        evaluate w = (
                headDef (length w) $  -- 3. Take first index or fallback to length of w
                map (fst) $           -- 2. Throw away values and keep indexes
                filter (higher.snd) w -- 1. Keep only higher values
            )
        -- Index number in grid
        self = grid ! index
        -- Is index number higher or equal to number of i
        higher i = (grid ! i) >= self

-- How many indices are visible in a grid?
-- Can be optimized to use bitmap (Grid of booleans) to mark
-- what is / isn't visible.
resultPart1 :: (Ord a) => Grid a -> Int
resultPart1 grid = length $ filter (isVisible grid) $ indices grid

-- Maximum possible score of single index in grid. Score calculated using score function.
resultPart2 :: (Ord a) => Grid a -> Int
resultPart2 grid = maximum $ map (score grid) $ indices grid


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let grid = parseInput contents :: Grid Int
        -- Part #1
        resultP1 = resultPart1 grid
        -- Part #2
        resultP2 = resultPart2 grid

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++ show resultP1 ++ "' | '" ++ show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal08-1.test.txt" -- 21 | 8
    -- printResult "./data/cal08-1.orig.txt"
