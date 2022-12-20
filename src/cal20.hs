#!/usr/bin/env runhaskell

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (dupe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap


-- Efficiency of this implementation is very low.
-- It is using Bidirectional Map to keep track of indices mapping.
-- It is possible in other languages to use two arrays to keep track
-- of mappings with O(1) efficiency for search.
-- I don't know how to do the same in haskell.


-- Parse input into list of numbers
parseInput :: String -> [Int]
parseInput = map read . -- 2. Parse each line to number
            lines -- 1. List of lines
            

-- Make array from list of numbers
asArray :: [Int] -> Array Int Int
asArray numbers = Array.array (0, length numbers - 1) $ zip [0..] numbers

-- Make indices mapping from list of numbers
indicesMap :: Array Int Int -> Bimap Int Int
indicesMap = Bimap.fromList . map dupe . Array.indices

-- Helper function to move all significant indexes by one up or down
moveHelper :: Int -> Int -> Bimap Int Int -> Bimap Int Int
moveHelper from to indicesMap
    | from == to = indicesMap
    | from < to = foldl (\m x -> Bimap.adjust (flip (-) 1) x m) indicesMap toDecrease
    | otherwise = foldr (\x m-> Bimap.adjust ((+) 1) x m) indicesMap toIncrease
    where
        toDecrease = map (fromJust . flip Bimap.lookupR indicesMap) [from+1..to]
        toIncrease = map (fromJust . flip Bimap.lookupR indicesMap) [to..from-1]

-- Move index in mapping from given index to another index
move :: Int -> Int -> Bimap Int Int -> Bimap Int Int
move from to indicesMap = Bimap.insert toMove to $ moveHelper from to indicesMap
    where toMove = fromJust $ Bimap.lookupR from indicesMap

-- Move index in mapping based on value of index in array
moveWith :: Array Int Int -> Int -> Bimap Int Int -> Bimap Int Int
moveWith numbers originalIndex indicesMap = move from to indicesMap
    where
        (0, e) = Array.bounds numbers
        from = fromJust $ Bimap.lookup originalIndex indicesMap
        to = ((from + numbers ! originalIndex - 1) `mod` e) + 1

-- Resolve indices mapping to list of numbers
resolveMapping :: Array Int Int -> Bimap Int Int -> [Int]
resolveMapping numbers = map ((!) numbers . snd) . Bimap.toAscListR

-- Go through all numbers and move each number in mapping based on its value
moveAll :: Array Int Int -> Bimap Int Int -> Bimap Int Int
moveAll numbers indicesMap = foldl (\m x -> moveWith numbers x m) indicesMap [s..e]
    where (s, e) = Array.bounds numbers

-- Calculate task result:
-- - multiply numbers by 'multiplier',
-- - apply moveAll 'shuffleRepeat' times,
-- - find index of 'startNumber',
-- - find numbers with indexes from 'startNumber' of 'countNumbersIndexes',
-- - sum those numbers
result :: [Int] -> Int -> Int -> Int -> [Int] -> Int
result numbersList multiplier shuffleRepeat startNumber countNumbersIndexes = sum countNumbers
    where
        numbers = asArray $ map ((*) multiplier) numbersList
        numbersIndices = indicesMap numbers
        shuffledIndices = foldr (\_ -> moveAll numbers) numbersIndices [1..shuffleRepeat]
        shuffledNumbers = resolveMapping numbers shuffledIndices
        Just startIndex = elemIndex startNumber shuffledNumbers
        indexLimit = length shuffledNumbers
        fixIndex num = num `mod` indexLimit
        countNumbersRealIndexes = map fixIndex $ map ((+) startIndex) countNumbersIndexes
        countNumbers = map ((!!) shuffledNumbers) countNumbersRealIndexes


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Task specific constants
    let startNumber = 0
        countIndexes = [1000, 2000, 3000]

    -- Main logic
    let -- Parse input to array of blueprints
        numbersList = parseInput contents

        -- Part #1 - No multiplier, apply once
        resultP1 = result numbersList 1 1 startNumber countIndexes
        -- Part #2 - Multiplier 811589153, apply 10 times
        resultP2 = result numbersList 811589153 10 startNumber countIndexes

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal20-1.test.txt" -- 3 | 1623178306
    printResult "./data/cal20-1.orig.txt"
