#!/usr/bin/env runhaskell

import System.IO
import Data.List (intercalate)
import Data.List.Split (chunksOf)


-- Available operations
data Operation a = ONop
                 | OAdd a
                 deriving (Show, Eq)


-- Convert line to list of operations
tokenize :: (Read a) => [String] -> [Operation a]
tokenize ["noop"] = [ONop]
tokenize ["addx", n] = [ONop, OAdd (read n)]

-- Convert input string to list of operations for simulation
parseInput :: (Read a) => String -> [Operation a]
parseInput input = concat $ map (tokenize.words) $ lines input

-- Apply operation to value
applyOp :: (Num a) => Operation a -> a -> a
applyOp ONop a = a
applyOp (OAdd v) a = a + v

-- Simulate running operations. Return list of states of value
simulate :: (Num a) => [Operation a] -> a -> [a]
simulate [] a = [a]
simulate (op:ops) a = a : simulate ops (applyOp op a)

-- Render one point of screen
renderPoint :: Int -> Int -> Char
renderPoint i a
    | i < a - 1 = '.'
    | i > a + 1 = '.'
    | otherwise = '#'

-- Render line of screen
renderLine :: [Int] -> String
renderLine = map (uncurry renderPoint) . zip [0..]

-- Render whole screen
renderScreen :: [Int] -> String
renderScreen = concat . map ((\s -> s ++ "\n") . renderLine) . chunksOf 40

-- Take states at cycles 20,60,100,140,.. and calculate sum of products of cycle and value
resultPart1 :: [Int] -> Int
resultPart1 = sum . map (uncurry (*)) . zip [20,60..] . map head . chunksOf 40 . drop 19

printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let operations = parseInput contents
        -- Simulate operations starting with value 1
        simulation = simulate operations 1
        -- Part #1
        resultP1 = resultPart1 simulation
        -- Part #2
        screen = renderScreen simulation

    -- Output handling
    -- print operations
    -- print simulation
    putStrLn $ "'" ++ file ++ "': '" ++ show (resultP1) ++ "'\n"
    putStrLn screen

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal10-1.test.txt" -- 0
    printResult "./data/cal10-2.test.txt" -- 13140
    -- printResult "./data/cal10-1.orig.txt"
