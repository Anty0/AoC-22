#!/usr/bin/env runhaskell

import System.IO
-- import Debug.Trace (trace)
import Data.List (transpose, replicate)
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn, chunksOf)


-- Call function on top of element selected by index
applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt _ _ [] = []
applyAt 0 f (x:xs) = f x:xs
applyAt n f (x:xs) = x:applyAt (n-1) f xs

-- Take n elements from top of stack
takeAt :: Int -> Int -> [[a]] -> [a]
takeAt index n stacks = take n $ stacks !! index

-- Heads of all lists in 2D list
header :: [[a]] -> [a]
header = map (head)

-- Process input string into list of stacks and list of commands,
-- optionally split commands into single move steps
parse :: String -> Bool -> ([[Char]], [(Int, Int, Int)])
parse input singleStep = (stacks, commands)
    where
        -- Split stacks and commands by an empty line
        [stacksStr, commandsStr] = splitOn [""] $ lines input

        -- Split line into columns, remove space at the and of come
        -- columns and parse item in each column
        parseStackLine line = map (parseStackItem.take 3) $ chunksOf 4 line

        -- Either there are no more items in stack
        parseStackItem "   " = Nothing
        -- Or there is an item in bracket - "[A]"
        parseStackItem ['[', c,']'] = Just c
        -- Optionally debug invalid columns
        -- parseStackItem a = trace a Nothing

        -- Remove last line from stacks (useless stack indexes),
        -- parse lines and then use transpose, to make a list for each column
        -- (instead of list for each line), finally remove Nothings and keep only
        -- values of Justs using catMaybe
        stacks = map (catMaybes) $ transpose $ map (parseStackLine) $ init stacksStr

        -- Read command line (which is split by spaces) into three ints - n, x, y
        -- - n = how many items should be moved
        -- - x = from which stack should we take them
        -- - y = into which stack should we put them
        --
        -- If singleStep is set to true, then commands with n higher
        -- then 1 will be split into n commands where each command has n=1
        -- Returns list of commands for each line - to allow for
        -- splitting commands into multiple commands
        parseCommandLine ["move", n, "from", x, "to", y] = if singleStep
            then replicate (read n) (1, read x - 1, read y - 1)
            else [(read n, read x - 1, read y - 1)]

        -- Split command line by spaces to simplify matching in parseCommandLine,
        -- parse lines into lists of commands and them flatten output into list of commands
        commands = concat $ map (parseCommandLine.splitOn " ") commandsStr

-- Apply commands to stacks and return modified stacks
apply :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
apply stacks [] = stacks
-- apply stacks ((x,y):xc) = trace (show stacks) $ apply newStacks xc
apply stacks ((n,x,y):xc) = apply newStacks xc
    where newStacks = applyAt y (takeAt x n stacks ++) $ applyAt x (drop n) stacks

-- Parse input, apply commands to stacks, get stacks header
result :: String -> Bool -> String
result input singleStep = header $ uncurry (apply) $ parse input singleStep

main = do
    -- Reading file
    contents <- readFile "./data/cal05-1.test.txt" -- CMZ | MCD
    -- contents <- readFile "./data/cal05-1.orig.txt"

    -- Part #1 - heads of stacks, commands executed element by element
    print $ result contents True
    -- Part #2 - heads of stacks, commands executed for all elements at the same time
    print $ result contents False
