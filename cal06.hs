#!/usr/bin/env runhaskell

import System.IO
import Data.List (find, nub)
import Data.Maybe (fromMaybe)


-- List of lists representing sliding window
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow _ [] = []
slidingWindow s l@(_:lt)
  | length l <= s = window:[]
  | otherwise = window:slidingWindow s lt
  where window = take s l

-- Is each character in string unique?
unique :: String -> Bool
unique l = length l == length (nub l)

-- Create sliding window, add index to it using zip and find first occurrence of unique packet
result :: Int -> String -> (Int, String)
result len input = fromMaybe (-1, "") $ find (unique.snd) $ zip [len..] $ slidingWindow len input

printResult file = do
    -- Reading file
    contents <- readFile file

    -- Part #1 - first unique sequence of length 4
    let resultPart1 = result 4 contents
    -- Part #2 - first unique sequence of length 14
    let resultPart2 =  result 14 contents

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++ show resultPart1 ++ "' | '" ++ show resultPart2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "cal06-1.test.txt" -- 7 19
    printResult "cal06-2.test.txt" -- 5 23
    printResult "cal06-3.test.txt" -- 6 23
    printResult "cal06-4.test.txt" -- 10 29
    printResult "cal06-5.test.txt" -- 11 26
    -- printResult "cal06-1.orig.txt"
