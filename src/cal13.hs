#!/usr/bin/env runhaskell

import Data.List.Split (splitOn)
import Data.List (sort, intercalate)


-- List with dynamic depth
data ValueList a
    = VValue a
    | VList [ValueList a]
      deriving (Eq)

-- Implement comparison for list
instance (Ord a) => Ord (ValueList a) where
    compare (VValue x) (VValue y) = compare x y
    compare (VList x) (VList y) = compare x y
    compare x@(VValue _) y = compare (VList [x]) y
    compare x y@(VValue _) = compare x (VList [y])

-- Simulate basic list when displaying
instance (Show a) => Show (ValueList a) where
    show (VValue x) = show x
    show (VList x) = '[' : (intercalate "," $ map show x) ++ "]"

-- Representation of pair of two lists
type PacketPair a = (ValueList a, ValueList a)


-- Helper function to parse items of ValueList
valueListParseItems :: (Read a) => String -> (String, [ValueList a])
valueListParseItems (']':xs) = (xs, [])
valueListParseItems text = (xs3, item:items)
    where
        (xs, item) = valueListParseInner text -- Parse first item
        xs2 = if head xs == ',' then tail xs else xs -- Drop separator
        (xs3, items) = valueListParseItems xs2 -- Parse rest of items

-- Helper function to parse item of ValueList
valueListParseInner :: (Read a) => String -> (String, ValueList a)
valueListParseInner ('[':xs) = (xs2, VList items)
    where (xs2, items) = valueListParseItems xs
valueListParseInner text = (remainingText, VValue value)
    where
        takeRule ',' = False
        takeRule ']' = False
        takeRule _   = True
        value =
            read $ -- 2. Parse it
            takeWhile takeRule text -- 1. Take value
        remainingText =
            dropWhile takeRule text -- Drop value

-- Parse value list
valueListParse :: (Read a) => String -> ValueList a
valueListParse input = valueList
    where ("", valueList) = valueListParseInner input


-- Parse input for part 1 - Pairs of packets
parseInputPart1 :: (Read a) => String -> [PacketPair a]
parseInputPart1 =
    map (
        (\[l1, l2] -> (l1, l2)) . -- 4. Make pairs of these lists
        map valueListParse -- 3. Convert each line ValueList
    ) .
    splitOn [""] . -- 2. Split them to blocks
    lines -- 1. Take individual lines

-- Parse input for part 2 - List of packets
parseInputPart2 :: (Read a) => String -> [ValueList a]
parseInputPart2 =
    map valueListParse . -- 3. Convert each line ValueList
    filter (/= "") . -- 2. Skip empty lines
    lines -- 1. Take individual lines


-- Part 1 - Sum of indexes of all correctly sorted pairs
resultPart1 :: (Ord a) => [PacketPair a] -> Int
resultPart1 =
    sum . -- 5. Sum all remaining indexes
    map fst . -- 4. Throw away packets and keep only indexes
    filter snd . -- 3. Keep only pairs with correct order
    zip [1..] . -- 2. Add index
    map (uncurry (<)) -- 1. Compare pair

-- Part 2 - Product of indexes of dividers in sorted packets
resultPart2 :: (Eq a, Ord a, Num a) => [ValueList a] -> Int
resultPart2 packets =
        product $ -- 5. Calculate product of indexes of dividers
        map fst $ -- 4. Throw away packets and keep only indexes
        filter (
            (\p -> p == divider1 || p == divider2) . -- 3. Keep only dividers
            snd
        ) $
        zip [1..] $ -- 2. Add index
        sort (divider1:divider2:packets) -- 1. Sort packets with dividers
    where
        -- Dividers
        divider1 = VList [VList [VValue 2]]
        divider2 = VList [VList [VValue 6]]


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Read pairs of packets for part 1
        packetPairs = parseInputPart1 contents :: [PacketPair Int]
        -- Read list of packets for part 2
        packets = parseInputPart2 contents -- :: [ValueList Int]

        -- Part #1
        resultP1 = resultPart1 packetPairs
        -- Part #2
        resultP2 = resultPart2 packets

    -- Output handling
    -- putStrLn "Pocket pairs:"
    -- putStrLn $ concat $ map (flip (++) "\n" . show) packetPairs
    -- putStrLn "Packets:"
    -- putStrLn $ concat $ map (flip (++) "\n" . show) packets
    -- Results
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal13-1.test.txt" -- 13 | 140
    -- printResult "./data/cal13-1.orig.txt"
