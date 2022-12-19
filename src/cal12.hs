#!/usr/bin/env runhaskell

import Data.List (insertBy, elemIndex, intercalate)
import Data.Array (Array, (!), listArray, bounds)
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import qualified Data.Set as Set


-- Index in a 2D grid
type Index = (Int, Int)

-- 2D grid
type Grid a = Array Index a

-- Path in 2D grid
type Path = [Index]

-- Queue sorted by a
type Queue a = [(a, Path)]


-- Insert into sorted queue
queueInsert :: (Ord a) => (a, Path) -> Queue a -> Queue a
queueInsert (price, path) = insertBy (compare `on` fst) (price, path)


-- Parse single character to grid value
gridValueFromChar :: Char -> Int
-- Assume 'S' is an 'a'
gridValueFromChar 'S' = ord 'a' - ord 'a'
-- Assume 'E' is an 'z'
gridValueFromChar 'E' = ord 'z' - ord 'a'
gridValueFromChar c   = ord c   - ord 'a'

-- Parse grid value into single character
gridValueToChar :: Int -> Char
gridValueToChar v = chr $ v + ord 'a'


-- Valid grid indexes next to an index
gridNeighbors :: Grid a -> Index -> [Index]
gridNeighbors grid (y, x) = filter validIndex $ [(y+1,x), (y-1, x), (y,x+1), (y, x-1)]
    where
        ((sy, sx), (ey, ex)) = bounds grid
        validIndex (ty, tx)
            | tx < sx  = False
            | ty < sy  = False
            | tx > ex  = False
            | ty > ey  = False
            | otherwise = True


-- Parse input and 2D grid
parseInput :: String -> (Index, Index, Grid Int)
parseInput input = (sIndex, eIndex, listArray range values)
    where
        inputLines = lines input
        -- Width of grid
        width = length $ head inputLines
        -- Height of grid
        height = length $ inputLines
        -- Range of grid indices
        range = ((0, 0), (height-1, width-1))
        -- Input as single list
        inputList = concat inputLines
        -- List of values for indices
        values = map gridValueFromChar inputList
        -- Index of 'S' in input
        Just sElemIndex = elemIndex 'S' inputList
        sIndex = (sElemIndex `div` width, sElemIndex `mod` width)
        -- Index of 'E' in input
        Just eElemIndex = elemIndex 'E' inputList
        eIndex = (eElemIndex `div` width, eElemIndex `mod` width)


-- Recursive function to preform path search in a grid
gridPathSearchStep :: (Ord a) => Grid a -> (Index -> Bool) -> (Index -> Index -> Bool) ->
    (Index -> Index -> a -> a) -> Set.Set Index -> Queue a -> Maybe Path
-- No path found
gridPathSearchStep _ _ _ _ _ [] = Nothing
-- Take an item from top of queue and expand it
gridPathSearchStep grid goal filterFnc priceFnc visited queue
    -- Goal was found - return it's path
    | goal current = Just path
    -- Current is not yet goal - expand it and continue
    | otherwise    = gridPathSearchStep grid goal filterFnc priceFnc nextVisited nextQueue
    where
        -- Expand queue to get top item and it's info
        ((price, path@(current:_)) : qs) = queue
        -- Expanded neighbor indexes for current index
        nextIndexes =
            filter (not . flip Set.member visited) $ -- 3. Remove already processed neighbors
            filter (filterFnc current) $ -- 2. Remove unwanted neighbors
            gridNeighbors grid current -- 1. All available neighbors of current
        -- Updated visited set to include new indexes
        nextVisited = foldr Set.insert visited nextIndexes
        -- Updated queue to include new indexes
        nextQueue =
            -- 2. Insert them all to queue (but keep first item of queue removed)
            foldr queueInsert qs $
            -- 1. Calculate new price for each index and create pair of price and path with new index
            map (\i -> (priceFnc current i price, i:path)) nextIndexes

-- Perform path search in a grid
gridPathSearch :: (Ord a) => Index -> (Index -> Bool) -> (Index -> Index -> Bool) ->
    (Index -> Index -> a -> a) -> a -> Grid a -> Maybe Path
gridPathSearch start goal filterFnc priceFnc initialPrice grid =
        gridPathSearchStep grid goal filterFnc priceFnc initialVisited initialQueue
    where
        initialVisited = Set.fromList [start]
        initialQueue = [(initialPrice, [start])]


-- Arrow character corresponding to coordinate change
arrowFor :: Int -> Int -> Char
arrowFor 1    0 = 'v'
arrowFor 0    1 = '>'
arrowFor (-1) 0 = '^'
arrowFor 0 (-1) = '<'
arrowFor _    _ = '+'

-- Print 2D grid with optional path
gridPathStr :: Path -> Grid Int -> String
gridPathStr path grid = (
        intercalate "\n" $
        [
            [
                gridChar (y, x)
                | x <- [sx..ex]
            ] | y <- [sy..ey]
        ]
    ) ++ ['\n']
    where
        ((sy, sx), (ey, ex)) = bounds grid
        pathChar index = do
            i <- elemIndex index path
            let (y1, x1) = path !! i
                (y2, x2) = path !! (i-1)
            if i == 0
                then return 'E'
                else return $ arrowFor (y2-y1) (x2-x1)
        gridChar index = fromMaybe (gridValueToChar $ grid ! index) $ pathChar index


-- Filter which allows change of value of index only by one up or by any number down
filterAllowUpByOne :: (Ord a, Num a) => Grid a -> Index -> Index -> Bool
filterAllowUpByOne grid current next = nextValue <= currentValue + 1
    where
        currentValue = grid ! current
        nextValue = grid ! next

-- Price function - price is increasing by one for each step
priceAsPathLength :: (Num a) => Grid a -> Index -> Index -> a -> a
priceAsPathLength grid _ _ value = value + 1


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let (start, end, grid) = parseInput contents
        -- Setup filter for path search
        pathFilter = flip $ filterAllowUpByOne grid
        -- Setup price function for path search
        pathPrice = priceAsPathLength grid
        -- Setup goals for path search
        -- This goal will search for start position
        goalIsStart = (==) start
        -- This goal will search for any position with a 'a' value
        goalIsAnyA = (==) (gridValueFromChar 'a') . (!) grid

        -- Part #1 - Length of path from start to an end
        -- (here we search from end to start, it doesn't really matter)
        Just pathPart1 = gridPathSearch end goalIsStart pathFilter pathPrice 0 grid
        -- Part #2 - Length of path from end to closest tile with 'a'
        Just pathPart2 = gridPathSearch end goalIsAnyA  pathFilter pathPrice 0 grid

    -- Output handling
    -- Original grid
    putStrLn "Original grid:"
    putStrLn $ gridPathStr [] grid
    -- Representation of path for part 1
    putStrLn "Path for part 1:"
    putStrLn $ gridPathStr pathPart1 grid
    -- Representation of path for part 2
    putStrLn "Path for part 2:"
    putStrLn $ gridPathStr pathPart2 grid
    -- Results
    putStrLn $ "'" ++ file ++ "': '" ++
        show (length pathPart1 - 1) ++ "' | '" ++
        show (length pathPart2 - 1) ++ "'\n"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal12-1.test.txt" -- 31 | 29
    -- printResult "./data/cal12-1.orig.txt"
