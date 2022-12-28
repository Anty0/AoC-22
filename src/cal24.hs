#!/usr/bin/env runhaskell

-- import Debug.Trace (traceShowId, traceShow, trace)
import Data.List (sortBy, groupBy)
-- import Data.Ix (range)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (Array, (!))
import qualified Data.Array as Array
-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HashMap
import Data.Heap (Heap)
import qualified Data.Heap as Heap


-- Index in a 2D grid
type Index = (Int, Int)

-- Path in 2D grid
type Path = [Index]

-- Item of queue
data QueueItem a b = QueueItem {
    queueItemPrice :: a,
    queueItemValue :: b
} deriving (Show, Eq)

-- Sort queue by item key
instance (Ord a, Eq b) => Ord (QueueItem a b) where
    compare o1 o2 = compare (queueItemPrice o1) (queueItemPrice o2)

-- Queue sorted by a
type PriorityQueue a b = Heap (QueueItem a b)

-- Moving object
data Actor = Actor {
    actorOffset :: Int,
    actorSpeed :: Int
} deriving (Show, Eq)

-- Map of horizontally and vertically moving objects
data ActorsMap = ActorsMap {
    actorsMapRange :: (Index, Index),
    actorsMapHorizontal :: Array Int [Actor],
    actorsMapVertical :: Array Int [Actor],
    actorsMapStart :: Index,
    actorsMapEnd :: Index
} deriving (Show, Eq)


-- Insert into sorted queue
queueInsert :: (Ord a, Eq b) => (a, b) -> PriorityQueue a b -> PriorityQueue a b
queueInsert (key, value) = Heap.insert (QueueItem key value)

-- Get top of queue and rest of queue as pair
queueTop :: PriorityQueue a b -> ((a, b), PriorityQueue a b)
queueTop queue = ((\o -> (queueItemPrice o, queueItemValue o)) $ Heap.minimum queue, Heap.deleteMin queue)

-- Get whole queue as list with random order
queueUnpack :: PriorityQueue a b -> [(a, b)]
queueUnpack = map (\o -> (queueItemPrice o, queueItemValue o)) . Heap.toUnsortedList


-- Valid grid indexes next to an index
gridNeighbors :: ActorsMap -> Index -> [Index]
gridNeighbors actors (x, y) = filter validIndex $ [(x, y), (x+1,y), (x-1, y), (x,y+1), (x, y-1)]
    where
        start = actorsMapStart actors
        end = actorsMapEnd actors
        ((sx, sy), (ex, ey)) = actorsMapRange actors
        validIndex (tx, ty)
            | (tx, ty) == start = True
            | (tx, ty) == end = True
            | tx < sx  = False
            | ty < sy  = False
            | tx > ex  = False
            | ty > ey  = False
            | otherwise = True


-- Parse input into map of actors
parseInput :: String -> ActorsMap
parseInput input = ActorsMap {
        actorsMapRange = ((minX, minY), (maxX, maxY)),
        actorsMapHorizontal = Array.array (minY, maxY) ([(y, []) | y <- [minY..maxY]] ++ actorsX),
        actorsMapVertical = Array.array (minX, maxX) ([(x, []) | x <- [minX..maxX]] ++ actorsY),
        actorsMapStart = fst $ head $ filter ((==) 0 . snd . fst) $ filter ((==) '.' . snd) inputElements,
        actorsMapEnd = fst $ head $ filter ((==) (maxY+1) . snd . fst) $ filter ((==) '.' . snd) inputElements
    }
    where
        inputElements =
            concat $
            map (\(y, l) ->
                map (\(x, c) -> ((x, y), c)) $
                zip [0..] l
            ) $
            zip [0..] $
            lines input
        wallElements = filter ((==) '#' . snd) inputElements
        wallX = map (fst.fst) wallElements
        wallY = map (snd.fst) wallElements
        minX = (minimum wallX + 1)
        minY = (minimum wallY + 1)
        maxX = (maximum wallX - 1)
        maxY = (maximum wallY - 1)
        actorsRight = map (\(x, y) -> (y, Actor {actorOffset = x, actorSpeed = 1})) $ map fst $ filter ((==) '>' . snd) inputElements
        actorsLeft = map (\(x, y) -> (y, Actor {actorOffset = x, actorSpeed = -1})) $ map fst $ filter ((==) '<' . snd) inputElements
        actorsDown = map (\(x, y) -> (x, Actor {actorOffset = y, actorSpeed = 1})) $ map fst $ filter ((==) 'v' . snd) inputElements
        actorsUp = map (\(x, y) -> (x, Actor {actorOffset = y, actorSpeed = -1})) $ map fst $ filter ((==) '^' . snd) inputElements
        actorsX = map (\l -> (fst $ head l, map snd l)) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) (actorsRight ++ actorsLeft)
        actorsY = map (\l -> (fst $ head l, map snd l)) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) (actorsDown ++ actorsUp)


-- Recursive function to preform path search in a grid
gridPathSearchStep :: (Ord a) => ActorsMap -> (Int -> Index -> Bool) -> (Int -> Index -> Index -> Bool) ->
    (Int -> Index -> Index -> a -> a) -> Set (Index, Int) -> PriorityQueue a (Path, Int) -> Maybe (Path, Int)
gridPathSearchStep actors goal filterFnc priceFnc visited queue
    -- No path found
    | Heap.null queue = Nothing
    -- Goal was found - return it's path
    | goal time current = Just (path, time)
    -- Current is not yet goal - expand it and continue
    -- Take an item from top of queue and expand it
    | otherwise    = gridPathSearchStep actors goal filterFnc priceFnc nextVisited nextQueue
    where
        -- Expand queue to get top item and it's info
        ((price, (path@(current:_), time)), qs) = queueTop queue
        -- Time for next index
        nextTime = time + 1
        -- Expanded neighbor indexes for current index
        nextIndexes =
            filter (not . flip Set.member visited) $ -- 4. Remove already processed neighbors
            map (\i -> (i, nextTime)) $ -- 3. Add time to the index
            filter (filterFnc nextTime current) $ -- 2. Remove unwanted neighbors
            gridNeighbors actors current -- 1. All available neighbors of current
        -- Updated visited set to include new indexes
        nextVisited = foldr Set.insert visited nextIndexes
        -- Updated queue to include new indexes
        nextQueue =
            -- 2. Insert them all to queue (but keep first item of queue removed)
            foldr queueInsert qs $
            -- 1. Calculate new price for each index and create pair of price and path with new index
            map (\(i, t) -> (priceFnc t current i price, (i:path, t))) nextIndexes

-- Perform path search in a grid
gridPathSearch :: (Ord a) => Index -> (Int -> Index -> Bool) -> (Int -> Index -> Index -> Bool) ->
    (Int -> Index -> Index -> a -> a) -> a -> Int -> ActorsMap -> Maybe (Path, Int)
gridPathSearch start goal filterFnc priceFnc initialPrice initialTime actors =
        gridPathSearchStep actors goal filterFnc priceFnc initialVisited initialQueue
    where
        initialVisited = Set.fromList [(start, initialTime)]
        initialQueue = foldr queueInsert Heap.empty [(initialPrice, ([start], initialTime))]

-- Calculate Manhattan distance
distance :: Index -> Index -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Calculate position of an actor based on current time
actorPosition :: Int -> (Int, Int) -> Actor -> Int
actorPosition time (minPos, maxPos) actor = ((speed * time + offset - minPos) `mod` (maxPos - minPos + 1)) + minPos
    where
        offset = actorOffset actor
        speed = actorSpeed actor

-- Filter out positions which are occupied based on current time
filterOccupiedAtTime :: ActorsMap -> Int -> Index -> Index -> Bool
filterOccupiedAtTime actors time _ next@(x, y)
    | next == start = True -- Always allow stepping on start location
    | next == end = True -- Always allow stepping on end location
    | otherwise = lineFree && columnFree
    where
        start = actorsMapStart actors
        end = actorsMapEnd actors
        ((sx, sy), (ex, ey)) = actorsMapRange actors
        line = actorsMapHorizontal actors ! y
        column = actorsMapVertical actors ! x
        lineFree = null $ filter ((==) x) $ map (actorPosition time (sx, ex)) line
        columnFree = null $ filter ((==) y) $ map (actorPosition time (sy, ey)) column

-- Heuristic based on distance from target and passed time
priceHeuristicDistance :: Index -> Int -> Index -> Index -> Int -> Int
priceHeuristicDistance end time _ next _ = distance next end + time

-- Visual representation of actors map with path at given time
actorsMapPathStr :: ActorsMap -> Path -> Int -> String
actorsMapPathStr actors path time = concat [
        [
            actorsMapChar x y
            | x <- [sx-1..ex+1]
        ] ++ "\n"
        | y <- [sy-1..ey+1]
    ]
    where
        pos = path !! time
        start = actorsMapStart actors
        end = actorsMapEnd actors
        ((sx, sy), (ex, ey)) = actorsMapRange actors
        actorsMapChar x y
            | (x, y) == start = 'S'
            | (x, y) == end = 'E'
            | x < sx = '#'
            | x > ex = '#'
            | y < sy = '#'
            | y > ey = '#'
            | not $ filterOccupiedAtTime actors time (x, y) (x, y) = '*'
            | (x, y) == pos = 'X'
            | otherwise = '.'


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Parse input to map of moving actors
        actors = parseInput contents

        -- Some functions for path search
        filterFnc = filterOccupiedAtTime actors
        priceFnc = priceHeuristicDistance (actorsMapEnd actors)

        -- Definition of start positions and goal positions
        start = actorsMapStart actors
        end = actorsMapEnd actors
        goalStart _ i = i == start
        goalEnd _ i = i == end

        -- Path from start to end
        Just (path1, time1) = gridPathSearch start goalEnd filterFnc priceFnc 0 0 actors
        -- Path back from end to start
        Just (path2, time2) = gridPathSearch end goalStart filterFnc priceFnc 0 time1 actors
        -- Path again from start to end
        Just (path3, time3) = gridPathSearch start goalEnd filterFnc priceFnc 0 time2 actors

        -- Part #1 - Time of one-way trip (start->end)
        resultP1 = time1
        -- Part #2 - (Bonus) Time of two-way trip (start->end->start)
        resultP2 = time2
        -- Part #3 - Time of three-way trip (start->end->start->end)
        resultP3 = time3

    -- Output handling
    -- putStrLn $
    --     concat $
    --     map ((++) "\n") $
    --     map (actorsMapPathStr actors (reverse path1)) [0..length path1-1]
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "' | '" ++
        show resultP3 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal24-1.test.txt" -- 10 | 20 | 30
    printResult "./data/cal24-2.test.txt" -- 18 | 41 | 54
    -- printResult "./data/cal24-1.orig.txt"
