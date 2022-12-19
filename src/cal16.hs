#!/usr/bin/env runhaskell

import System.IO
import Debug.Trace (traceShow)
import Data.List (sort)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (fromJust)
import Data.Char (ord, chr)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Heap (Heap)
import qualified Data.Heap as Heap


-- (unused)
-- Maximal flow rate (for heuristics)
-- maxFlow :: (Num a) => a
-- maxFlow = fromInteger 25


-- Item of queue
data QueueItem a b = QueueItem {
    queueItemKey :: a,
    queueItemValue :: b
} deriving (Show, Eq)

-- Sort queue by item key
instance (Ord a, Eq b) => Ord (QueueItem a b) where
    compare o1 o2 = compare (queueItemKey o1) (queueItemKey o2)

-- Queue sorted by a
type PriorityQueue a b = Heap (QueueItem a b)

-- Insert into sorted queue
queueInsert :: (Ord a, Eq b) => (a, b) -> PriorityQueue a b -> PriorityQueue a b
queueInsert (key, value) = Heap.insert (QueueItem key value)

-- Get top of queue and rest of queue as pair
queueTop :: PriorityQueue a b -> ((a, b), PriorityQueue a b)
queueTop queue = ((\o -> (queueItemKey o, queueItemValue o)) $ Heap.minimum queue, Heap.deleteMin queue)

-- Get whole queue as list with random order
queueUnpack :: PriorityQueue a b -> [(a, b)]
queueUnpack = map (\o -> (queueItemKey o, queueItemValue o)) . Heap.toUnsortedList


-- Path - ordered list
type Path = [IntMap.Key]
-- Path - unordered set for searching
type PathSet = Set IntMap.Key


-- Representation of value of Path
data Value = Value {
    valueRemainingTime :: Int,
    valueCurrentFlowRate :: Int,
    valueFlowReleased :: Int
} deriving (Show, Eq)

-- Heuristic for sorting paths in queue
valueAsInt :: Value -> Int

-- Prioritize exploring paths with low remaining time - paths with low branching expected
-- To be honest, I don't understand why this one is the best, but I know this:
-- - It will quickly find a path with relatively big score for the "worst-case scenario"
-- -- This is good as higher is score for the "worst-case scenario", more branches can be eliminated
-- - Once we reach the end of remaining time it starts to prioritize useless options - options which will be skipped
-- -- I'd expect this to be an issue, but in reality it keeps length of priority queue small. Small queue = small insert overhead
--
-- With all that said, I believe this means use of priority queue here was a bad idea
-- and unordered queue should perform similarly to priority one with this heuristic.
valueAsInt v = valueRemainingTime v

-- Prioritize exploring paths with high current flow rate
-- Similar results to prioritizing low remaining time
-- valueAsInt v = -valueCurrentFlowRate v

-- Prioritize low remaining time, then current flow rate
-- Performs worse then just using remaining time
-- valueAsInt v = valueRemainingTime v ^ maxFlow + valueCurrentFlowRate v

-- Prioritize based on the "best-case scenario" - search paths which are expected to have best possible outcome
-- Unfortunately this prioritizes paths with high remaining time, since paths with low remaining
-- time have by definition worse score for the "best-case scenario"
-- As result the algorithm cannot remove most of search branches and queue gets pretty long - high insert overhead
-- valueAsInt v = (valueCurrentFlowRate v) + (fst $ foldl (\ (c, r) rn -> (c + r + rn, r + rn)) (0, valueCurrentFlowRate v) (take (valueRemainingTime v) (repeat maxFlow)))

-- One of tests of heuristic - didn't help
-- valueAsInt v = - ((valueRemainingTime v) * maxFlow + (valueCurrentFlowRate v))

-- Prioritize paths with highest current flow rate multiplied by remaining time - didn't work
-- valueAsInt v = (- valueCurrentFlowRate v) * valueRemainingTime v

-- Compare values based on heuristic - for priority queue
instance Ord Value where
    compare v1 v2 = compare (valueAsInt v1) (valueAsInt v2)


-- Representation of queue of path branches for search
-- Priority queue of (Priority queue of paths of actors sorted by time when their current action ends, openedValves, closedValves)
type PathQueue = PriorityQueue Value (PriorityQueue Int Path, PathSet, PathSet)


-- Representation of valve - neighbor ids and its flow rate
data Valve = Valve {
    valveRate :: Int,
    valveNeighbors :: IntMap Int
} deriving (Show, Eq)


-- Convert ASCII string to int - for valve id
valveKeyFromStr :: String -> Int
valveKeyFromStr [] = 0
valveKeyFromStr (c:cs) = ord c + valveKeyFromStr cs * 256

-- Convert int back to ASCII string - for valve id
valveKeyToStr :: Int -> String
valveKeyToStr 0 = ""
valveKeyToStr v = chr (v `mod` 256) : valveKeyToStr (v `div` 256)

-- Create valve from string id, flow rate and list of neighbors as their string ids
mkValve :: String -> Int -> [String] -> (Int, Valve)
mkValve key rate neighbors = (keyInt, valve)
    where
        keyInt = valveKeyFromStr key
        neighborsInt = map ((\v -> (v, 1)) . valveKeyFromStr) neighbors
        valve = Valve {
            valveRate = rate,
            valveNeighbors = IntMap.fromList neighborsInt
        }

type ValvesMap = IntMap Valve


-- Remove all occurrences of string inside string
removeSubstring :: (Eq a) => [a] -> [a] -> [a]
removeSubstring needle = concat . splitOn needle


-- Modify value of path to include open action (but don't decrease remaining time)
valueForOpen :: Value -> Valve -> Value
valueForOpen value valve = value {
    valueRemainingTime = valueRemainingTime value,
    valueCurrentFlowRate = valueCurrentFlowRate value + valveRate valve,
    valueFlowReleased = valueFlowReleased value
}

-- Modify value of path to include some time spent doing something
valueForTimeSpent :: Value -> Int -> Value
valueForTimeSpent value n = value {
    valueRemainingTime = valueRemainingTime value - n,
    valueFlowReleased = valueFlowReleased value + (valueCurrentFlowRate value * n)
}

-- Calculate the best scenario for given value, number of actors and list of available valves to open
-- Represents the most score this path can possibly generate
valueBestScenario :: Value -> Int -> [Int] -> Int
valueBestScenario v speed availableRates = (
        fst $ -- 6. Take only "alreadyReleased" part of state
        foldl (
            \(c, r) rn -> (c + r + sum rn, r + sum rn) -- 5. Transform state each minute to include new valves opened by actors
        ) (valueFlowReleased v, valueCurrentFlowRate v) ( -- 4. Initialize state: (alreadyReleased, releasedEachMinute)
            take (valueRemainingTime v) $ -- 3. Take for each minute one chunk
            chunksOf speed ( -- 2. Split them to chunks - one for each actor
                availableRates ++ repeat 0 -- 1. Take all available valves yet to be opened (assume they are sorted) padded at the end by zeros
            )
        )
    )

-- Calculate the worst scenario for given value
-- This assumes no more valves will be opened and calculates score for that situation
valueWorstScenario :: Value -> Int
valueWorstScenario v = valueCurrentFlowRate v * valueRemainingTime v + valueFlowReleased v

-- timeLimit :: Int
-- -- timeLimit = 30
-- timeLimit = 26

-- initialPositions :: IntMap.Key -> PriorityQueue Int Path
-- -- initialPositions start = foldr queueInsert Heap.empty [(timeLimit, [start])]
-- initialPositions start = foldr queueInsert Heap.empty [(-timeLimit, [start]), (-timeLimit, [start])] 

-- Perform path search step optimizing for best score
maximizePathSearchStep :: ValvesMap -> PathQueue -> Value -> Value
maximizePathSearchStep valvesMap queue topValue
    -- Queue is empty - no more path branches to search - return the best one
    | Heap.null queue = topValue
    -- Current branch is proven to be worse then the best one known - skip it
    | valueWorstScenario topValue > valueBestScenario actualValue (Heap.size actors) closedRates =
        maximizePathSearchStep valvesMap qs topValue
    -- Expand current branch
    | otherwise = maximizePathSearchStep valvesMap nextQueue nextTopValue 
    where
        -- Expand queue to get top item and it's info
        ((value, (actors, opened, closed)), qs) = queueTop queue
        -- Expand actors queue to get actor for whom his current action ends sooner
        ((atRemainingTmp, path@(current:_)), others) = queueTop actors
        -- Fix remaining time (it is inverted to invert its sorting) - it is hack, it should be fixed
        atRemaining = - atRemainingTmp
        -- lookup valve for which will current action occur
        Just currentValve = IntMap.lookup current valvesMap
        -- Calculate updated value for current action
        actualValue =
            flip valueForOpen currentValve $
            valueForTimeSpent value (valueRemainingTime value - atRemaining)
        -- Extract remaining time for easy access
        actualRemainingTime = valueRemainingTime actualValue

        -- Closed valves as list
        closedList = Set.toList $ closed
        -- List of closed valves including valves which are about to be opened by other actors
        notYetOpenedList = closedList ++ (map (head.snd) $ queueUnpack actors)

        -- All possible move&open actions for current actor
        moveActions =
                -- 4. Build queue item for each neighbor representing actor decision to move towards that valve and open it
                map (\ (neighborId, neighborPrice) -> (
                    actualValue,
                    (
                        queueInsert (-(actualRemainingTime - neighborPrice - 1), neighborId:path) others,
                        Set.insert neighborId opened, Set.delete neighborId closed
                    )
                )) $
                -- 3. Keep only valves for which we have enough time to reach them
                filter (\ (_, neighborPrice) -> neighborPrice < actualRemainingTime) $
                map (\ neighborId -> (
                    neighborId,
                    -- 2. Lookup and add price of moving to that vlave
                    fromJust $ IntMap.lookup neighborId (valveNeighbors currentValve)
                )) closedList -- 1. List closed valves

        -- If possible create also action which would mark current actor as "doing nothing"
        -- Basically this will remove actor completely from this path branch
        -- Last actor cannot be removed (there is no reason to do that)
        -- Why remove actor? Once actor starts moving towards a valve, other actor cannot start moving there later
        -- even if their path would be shorter. This makes it impossible to search some path branches.
        -- Solution? Try removing the actor - this makes it possible for other actor to start moving to last valve
        -- while first actor is doing nothing
        waitActions = if null others
            then []
            else [(actualValue, (others, opened, closed))]

        -- List of rates of closed valves, sorted from highest to lowest
        closedRates =
            map negate $ -- 3. And negate them back to get their previous value
            sort $ -- 2. Sort then
            map ( -- 1. Find valve rates for all closed valves and negate them
                negate .
                valveRate .
                fromJust .
                flip IntMap.lookup valvesMap
            ) notYetOpenedList
        
        -- Should we update top value with current value?
        updateTop = valueWorstScenario actualValue > valueWorstScenario topValue

        -- Some debug info for trace
        -- stateInfo = (
        --         Heap.size queue, -- Length of queue
        --         actualValue, -- Value of currently inspected path
        --         -- Human readable representation of already opened valves (including valves which are about to be opened)
        --         Set.map valveKeyToStr opened,
        --         -- Human readable representation of yet to be opened valves
        --         Set.map valveKeyToStr closed
        --     )

        -- Updated top value, also log updated values using traceShow
        -- nextTopValue = if updateTop
        --     then traceShow stateInfo actualValue
        --     else topValue

        -- Updated top value
        nextTopValue = if updateTop
            then actualValue
            else topValue

        -- Actions to be added to queue
        availableActions = moveActions ++ waitActions
        -- Queue updated to include new actions
        nextQueue = foldr queueInsert qs availableActions

-- Perform path search optimizing for best score
maximizePathSearch :: Int -> Int -> Int -> ValvesMap -> Int
maximizePathSearch start timeLimit actors valvesMap =
        valueWorstScenario $ -- Calculate score of best value by assuming no more valves will be opened
        maximizePathSearchStep valvesMap initialQueue initialValue
    where
        initialValue = Value {
            valueRemainingTime = timeLimit,
            valueCurrentFlowRate = 0,
            valueFlowReleased = 0
        }
        -- Initial queue of paths for each actor
        initialPositions =
            foldr queueInsert Heap.empty $
            replicate actors (-timeLimit, [start])
        -- Set of all valves with flow rate higher then 0
        initialClosed =
            Set.fromList $
            map fst $
            filter ((/= 0) . valveRate . snd) $
            IntMap.toList valvesMap
        -- Initial queue with single path representing start position
        initialQueue = foldr queueInsert Heap.empty [(initialValue, (initialPositions, Set.empty, initialClosed))]


-- Parse input line to a valve
parseLine :: [String] -> (Int, Valve)
parseLine (
        "Valve" : key : "has" : "flow" : ('r':'a':'t':'e':'=':rate) :
        _ : _ : "to" : _ : neighbors
    ) = mkValve fixedKey fixedRate fixedNeighbors
    where
        fixedKey = key
        fixedRate = read $ removeSubstring ";" rate
        fixedNeighbors = map (removeSubstring ",") neighbors

-- Check if IntMap contains a key
intMapContains :: IntMap.Key -> IntMap a -> Bool
intMapContains k m = maybe False (\_ -> True) $ IntMap.lookup k m

-- Insert value into IntMap, if already present, keep smaller value
intMapInsertSmaller :: IntMap Int -> (Int, Int) -> IntMap Int
intMapInsertSmaller valves (key, cost) = IntMap.alter applyChange key valves
    where
        applyChange (Just oldCost)
            | oldCost > cost = Just cost
            | otherwise = Just oldCost
        applyChange Nothing = Just cost

-- Add neighbors of neighbors as neighbors of this valve with updated cost
valveUpdateNeighbors :: ValvesMap -> Int -> Valve -> Valve
valveUpdateNeighbors valves key valve = valve {
        valveNeighbors = updatedNeighbors
    }
    where
        -- List neighbors of valve by its id
        listNeighbors valveId = IntMap.toList $ valveNeighbors $ fromJust $ IntMap.lookup valveId valves
        -- Map of neighbors of current valve
        oldNeighbors = valveNeighbors valve
        -- New updated map of neighbors
        newNeighbors =
            -- 5. Don't add self into list of neighbors
            filter ((/=) key . fst) $
            concat $ -- 4. Make one big list of all new neighbors
            map (
                \(valveId, cost) -> map (
                    \(nValveId, nCost) -> (nValveId, cost + nCost)
                ) $ -- 3. Calculate updated cost for each neighbor
                listNeighbors valveId -- 2. List neighbors of each neighbor
            ) $
            IntMap.toList oldNeighbors -- 1. Take old neighbors as list
        -- Updated neighbors with all new neighbors inserted
        updatedNeighbors = foldr (flip intMapInsertSmaller) oldNeighbors newNeighbors

-- Repeatedly add all neighbors of neighbors of valves to neighbors of valves until
-- there are no more changes to valves
addIndirectNeighbors :: ValvesMap -> ValvesMap
addIndirectNeighbors valves =
    if updatedValves == valves
        then valves
        else addIndirectNeighbors updatedValves
    where
        updatedValves = IntMap.mapWithKey (valveUpdateNeighbors valves) valves
        

-- Parse input into ValvesMap
parseInput :: String -> ValvesMap
parseInput =
    addIndirectNeighbors . -- 5. Make sure we know how to reach any valve from any other valve
    IntMap.fromList . -- 4. Create map of valves
    map parseLine . -- 3. Parse each line to Valve with id
    map words . -- 2. Split lines by words
    lines -- 1. List of lines


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Task specific constants
    let start = valveKeyFromStr "AA"

    -- Main logic
    let -- Parse input to ValvesMap
        valvesMap = parseInput contents

        -- Part #1 - 30 minutes time limit, only 1 actor
        resultP1 = maximizePathSearch start 30 1 valvesMap
        -- Part #2 - 26 minutes time limit, 2 actors
        resultP2 = maximizePathSearch start 26 2 valvesMap

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal16-1.test.txt" -- 1651 | 1707
    printResult "./data/cal16-1.orig.txt"
