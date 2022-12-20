#!/usr/bin/env runhaskell

-- import Debug.Trace (traceShow)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Array (Array, (!))
import qualified Data.Array as Array

-- Representation of stack
type Stack a = [a]

-- Representation of resource - list of costs of building bot for this resource
type Resource = [Int]

-- Representation of Blueprint
type Blueprint = Array Int (Resource, Int)

-- State of simulation
data State = State {
    stateRemainingTime :: Int,
    stateResources :: [Int],
    stateBots :: IntMap Int,
    stateSkipBots :: Set Int
} deriving (Show, Eq)

-- Stack of branches to search
type StateStack = Stack State


-- Advance time of state
stateAdvance :: State -> State
stateAdvance state = state {
    stateRemainingTime = stateRemainingTime state - 1,
    stateResources = map (uncurry (+)) $ zip (map snd $ IntMap.toAscList $ stateBots state) (stateResources state)
}

-- Check if we have enough resources for building this bot
stateCanBuildBot :: Blueprint -> Int -> State -> Bool
stateCanBuildBot blueprint botId state =
    botMax > (fromJust $ IntMap.lookup botId $ stateBots state) &&
    (all (uncurry (>=)) $ zip (stateResources state) botResources)
    where
        (botResources, botMax) = blueprint ! botId

-- Build action
stateBuildBot :: Blueprint -> Int -> State -> State
stateBuildBot blueprint botId state = state {
    stateResources = map (uncurry (-)) $ zip (stateResources state) (fst $ blueprint ! botId),
    stateBots = IntMap.adjust ((+) 1) botId $ stateBots state,
    stateSkipBots = Set.empty
}

-- Available build actions
stateAvailableBuilds :: Blueprint -> State -> [(State -> State)]
stateAvailableBuilds blueprint state = buildStates ++ [waitState]
    where
        skipBots = stateSkipBots state
        keepBot botId = (not $ botId `Set.member` skipBots) && stateCanBuildBot blueprint botId state
        availableBots = filter keepBot [0..3]
        buildStates = map (\i -> stateBuildBot blueprint i) availableBots
        waitState s = s {
            stateSkipBots = Set.fromList availableBots `Set.union` stateSkipBots s
        }

-- Worst possible scenario for state
stateWorstScenario :: Blueprint -> Int -> State -> Int
stateWorstScenario blueprint resourceId state = alreadyProduced + toBeProduced
    where
        alreadyProduced = stateResources state !! resourceId
        toBeProduced = (fromJust $ IntMap.lookup resourceId $ stateBots state) * stateRemainingTime state

-- Optimistic best scenario for state
stateBestScenario :: Blueprint -> Int -> State -> Int
stateBestScenario blueprint resourceId state = alreadyProduced + toBeProduced + toBeProducedByBuiltRobots
    where
        remainingTime = stateRemainingTime state
        alreadyProduced = stateResources state !! resourceId
        toBeProduced = (fromJust $ IntMap.lookup resourceId $ stateBots state) * remainingTime
        toBeProducedByBuiltRobots = (remainingTime * (remainingTime - 1)) `div` 2

-- Perform search step optimizing for best score
maximizeScoreSearchStep :: Blueprint -> StateStack -> State -> Int -> State
-- Stack is empty - no more branches to search - return the best one
maximizeScoreSearchStep _ [] topState _ = topState
maximizeScoreSearchStep blueprint stack topState topStateWorst
    -- Current branch is proven to be worse then the best one known - skip it
    | topStateWorst >= stateBest = maximizeScoreSearchStep blueprint stackRest topState topStateWorst
    -- Expand current branch
    | otherwise = maximizeScoreSearchStep blueprint nextStack nextTopState nextTopStateWorst
    where
        -- Expand stack to get top item and it's info
        (state : stackRest) = stack

        -- All possible move&open actions for current actor
        buildActions = stateAvailableBuilds blueprint state

        stateWorst = stateWorstScenario blueprint 0 state
        stateBest = stateBestScenario blueprint 0 state
        
        -- Should we update top value with current value?
        updateTop = stateWorst > topStateWorst

        -- Some debug info for trace
        -- stateInfo = (
        --         length stack, -- Length of stack
        --         stateWorst,
        --         state -- Currently inspected state
        --     )

        -- Updated top state
        nextTopState = if updateTop
            then state
            else topState

        -- Updated top state score, also log updated states using traceShow
        -- nextTopStateWorst = if updateTop
        --     then traceShow stateInfo stateWorst
        --     else topStateWorst

        -- Updated top state score
        nextTopStateWorst = if updateTop
            then stateWorst
            else topStateWorst

        updatedState = stateAdvance state

        -- Actions to be added to stack
        availableActions = map ($ updatedState) buildActions

        -- Stack updated to include new actions
        nextStack = if stateRemainingTime state > 0
            then availableActions ++ stackRest
            else stackRest

-- Initial state
stateInit :: Int -> State
stateInit timeLimit = State {
        stateRemainingTime = timeLimit,
        stateResources = [0 | _ <- [0..3]],
        stateBots = IntMap.fromList $ (3, 1) : [(i, 0) | i <- [0..2]],
        stateSkipBots = Set.empty
    }

-- Perform search optimizing for best score
maximizeScoreSearch :: Int -> Blueprint -> Int
maximizeScoreSearch timeLimit blueprint =
        stateWorstScenario blueprint 0 $ maximizeScoreSearchStep blueprint initialStack initialTopState 0
    where
        initialState = stateInit timeLimit
        initialTopState = initialState {
            stateRemainingTime = 0
        }
        -- Initial stack with single state representing start position
        initialStack = [initialState]


-- Parse input line to a valve
parseLine :: [String] -> Blueprint
parseLine [
        "Blueprint", _,
        "Each", "ore", "robot", "costs", oreBotOre, "ore.",
        "Each", "clay", "robot", "costs", clayBotOre, "ore.",
        "Each", "obsidian", "robot", "costs", obsidianBotOre, "ore", "and", obsidianBotClay, "clay.",
        "Each", "geode", "robot", "costs", geodeBodOre, "ore", "and", geodeBotObsidian, "obsidian."
    ] = Array.array (0, 3) [
        (0, ([0, read geodeBotObsidian, 0,                    read geodeBodOre   ], maxBound)), -- Geode
        (1, ([0, 0,                     read obsidianBotClay, read obsidianBotOre], read geodeBotObsidian)), -- Obsidian
        (2, ([0, 0,                     0,                    read clayBotOre    ], read obsidianBotClay)), -- Clay
        (3, ([0, 0,                     0,                    read oreBotOre     ], maximum [
            read geodeBodOre, read obsidianBotOre, read clayBotOre, read oreBotOre]))  -- Ore
    ]
        

-- Parse input into ValvesMap
parseInput :: String -> [Blueprint]
parseInput =
    map parseLine . -- 3. Parse each line to blueprint
    map words . -- 2. Split lines by words
    lines -- 1. List of lines


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Parse input to list of blueprints
        blueprints = parseInput contents

        -- -- Part #1 - 24 minutes time limit, all blueprints
        resultP1 = sum $ map (uncurry (*)) $ zip [1..] $ map (maximizeScoreSearch 24) blueprints
        -- -- Part #2 - 32 minutes time limit, first three blueprints
        resultP2 = product $ map (maximizeScoreSearch 32) $ take 3 blueprints

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal19-1.test.txt" -- 33 | 3472
    -- printResult "./data/cal19-1.orig.txt"
