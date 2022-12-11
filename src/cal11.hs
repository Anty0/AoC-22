#!/usr/bin/env runhaskell

import System.IO
import Data.List (groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Ix (Ix, range)
import Data.Function (on)


-- Representation of Range
type Range a = (a, a)

-- Action which can be applied to two numbers to get single number
data Action = Sum | Multiply | Divide | Subtract | Modulo deriving (Show, Eq)

-- Representation of math operation to modify a number (single variable supported)
data Operation a
    -- Action between two numbers
    = Operation Action (Operation a) (Operation a)
    -- Constant number
    | Const a
    -- Value from variable
    | OldValue
      deriving (Show, Eq)

-- Representation of possible test conditions
data Test a
    -- Only "is divisible by" test is supported
    = TestDivisible a deriving (Show, Eq)

-- Representation of monkey holding items
data Monkey a b = Monkey {
    -- Items held by monkey, represented by number
    monkeyItems :: [b],
    -- Operation to apply to all items before testing them
    monkeyOperation :: Operation b,
    -- Test to be applied to all items after operation
    monkeyTest :: Test b,
    -- If test results in true - move item to monkey with this id
    monkeyIfTrue :: a,
    -- If test results in false - move item to monkey with this id
    monkeyIfFalse :: a,
    -- Statistic of how many items did monkey tested
    monkeyHandled :: Int
} deriving (Show, Eq)

-- Tokenized representation of input data - for parsing
data Token a b = MId a
               | MItems [b]
               | MOperation (Operation b)
               | MTest (Test b)
               | MIfTrue a
               | MIfFalse a
                 deriving (Show, Eq)

-- Representation of lookup table of monkeys
data MonkeyLookupTable a b = MonkeyLookupTable {
    monkeysRange :: Range a,
    monkeysTable :: [(a, Monkey a b)]
}


-- Convert string representing action to action
actionRead :: String -> Action
actionRead "+" = Sum
actionRead "*" = Multiply
actionRead "/" = Divide
actionRead "-" = Subtract
actionRead "%" = Modulo

-- Convert string representing operation to operation
-- Only constant number and variable called "old" are supported
operationRead :: (Read a) => String -> Operation a
operationRead "old" = OldValue
operationRead v = Const $ read v

-- Lookup monkey by id in lookup table; Assumes monkey is present
-- First occurrence of id in lookup table is considered
-- the most current representation of monkey
-- Lookup table may also contain older versions of monkeys.
-- To update monkey representation it is enough to place it at
-- the beginning of list
monkeyLookup :: (Eq a) => a -> MonkeyLookupTable a b -> Monkey a b
monkeyLookup id = fromJust . lookup id . monkeysTable

-- List all monkeys in lookup table
monkeysList :: (Ix a, Eq a) => MonkeyLookupTable a b -> [Monkey a b]
monkeysList monkeys = map lookupById $ range $ monkeysRange monkeys
    where lookupById = flip monkeyLookup monkeys

-- Apply selected action to two numbers and return result
actionApply :: (Integral a) => Action -> a -> a -> a
actionApply Sum      v1 v2 = v1 + v2
actionApply Multiply v1 v2 = v1 * v2
actionApply Divide   v1 v2 = v1 `div` v2
actionApply Subtract v1 v2 = v1 - v2
actionApply Modulo   v1 v2 = v1 `mod` v2

-- Apply operation to a value
operationApply :: (Integral a) => Operation a -> a -> a
operationApply (Operation action op1 op2) a = actionApply action (operationApply op1 a) (operationApply op2 a)
operationApply (OldValue) a = a
operationApply (Const v) _ = v

-- Remove all occurrences of string inside string
removeSubstring :: (Eq a) => [a] -> [a] -> [a]
removeSubstring needle = concat . splitOn needle


-- Convert input line to a token
tokenize :: (Read a, Read b) => [String] -> Token a b
-- Parse monkey id
tokenize ["Monkey", v] = MId $ read $ removeSubstring ":" v
-- Parse monkey items
tokenize ("Starting" : "items:" : values) = MItems $ map (read . removeSubstring ",") values
-- Parse monkey operation
tokenize ["Operation:", "new", "=", v1, a, v2] =
    MOperation $ Operation (actionRead a) (operationRead v1) (operationRead v2)
-- Parse monkey test
tokenize ["Test:", "divisible", "by", v] = MTest $ TestDivisible $ read v
-- Parse id of monkey for successful test
tokenize ["If", "true:", "throw", "to", "monkey", v] = MIfTrue $ read v
-- Parse id of monkey for unsuccessful test
tokenize ["If", "false:", "throw", "to", "monkey", v] = MIfFalse $ read v

-- Convert block of tokens to monkey
monkeynize :: [Token a b] -> (a, Monkey a b)
monkeynize [
        MId id,
        MItems items,
        MOperation op,
        MTest test,
        MIfTrue ifTrue,
        MIfFalse ifFalse
    ] = (id, Monkey items op test ifTrue ifFalse 0)

-- Convert input string to list of monkeys
parseInput :: (Num a, Read a, Read b) => String -> MonkeyLookupTable a b
parseInput input = MonkeyLookupTable {
        monkeysRange = (fromInteger 0, fromInteger $ toInteger $ length monkeys - 1),
        monkeysTable = monkeys
    }
    where
        monkeys =
            map (
                monkeynize . -- 5. Convert each block of tokens to monkey
                map (
                    tokenize . -- 4. Convert these lines of words to tokens
                    words -- 3. Split each line in each block to words
                )
            ) $
            splitOn [""] $ -- 2. Split them to blocks
            lines input -- 1. Take individual lines


-- Run given test and return vt of vf based on result
testChooseBy ::(Integral b) => Test b -> b -> a -> a -> a
testChooseBy (TestDivisible v) b vt vf
    | b `mod` v == 0 = vt
    | otherwise      = vf

-- Test monkey condition for value
monkeyCheckTest :: (Integral b) => Monkey a b -> b -> a
monkeyCheckTest current value = testChooseBy (monkeyTest current) value (monkeyIfTrue current) (monkeyIfFalse current)

-- Update lookup table by adding new version of monkey with some items added
monkeyAddItems :: (Eq a) => (a, [b]) -> MonkeyLookupTable a b -> MonkeyLookupTable a b
monkeyAddItems (id, newItems) monkeys = monkeys {
        monkeysTable = (
            id,
            current {
                monkeyItems = monkeyItems current ++ newItems
            }
        ) : monkeysTable monkeys
    }
    where current = monkeyLookup id monkeys

-- Update lookup table by adding new version of monkey without any
-- items and increased handling by amount of items it had
monkeyMarkHandledItems :: (Eq a) => a -> MonkeyLookupTable a b -> MonkeyLookupTable a b
monkeyMarkHandledItems id monkeys = monkeys {
        monkeysTable = (
            id,
            current {
                monkeyItems = [],
                monkeyHandled = currentHandled
            }
        ) : monkeysTable monkeys
    }
    where
        current = monkeyLookup id monkeys
        currentHandled = monkeyHandled current + length (monkeyItems current)

-- Simulate monkey testing its items and throwing them to other monkeys
simulateMonkey :: (Ix a, Eq a, Ord a, Integral b) => a -> MonkeyLookupTable a b -> MonkeyLookupTable a b
simulateMonkey id monkeys =
    -- Update current monkey
    monkeyMarkHandledItems id $
    -- Add handled items to target monkeys
    foldr monkeyAddItems monkeys handledItems
    where
        current = monkeyLookup id monkeys
        handledItems =
            map (\l -> (fst $ head l, map snd l)) $ -- 6. Extract target monkey it from list to pair of (target monkey, values)
            groupBy ((==) `on` fst) $ -- 5. Create separate list for each monkey
            sortBy (compare `on` fst) $ -- 4. Sort by target monkey (required by groupBy)
            map (
                (\v -> (monkeyCheckTest current v, v)) . -- 3. Make pair - (target monkey, value)
                operationApply (monkeyOperation current) -- 2. Apply operation to item
            ) $
            monkeyItems current -- 1. Take items of current monkey

-- Simulate a round of monkeys testing their items and throwing them to other monkeys
simulateRound :: (Ix a, Eq a, Ord a, Integral b) => MonkeyLookupTable a b -> [MonkeyLookupTable a b]
simulateRound monkeys = scanl (flip simulateMonkey) monkeys $ range $ monkeysRange monkeys

-- Run endless simulation of rounds of monkeys testing their items and throwing them to other monkeys
-- Produces infinite list of states during each round
simulate :: (Ix a, Eq a, Ord a, Integral b) => MonkeyLookupTable a b -> [[MonkeyLookupTable a b]]
simulate monkeys = iterate (\l -> simulateRound $ last l) [monkeys]

-- Print monkeys in lookup table
printMonkeys :: (Ix a, Eq a, Show a, Show b) => MonkeyLookupTable a b -> IO ()
printMonkeys monkeys = do
    mapM print $ monkeysList monkeys
    putStrLn ""

-- Optimization - Let's assume all tests are TestDivisible and there are no
-- incompatible operations in monkeys operations.
-- If thats true, we may use modulo to limit size of items numbers.
monkeysAddLcmLimit :: (Ix a, Integral b) => MonkeyLookupTable a b -> MonkeyLookupTable a b
monkeysAddLcmLimit monkeys = monkeys {
        monkeysTable =
            map (
                \(id, monkey) -> (
                    id,
                    monkey {
                        monkeyOperation = Operation Modulo (monkeyOperation monkey) (Const monkeysLCM)
                    }
                )
            ) $
            monkeysTable monkeys
    }
    where monkeysLCM =
            foldr lcm 1 $ -- 3. Calculate LCM of all those numbers
            map (
                (\(TestDivisible v) -> v) . -- 2. Extract value of "TestDivisible" test
                monkeyTest -- 1. Take monkeys test
            ) $ monkeysList monkeys

-- Score a round by selecting 2 monkeys with highest handling and multiplying their handling
monkeysRoundScore :: (Ix a) => MonkeyLookupTable a b -> Int
monkeysRoundScore = product . map monkeyHandled . take 2 . sortBy (flip compare `on` (monkeyHandled)) . monkeysList

-- Automatically decrease item number by dividing it with 3
monkeysAddRelief :: (Num b) => MonkeyLookupTable a b -> MonkeyLookupTable a b
monkeysAddRelief monkeys = monkeys {
        monkeysTable =
            map (
                \(id, monkey) -> (
                    id,
                    monkey {
                        monkeyOperation = Operation Divide (monkeyOperation monkey) (Const $ fromInteger 3)
                    }
                )
            ) $
            monkeysTable monkeys
    }


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let monkeys = parseInput contents
        -- Monkeys with modified operation to add relief of item values
        monkeysWithRelief = monkeysAddRelief monkeys
        -- Monkeys with modified operation to limit value size based on LCM
        monkeysWithoutRelief = monkeysAddLcmLimit monkeys

        -- Simulate monkeys
        simulationWithRelief = simulate monkeysWithRelief
        simulationWithoutRelief = simulate monkeysWithoutRelief

        -- Round 20 of simulation with relief
        simulationWithRelief20 = head $ drop 21 $ map head simulationWithRelief
        -- Round 10000 of simulation without relief
        simulationWithoutRelief10000 = head $ drop 10001 $ map head simulationWithoutRelief
        
        -- Part #1 - Score from simulation round 20 with relief
        simulationWithRelief20Top = monkeysRoundScore simulationWithRelief20
        -- Part #2 - Score from simulation round 10000 without relief
        simulationWithoutRelief10000Top = monkeysRoundScore simulationWithoutRelief10000

    -- Output handling
    printMonkeys simulationWithRelief20
    printMonkeys simulationWithoutRelief10000
    putStrLn $ "'" ++ file ++ "': '" ++ show (simulationWithRelief20Top) ++ "' | '" ++ show (simulationWithoutRelief10000Top) ++ "'\n"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal11-1.test.txt" -- 10605 | 2713310158
    printResult "./data/cal11-1.orig.txt"
