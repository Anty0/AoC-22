#!/usr/bin/env runhaskell

import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Char (ord, chr)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- Remove all occurrences of string inside string
removeSubstring :: (Eq a) => [a] -> [a] -> [a]
removeSubstring needle = concat . splitOn needle

-- Known operations
data Operation
    = OpPlus
    | OpMinus
    | OpMul
    | OpDiv
      deriving (Show, Eq)

-- Representation of equation
-- Branches of equation linked using IntMap
-- There are definitely faster solutions to this
data Value
    = Value Int
    | Equation Operation Int Int
    | Variable
      deriving (Show, Eq)

-- Convert short string to number
strToInt :: String -> Int
strToInt = foldl (\c x -> c * 256 + x) 0 . map ord

-- Convert number back to string (useful for debugging)
strFromInt :: Int -> String
strFromInt num =
    reverse $
    map chr $
    takeWhile (/= 0) $
    drop 1 $
    map snd $
    iterate (\(c, _) -> (c `div` 256, c `mod` 256)) (num, 0)

-- Parse operation from string
operationFromStr :: String -> Operation
operationFromStr "+" = OpPlus
operationFromStr "-" = OpMinus
operationFromStr "*" = OpMul
operationFromStr "/" = OpDiv

-- Apply operation
operationApply :: Operation -> Int -> Int -> Int
operationApply OpPlus n1 n2 = n1 + n2
operationApply OpMinus n1 n2 = n1 - n2
operationApply OpMul n1 n2 = n1 * n2
operationApply OpDiv n1 n2 = n1 `div` n2

-- Reverse operation for unknown branch
operationReverse :: Operation -> Int -> Maybe Int -> Maybe Int -> Int
operationReverse OpPlus value (Just n) Nothing = value - n
operationReverse OpPlus value Nothing (Just n) = value - n
operationReverse OpMinus value (Just n) Nothing = n - value
operationReverse OpMinus value Nothing (Just n) = value + n
operationReverse OpMul value (Just n) Nothing = value `div` n
operationReverse OpMul value Nothing (Just n) = value `div` n
operationReverse OpDiv value (Just n) Nothing = n `div` value
operationReverse OpDiv value Nothing (Just n) = value * n

-- Parse part of equation
parseLine :: [String] -> (Int, Value)
parseLine [name, num] = (strToInt fixedName, Value (read num))
    where fixedName = removeSubstring ":" name
parseLine [name, n1, op, n2] =
    (strToInt fixedName, Equation (operationFromStr op) (strToInt n1) (strToInt n2))
    where
        fixedName = removeSubstring ":" name

-- Parse input into map of equations
parseInput :: String -> IntMap Value
parseInput =
    IntMap.fromList . -- 3. Create an IntMap
    map (parseLine . words) . -- 2. Parse each line
    lines -- 1. List of lines


-- Helper for solve - uses pattern matching for Value
solveWith :: IntMap Value -> Value -> Maybe Int
solveWith _ (Value n) = Just n
-- We don't know what should be in variable
solveWith _ Variable = Nothing
-- If solve n1 or solve n2 is nothing, returns nothing, otherwise applies operation and returns Just
solveWith equations (Equation op n1 n2) = do
    v1 <- solve equations n1
    v2 <- solve equations n2
    return $ operationApply op v1 v2

-- Solve equation - returns Just if result is known, otherwise Nothing
solve :: IntMap Value -> Int -> Maybe Int
solve equations name = solveWith equations $ fromJust $ IntMap.lookup name equations

-- Helper for expect - uses pattern matching for Value
expectWith :: IntMap Value -> Int -> Value -> Int
-- We can't expect value of constant
-- this is a constant branch, but expect can be executed only on variable branches
expectWith _ _ (Value _) = undefined
-- Variable found - use expected value as value of variable
expectWith _ value Variable = value
-- Reverse operation and recurse into variable branch
expectWith equations value (Equation op n1 n2) =
    expect equations newValue unknownBranch
    where
        v1 = solve equations n1
        v2 = solve equations n2
        -- Value the unknown branch should result in
        newValue = operationReverse op value v1 v2
        -- Name of unknown (variable) branch
        unknownBranch = case v1 of
            -- First branch is known - solve for second
            Just _ -> n2
            -- Second branch is known - solve for first
            Nothing -> n1

-- Solve variable in equation with expected value, expects exactly one occurrence of variable in whole equation
expect :: IntMap Value -> Int -> Int -> Int
expect equations value name = expectWith equations value $ fromJust $ IntMap.lookup name equations

-- Solve for variable, use constant branch a expected value
-- (basically forces operation '=' for selected equation)
solveFor :: IntMap Value -> Int -> Int
solveFor equations name = expect equations known unknown
    where
        -- We expect this to be an equation
        Equation _ n1 n2 = fromJust $ IntMap.lookup name equations
        v1 = solve equations n1
        v2 = solve equations n2
        -- Value of known (constant) branch
        known = maybe (fromJust v2) id v1
        -- Name of unknown (variable) branch
        unknown = maybe n1 (const n2) v1

printResult file = do
    -- Reading file
    contents <- readFile file

    -- Task specific constants
    let start = strToInt "root"
        human = strToInt "humn"

    -- Main logic
    let -- Parse input to map of equation
        equations = parseInput contents
        equationsWithHuman = IntMap.adjust (const Variable) human equations

        -- Part #1 - Solve root
        resultP1 = fromJust $ solve equations start
        -- Part #2 - Solve for variable
        resultP2 = solveFor equationsWithHuman start

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal21-1.test.txt" -- 152 | 301
    -- printResult "./data/cal21-1.orig.txt"
