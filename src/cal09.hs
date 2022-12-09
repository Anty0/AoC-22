#!/usr/bin/env runhaskell

import System.IO
import Data.List (intercalate, nub)
import qualified Data.Set as Set


-- Coordinates in 2D space
type Position = (Int, Int)

-- State of simulation - head plus list of connected tails
type SimulationState = (Position, [Position])
-- Tvo sets of coordinates representing boundaries of square
type Boundaries = (Position, Position)

-- Available actions for head movement
data Action = AUp
            | ARight
            | ADown
            | ALeft
            deriving (Show, Eq)


-- Convert line to set of actions
tokenize :: [String] -> [Action]
tokenize ["U", n] = replicate (read n) AUp
tokenize ["R", n] = replicate (read n) ARight
tokenize ["D", n] = replicate (read n) ADown
tokenize ["L", n] = replicate (read n) ALeft

-- Move position by action
apply :: Action -> Position -> Position
apply AUp    (x, y) = (x,   y-1)
apply ARight (x, y) = (x+1, y  )
apply ADown  (x, y) = (x,   y+1)
apply ALeft  (x, y) = (x-1, y  )

-- Actions to apply to move tail correctly towards head
tailActions :: Position -> Position -> [Action]
tailActions (hx, hy) (tx, ty) = moveX ++ moveY
    where
        -- Are we far enough to start moving?
        shouldMove = hx > tx + 1
                  || hx < tx - 1
                  || hy > ty + 1
                  || hy < ty - 1
        -- Actions to preform to make x coordinate correct
        moveX
            | shouldMove && hx > tx = [ARight]
            | shouldMove && hx < tx = [ALeft]
            | otherwise   = []
        -- Actions to preform to make y coordinate correct
        moveY
            | shouldMove && hy > ty = [ADown]
            | shouldMove && hy < ty = [AUp]
            | otherwise   = []

-- Convert input string to list of actions for simulation
parseInput :: String -> [Action]
parseInput input = concat $ map (tokenize.words) $ lines input

-- Move tail to correct for any head movement
updateTail :: Position -> [Position] -> [Position]
updateTail _ [] = []
updateTail head (tail:xt) = nt : updateTail nt xt
    where nt = foldr apply tail $ tailActions head tail

-- Apply all actions one by one to head and move tail accordingly
-- Return list of positions of head and tails as the simulation goes through them
simulate :: [Action] -> Position -> [Position] -> [SimulationState]
simulate [] head tail = [(head, tail)]
simulate (action:as) head tail = (head, tail) : simulate as newHead newTail
    where newHead = apply action head
          newTail = updateTail newHead tail

-- Extends boundaries to include position if required
updateBoundaries :: Boundaries -> Position -> Boundaries
updateBoundaries ((sx, sy), (ex, ey)) (x, y) = ((nsx, nsy), (nex, ney))
    where nsx = min sx x
          nsy = min sy y
          nex = max ex x
          ney = max ey y

-- Make boundaries to match simulation area
simulationBoundaries :: Boundaries -> [SimulationState] -> Boundaries
simulationBoundaries bounds simulation = foldl updateBoundaries bounds $ concat $ map (\(h, t) -> h:t) simulation

-- Helper function to search through list of tail and return its index or '.' or 's' (for start position)
tailVisualization :: Int -> [Position] -> Position -> Char
tailVisualization _ [] (0, 0) = 's'
tailVisualization _ [] _ = '.'
tailVisualization n ((tx, ty):tail) pos@(x, y)
    | x == tx && y == ty = head $ show n
    | otherwise          = tailVisualization (n+1) tail pos

-- Visualization of point in area of simulation - 'H' for head location, number
-- index for tail, 's' for start location and '.' for any other point
positionVisualization :: SimulationState -> Position -> Char
positionVisualization ((hx, hy), tail) pos@(x, y)
    | x == hx && y == hy = 'H'
    | otherwise          = tailVisualization 1 tail pos

-- 2D map visualization of simulation state
stateVisualization :: (Position, Position) -> SimulationState -> String
stateVisualization ((sx, sy), (ex, ey)) state = (intercalate "\n" $ [[positionVisualization state (x, y) | x <- [sx..ex]] | y <- [sy..ey]]) ++ ['\n']
    -- where ((sx, sy), (ex, ey)) = simulationBoundaries ((0, 0), (0, 0)) [state]

-- print 2D map visualization of all simulation states
drawSimulation :: (Position, Position) -> [SimulationState] -> IO [()]
drawSimulation bounds = mapM (putStrLn . stateVisualization bounds)

-- print 2D map of all visited positions
drawVisited :: (Position, Position) -> Set.Set Position -> IO ()
drawVisited ((sx, sy), (ex, ey)) positions = putStrLn $ (intercalate "\n" $ [[if (x, y) `Set.member` positions then '#' else '.' | x <- [sx..ex]] | y <- [sy..ey]]) ++ ['\n']

printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let actions = parseInput contents
        -- Simulate actions starting from point (0, 0)
        simulation = simulate actions (0, 0) (replicate 9 (0, 0))
        -- Calculate boundaries of all visited points in simulation
        bounds = simulationBoundaries ((0, 0), (0, 0)) simulation
        -- Part #1 - list of all visited points by first tail
        visitedFirst = nub $ map (head.snd) simulation
        -- Part #2 - list of all visited points by last tail
        visitedLast = nub $ map (last.snd) simulation
        -- Bonus - list of all visited points by all tails
        visitedAll = nub $ concat $ map (snd) simulation

    -- Output handling
    -- print actions
    -- print simulation
    -- print bounds

    putStrLn "Last state of simulation:"
    drawSimulation bounds [last simulation]
    putStrLn "Visited by first tail:"
    drawVisited bounds $ Set.fromList visitedFirst
    putStrLn "Visited by last tail:"
    drawVisited bounds $ Set.fromList visitedLast
    putStrLn "Visited by any tail:"
    drawVisited bounds $ Set.fromList visitedAll
    putStrLn $ "'" ++ file ++ "': '" ++ show (length visitedFirst) ++ "' | '" ++ show (length visitedLast) ++ "' | '" ++ show (length visitedAll) ++ "'\n\n"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal09-1.test.txt" -- 13 | 1
    printResult "./data/cal09-2.test.txt" -- 88 | 36
    -- printResult "./data/cal09-1.orig.txt"
