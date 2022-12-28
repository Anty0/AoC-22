#!/usr/bin/env runhaskell

-- import Debug.Trace (traceShowId, traceShow, trace)
import Data.Ix (range)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


-- Index in a 2D grid
type Index = (Int, Int)

-- Bitmap of 2D grid specialized for current task
data Bitmap = Bitmap {
    bitmapRound :: Int, -- Round counter
    bitmapRange :: (Index, Index), -- Range of bitmap (for drawing)
    bitmapOccupied :: Set Index, -- Actual bitmap of occupied indexes
    bitmapReservations :: HashMap Index (Maybe Index) -- Reservation map for upcoming moves
} deriving (Show, Eq)


-- Sum of two indexes
indexAdd :: Index -> Index -> Index
indexAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- Create empty bitmap
bitmapEmpty :: Bitmap
bitmapEmpty = Bitmap {
    bitmapRound = 0,
    bitmapRange = ((0,0), (0,0)),
    bitmapOccupied = Set.empty,
    bitmapReservations = HashMap.empty
}

-- Modify range of bitmap to include index
bitmapExtendRange :: Index -> Bitmap -> Bitmap
bitmapExtendRange (x, y) bitmap = bitmap {
    bitmapRange = newRange
}
    where
        ((sx, sy), (ex, ey)) = bitmapRange bitmap
        nsx = min sx x
        nex = max ex x
        nsy = min sy y
        ney = max ey y
        newRange = ((nsx, nsy), (nex, ney))


-- Parse input into bitmap of occupied indexes
parseInput :: String -> Bitmap
parseInput input = bitmapEmpty {
        bitmapRange = ((minX, minY), (maxX, maxY)),
        bitmapOccupied = Set.fromList occupiedIndexes
    }
    where
        occupiedIndexes =
            concat $
            map (\(y, l) -> map (\x -> (x, y)) $
            map fst $
            filter ((==) '#'.snd) $ zip [0..] l) $
            zip [0..] $
            lines input
        allX = map fst occupiedIndexes
        allY = map snd occupiedIndexes
        minX = minimum allX
        minY = minimum allY
        maxX = maximum allX
        maxY = maximum allY


-- Convert a bitmap point to a char
drawPoint :: Bitmap -> Index -> Char
drawPoint bitmap index =
    case (Set.member index $ bitmapOccupied bitmap, HashMap.lookup index $ bitmapReservations bitmap) of
        (False, Just _) -> 'X' -- Not yet occupied, but will be occupied once move is applied
        (True, Nothing) -> '#' -- Occupied
        (True, Just _) -> 'O' -- Occupied and will not move during next move
        (False, Nothing) -> case index of -- Draw a grid lines for free indexes
            (0, 0) -> '+'
            (0, _) -> '|'
            (_, 0) -> '-'
            _ -> '.'

-- Convert a bitmap to its visual representation
drawBitmap :: Bitmap -> String
drawBitmap bitmap = concat [
        [
            drawPoint bitmap (x, y)
            | x <- [sx..ex]
        ] ++ "\n"
        | y <- [sy..ey]
    ]
    where
        ((sx, sy), (ex, ey)) = bitmapRange bitmap


-- Check if all indexed related to index are free in bitmap
checkPositions :: [Index] -> Bitmap -> Index -> Bool
checkPositions positions bitmap index =
    all (not . flip Set.member (bitmapOccupied bitmap)) $
    map (indexAdd index) positions


-- List of positions with same y and x in range of -1..1
positionsForY :: Int -> [Index]
positionsForY y = [(-1, y), (0, y), (1, y)]

-- List of positions with same x and y in range of -1..1
positionsForX :: Int -> [Index]
positionsForX x = [(x, -1), (x, 0), (x, 1)]

-- Move which will not change current position
moveStay :: Index
moveStay = (0, 0)

-- If all positions around index are free - we should stay
checkStay :: Bitmap -> Index -> Bool
checkStay = checkPositions [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- Move which will move current position one point to the north
moveNorth :: Index
moveNorth = (0, -1)

-- If all neighbor position to the north from us are free - we should move north
checkNorth :: Bitmap -> Index -> Bool
checkNorth = checkPositions (positionsForY (-1))

-- Move which will move current position one point to the south
moveSouth :: Index
moveSouth = (0, 1)

-- If all neighbor position to the south from us are free - we should move south
checkSouth :: Bitmap -> Index -> Bool
checkSouth = checkPositions (positionsForY 1)

-- Move which will move current position one point to the west
moveWest :: Index
moveWest = (-1, 0)

-- If all neighbor position to the west from us are free - we should move west
checkWest :: Bitmap -> Index -> Bool
checkWest = checkPositions (positionsForX (-1))

-- Move which will move current position one point to the east
moveEast :: Index
moveEast = (1, 0)

-- If all neighbor position to the east from us are free - we should move east
checkEast :: Bitmap -> Index -> Bool
checkEast = checkPositions (positionsForX 1)

-- List of all check which we are supposed to cycle
checksBaseline :: [(Bitmap -> Index -> Bool, Index)]
checksBaseline = [
        (checkNorth, moveNorth),
        (checkSouth, moveSouth),
        (checkWest, moveWest),
        (checkEast, moveEast)
    ]

-- List of checks for given bitmap
-- First is always a check for staying with other checks following cycled by a round counter
checksFor :: Bitmap -> [(Index -> Bool, Index)]
checksFor bitmap =
    map (\(f, m) -> (f bitmap, m)) $
    ((checkStay, moveStay) : (    
        take checksCnt $
        drop round $
        cycle checksBaseline
    ))
    where
        checksCnt = length checksBaseline
        round = bitmapRound bitmap `mod` checksCnt


-- Create reservation for move of single index
-- Takes care of solving move conflicts
addReservationMove :: Index -> Index -> HashMap Index (Maybe Index) -> HashMap Index (Maybe Index)
addReservationMove index newIndex reservations = HashMap.alter solveConflicts newIndex reservations
    where
        solveConflicts Nothing = Just (Just index)
        solveConflicts (Just Nothing) = Just Nothing
        solveConflicts (Just (Just _)) = Just Nothing

-- Adds a reservation for a move of given index
addReservationFor :: Index -> Bitmap -> Bitmap
addReservationFor index bitmap = newBitmap {
    bitmapReservations = foldr (addReservationMove index) reservations moves
}
    where
        newBitmap = foldr bitmapExtendRange bitmap moves
        moves =
            take 1 $
            map (indexAdd index) $
            map snd $
            filter ((\f -> f index) . fst) $
            checksFor bitmap
        reservations = bitmapReservations bitmap

-- Add reservations for all occupied indexes in bitmap
updateReservations :: Bitmap -> Bitmap
updateReservations bitmap =
    foldr addReservationFor bitmap $
    Set.toList $
    bitmapOccupied bitmap

-- Apply a reservation to occupied indexes of bitmap
applyReservation :: (Index, Maybe Index) -> Bitmap -> Bitmap
applyReservation (_, Nothing) bitmap = bitmap -- Reservation representing move conflict - nothing to do here
applyReservation (newIndex, Just oldIndex) bitmap = (bitmapExtendRange newIndex bitmap) {
    bitmapOccupied =
        Set.insert newIndex $
        Set.delete oldIndex $
        bitmapOccupied bitmap
}

-- Apply all reservations to occupied indexes of bitmap, reset reservations and increase round counter
applyReservations :: Bitmap -> Bitmap
applyReservations bitmap = updatedBitmap {
        bitmapReservations = HashMap.empty,
        bitmapRound = bitmapRound bitmap + 1
    }
    where
        updatedBitmap =
            foldr applyReservation bitmap $
            HashMap.toList $
            bitmapReservations bitmap

-- Simulate one whole round
simulateRound :: Bitmap -> Bitmap
simulateRound = applyReservations . updateReservations

simulationRun :: Bitmap -> [Bitmap]
simulationRun = iterate simulateRound

-- Calculate score of bitmap - all free indexed within currently occupied range
bitmapScore :: Bitmap -> Int
bitmapScore bitmap =
        length $
        filter (not . flip Set.member occupiedIndexes) $
        range ((minX, minY), (maxX, maxY))
    where
        occupiedIndexes = bitmapOccupied bitmap
        occupiedIndexesList = Set.toList occupiedIndexes
        allX = map fst occupiedIndexesList
        allY = map snd occupiedIndexesList
        minX = minimum allX
        minY = minimum allY
        maxX = maximum allX
        maxY = maximum allY

-- List of all consecutive pairs within a list
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (v1:v2:lt) = (v1, v2) : pairs (v2:lt)


-- Score after 10 rounds
resultPart1 :: [Bitmap] -> Int
resultPart1 = bitmapScore . head . drop 10

-- Round which stopped modifying occupied indexes
resultPart2 :: [Bitmap] -> Int
resultPart2 = bitmapRound . snd . head .
    filter (\(b1, b2) -> bitmapOccupied b1 == bitmapOccupied b2) .
    pairs


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Parse input to bitmap of occupied indexes
        bitmap = parseInput contents
        simulation = simulationRun bitmap

        -- Part #1 - Score after 10 rounds
        resultP1 = resultPart1 simulation
        -- Part #2 - Round which stopped modifying occupied indexes
        resultP2 = resultPart2 simulation

    -- Output handling
    -- putStrLn $
    --     concat $
    --     map (\b ->
    --         drawBitmap b ++ "\n" ++
    --         drawBitmap (updateReservations b) ++ "\n"
    --     ) $
    --     take 3 simulation
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal23-1.test.txt" -- 110 | 20
    printResult "./data/cal23-2.test.txt" -- 25 | 4
    -- printResult "./data/cal23-1.orig.txt"
