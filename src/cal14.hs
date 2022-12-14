#!/usr/bin/env runhaskell

import System.IO
import Data.List.Split (splitOn)
import Data.List (intercalate, unfoldr)
import Data.Tuple.Extra (dupe)
import qualified Data.Set as Set


-- Index in a 2D grid
type Index = (Int, Int)

-- A type of 2D grid. In this implementation it holds set of all
-- points which are occupied by wall or fallen object
data Bitmap = Bitmap {
    -- Range of bitmap - there is nothing in occupied sets beyond this range
    bitmapRange :: (Index, Index),
    -- Indexes occupied by wall
    bitmapWallOccupied :: Set.Set Index,
    -- Indexes occupied by fallen objects
    bitmapFallenOccupied :: Set.Set Index,
    -- Index from which are objects falling
    bitmapStartIndex :: Maybe Index,
    -- Floor - nothing can fall beyond that point
    -- If set to Nothing - there is no floor
    bitmapFloor :: Maybe Int
} deriving (Show, Eq)


-- Check if index is occupied by wall of floor
bitmapIsWallOccupied :: Index -> Bitmap -> Bool
bitmapIsWallOccupied index bitmap =
    index `Set.member` (bitmapWallOccupied bitmap) ||
    maybe False (\floorY -> (snd index) >= floorY) (bitmapFloor bitmap)

-- Check if index is occupied by fallen object
bitmapIsFallenOccupied :: Index -> Bitmap -> Bool
bitmapIsFallenOccupied index = Set.member index . bitmapFallenOccupied

-- Check if index is occupied by something
bitmapIsOccupied :: Index -> Bitmap -> Bool
bitmapIsOccupied index bitmap = bitmapIsWallOccupied index bitmap || bitmapIsFallenOccupied index bitmap

-- Modify range to include x
bitmapExtendXRange :: Int -> Bitmap -> Bitmap
bitmapExtendXRange x bitmap  = bitmap {
    bitmapRange = newRange
}
    where
        ((sx, sy), (ex, ey)) = bitmapRange bitmap
        nsx = min sx x
        nex = max ex x
        newRange = ((nsx, sy), (nex, ey))

-- Modify range to include y
bitmapExtendYRange :: Int -> Bitmap -> Bitmap
bitmapExtendYRange y bitmap = bitmap {
    bitmapRange = newRange
}
    where
        ((sx, sy), (ex, ey)) = bitmapRange bitmap
        nsy = min sy y
        ney = max ey y
        newRange = ((sx, nsy), (ex, ney))

-- Modify range to include index
bitmapExtendRange :: Index -> Bitmap -> Bitmap
bitmapExtendRange (x, y) = bitmapExtendXRange x . bitmapExtendYRange y

-- Set new floor and extend range if required
bitmapSetFloor :: Int -> Bitmap -> Bitmap
bitmapSetFloor y bitmap = (bitmapExtendYRange y bitmap) {bitmapFloor = Just y}

-- Set start index and extend range if required
bitmapSetStartIndex :: Index -> Bitmap -> Bitmap
bitmapSetStartIndex index bitmap = (bitmapExtendRange index bitmap) {bitmapStartIndex = Just index}

-- Insert new fallen object and extend range if required
bitmapFallenInsert :: Index -> Bitmap -> Bitmap
bitmapFallenInsert index bitmap = (bitmapExtendRange index bitmap) {
    bitmapFallenOccupied = Set.insert index $ bitmapFallenOccupied bitmap
}


-- Parse input line
parseLine :: [String] -> [Index]
parseLine [] = [] -- End of line
parseLine ("->":ls) = parseLine ls -- Skip arrows
parseLine (l:ls) = point : parseLine ls -- Parse point
    where
        point =
            (\[x, y] -> (x, y)) $ -- 3. Convert from list to pair
            map read $ -- 2. Read all numbers
            splitOn "," l -- 1. split by ','

-- Parse input list of lines of points
parseInput :: String -> [[Index]]
parseInput =
    map parseLine . -- 3. Parse lines
    map words . -- 2. Split lines into words
    lines -- 1. Take individual lines

-- Make list of indexes between two points - a line
-- Only straight lines are supported
makeLine :: (Index, Index) -> [Index]
makeLine ((sx, sy), (ex, ey)) = (sx, sy) : rest
    where
        makeRest newStart = makeLine (newStart, (ex, ey))
        rest
            | sx < ex = makeRest (sx+1, sy)
            | sx > ex = makeRest (sx-1, sy)
            | sy < ey = makeRest (sx, sy+1)
            | sy > ey = makeRest (sx, sy-1)
            | otherwise = []

-- Generate bitmap from lines
bitmapFromLines :: [[Index]] -> Bitmap
bitmapFromLines inputLines = Bitmap {
    bitmapRange = range,
    bitmapWallOccupied = Set.fromList indexes,
    bitmapFallenOccupied = Set.empty,
    bitmapStartIndex = Nothing,
    bitmapFloor = Nothing
}
    where
        -- Calculate initial range
        allX = map (\(x, _) -> x) $ concat inputLines
        allY = map (\(_, y) -> y) $ concat inputLines
        sx = minimum allX
        ex = maximum allX
        sy = minimum allY
        ey = maximum allY
        range = ((sx, sy), (ex, ey))
        -- List of all indexes occupied by wall
        indexes =
            concat $ -- 4. And make one big list of them - list of all occupied points
            map makeLine $ -- 3. Convert pairs to lists of points making up those lines
            concat $ -- 2. Make one big list of pairs representing line segments
            map (\l -> zip l (tail l)) inputLines -- 1. Make pairs of (lineStart, lineEnd) for each line segment
                                                  -- (one list of pairs for each set line segments)


-- Simulate falling object, returns modified bitmap including fallen object
-- or Nothing if object has fallen through map or there was no more room for that object
simulateFalling :: Index -> Bitmap -> Maybe Bitmap
simulateFalling index@(x, y) bitmap
    | isOccupied index = Nothing -- There is no more space for this object
    | y > ey = Nothing -- Object as fallen bellow map - there is nothing to stop it from falling infinitely
    | isNotOccupied bellow = tryFall bellow -- Try fall one point bellow
    | isNotOccupied bellowLeft = tryFall bellowLeft -- Try fall one point left and bellow
    | isNotOccupied bellowRight = tryFall bellowRight -- Try fall one point right and bellow
    | otherwise = Just $ bitmapFallenInsert index bitmap -- Can't fall - this is the place where object will come to rest
    where
        -- Extract lowest coordinate on y axis
        (_, (_, ey)) = bitmapRange bitmap
        -- Helper function to check if point is occupied
        isOccupied i = bitmapIsOccupied i bitmap
        -- Helper function to check if point is not occupied (yeah, pretty dumb)
        isNotOccupied = not . isOccupied
        -- Helper function to make recursive call with new index
        tryFall i = simulateFalling i bitmap
        -- Point directly bellow our current index
        bellow = (x, y+1)
        -- Point bellow to the left of our current index
        bellowLeft = (x-1, y+1)
        -- Point bellow to the right of our current index
        bellowRight = (x+1, y+1)


-- Simulate falling of new objects as long as they come to rest
-- Returns list of states of bitmap after each object has fallen
iterateFalling :: Bitmap -> [Bitmap]
iterateFalling bitmap = case bitmapStartIndex bitmap of
    Nothing -> []
    Just startIndex ->
        unfoldr (
            fmap dupe . -- 2. Make pair from result to match unfoldr requirements
            simulateFalling startIndex -- 1. Get updated bitmap with next fallen object
        ) bitmap


-- Get char representing single point of bitmap
drawBitmapPoint :: Index -> Bitmap -> Char
drawBitmapPoint i b
    | bitmapIsFallenOccupied i b = 'o'
    | bitmapIsWallOccupied i b = '#'
    | maybe False ((==) i) (bitmapStartIndex b) = '+'
    | otherwise = '.'

-- Get string representing bitmap
drawBitmap :: Bitmap -> String
drawBitmap bitmap = concat $ [
        [
            drawBitmapPoint (x, y) bitmap
            | x <- [sx..ex]
        ] ++ "\n"
        | y <- [sy..ey]
    ]
    where
        ((sx, sy), (ex, ey)) = bitmapRange bitmap


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Task specific constants
    let startIndex = (500,0)

    -- Main logic
    let -- Parse input to lines of indexes
        lines = parseInput contents
        -- Make bitmap from lines and set start index
        bitmap = bitmapSetStartIndex startIndex $ bitmapFromLines lines
        -- Create modified bitmap with floor
        (_, (_, bitmapLowestPointY)) = bitmapRange bitmap
        bitmapWithFloor = bitmapSetFloor (bitmapLowestPointY + 2) bitmap

        -- Simulate falling objects until no more objects can come to rest
        iterationsPart1 = iterateFalling bitmap
        iterationsPart2 = iterateFalling bitmapWithFloor

        -- Calculate result - how many objects can come to rest
        resultP1 = length iterationsPart1 -- Without floor
        resultP2 = length iterationsPart2 -- With floor

    -- Output handling
    putStrLn $ drawBitmap bitmap
    -- -- putStrLn $ intercalate "\n" $ map drawBitmap iterationsPart1
    -- -- putStrLn $ intercalate "\n" $ map drawBitmap iterationsPart2
    putStrLn $ drawBitmap $ last iterationsPart1
    putStrLn $ drawBitmap $ last iterationsPart2
    -- Results
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal14-1.test.txt" -- 24 | 93
    -- printResult "./data/cal14-1.orig.txt"
