#!/usr/bin/env runhaskell

import Debug.Trace (trace, traceShowId)
import Data.List.Split (splitOn)
import Data.List (intercalate, unfoldr)
import Data.Tuple.Extra (dupe)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set


-- This solution is not yet finished/polished
-- I was able to get all required answers with this solution,
-- but lot of things related to repeating windows are hardcoded
-- Also most logic is undocumented


-- Index in a 2D grid
type Index = (Integer, Integer)

-- A type of 2D grid. In this implementation it holds set of all
-- points which are occupied by wall or fallen object
data Bitmap = Bitmap {
    -- Range of bitmap - there is nothing in occupied sets beyond this range
    bitmapRange :: (Index, Index),
    -- Indexes occupied by fallen objects
    bitmapFallenOccupied :: Set Index,
    -- Floor - nothing can fall beyond that point
    bitmapFloor :: Integer,
    bitmapWalls :: (Integer, Integer)
} deriving (Show, Eq)


-- Check if index is occupied by wall of floor
bitmapIsWallOccupied :: Index -> Bitmap -> Bool
bitmapIsWallOccupied index@(x, y) bitmap =
    x <= wl || x >= wr || y >= bitmapFloor bitmap
    where (wl, wr) = bitmapWalls bitmap

-- Check if index is occupied by fallen object
bitmapIsFallenOccupied :: Index -> Bitmap -> Bool
bitmapIsFallenOccupied index = Set.member index . bitmapFallenOccupied

-- Check if index is occupied by something
bitmapIsOccupied :: Index -> Bitmap -> Bool
bitmapIsOccupied index bitmap = bitmapIsWallOccupied index bitmap || bitmapIsFallenOccupied index bitmap

-- Modify range to include x
bitmapExtendXRange :: Integer -> Bitmap -> Bitmap
bitmapExtendXRange x bitmap  = bitmap {
    bitmapRange = newRange
}
    where
        ((sx, sy), (ex, ey)) = bitmapRange bitmap
        nsx = min sx x
        nex = max ex x
        newRange = ((nsx, sy), (nex, ey))

-- Modify range to include y
bitmapExtendYRange :: Integer -> Bitmap -> Bitmap
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

-- Insert new fallen object and extend range if required
bitmapFallenInsert :: Index -> Bitmap -> Bitmap
bitmapFallenInsert index bitmap = (bitmapExtendRange index bitmap) {
    bitmapFallenOccupied = Set.insert index $ bitmapFallenOccupied bitmap
}

charToWind :: Char -> Integer
charToWind '>' = 1
charToWind '<' = -1
-- charToWind c = trace (show c) undefined

-- Parse input list of wind strength
parseInput :: String -> [Integer]
parseInput = map charToWind . concat . lines


data SimulatorState = SimulatorState {
    simulatorStateBitmap :: Bitmap,
    simulatorStateWind :: [Integer],
    simulatorStatePieces :: [[[Bool]]]
} deriving (Show, Eq)

pieceIndexes :: [[Bool]] -> Index -> [Index]
pieceIndexes piece (x, y) = catMaybes $ concat $ map mapPoints $ zip yIndexes piece
    where
        yIndexes = reverse [0..(toInteger $ length piece - 1)]
        mapPoints (yd, line) = map (pointIndex yd) $ zip [0..] line
        pointIndex yd (xd, True) = Just (x+xd, y-yd)
        pointIndex _ _ = Nothing

pieceOverlaps :: [[Bool]] -> Index -> Bitmap -> Bool
pieceOverlaps piece index bitmap = any (flip bitmapIsOccupied bitmap) $ pieceIndexes piece index

bitmapPieceInsert :: [[Bool]] -> Index -> Bitmap -> Bitmap
bitmapPieceInsert piece index bitmap = foldr (bitmapFallenInsert) bitmap $ pieceIndexes piece index

-- Simulate falling object, returns modified bitmap including fallen object
-- or Nothing if object has fallen through map or there was no more room for that object
simulateFalling :: Index -> SimulatorState -> SimulatorState
simulateFalling index@(x, y) state
    | pieceOverlaps currentPiece fallIndex bitmap = stateEnd
    -- trace (show (index, indexBeforeFall)) $
    | otherwise = simulateFalling fallIndex stateIter
    -- trace (show (index, fallIndex)) $
    -- drawBitmap $ simulatorStateBitmap stateEnd
    where
        bitmap = simulatorStateBitmap state
        (currentWind:nextWind) = simulatorStateWind state
        (currentPiece:nextPieces) = simulatorStatePieces state
        
        windIndex = (x+currentWind, y)
        indexBeforeFall@(x2, y2) = if pieceOverlaps currentPiece windIndex bitmap then index else windIndex
        fallIndex = (x2, y2+1)

        stateIter = state {
            simulatorStateWind = nextWind
        }
        stateEnd = stateIter {
            simulatorStateBitmap = bitmapPieceInsert currentPiece indexBeforeFall bitmap,
            simulatorStatePieces = nextPieces
        }

startIndexFor :: SimulatorState -> Index
startIndexFor state = (3, sy - 4)
    where ((_, sy), _) = bitmapRange $ simulatorStateBitmap state

-- Simulate falling of new objects as long as they come to rest
-- Returns list of states of bitmap after each object has fallen
iterateFalling :: SimulatorState -> [SimulatorState]
iterateFalling state = tail $ iterate (\ state -> simulateFalling (startIndexFor state) state) state


-- Get char representing single point of bitmap
drawBitmapPoint :: Index -> Bitmap -> Char
drawBitmapPoint i b
    | bitmapIsFallenOccupied i b = '#'
    | bitmapIsWallOccupied i b = '█'
    | otherwise = '.'

-- Get string representing bitmap
drawBitmap :: Bitmap -> String
drawBitmap bitmap = concat $ [
        [
            drawBitmapPoint (x, y) bitmap
            | x <- [sx..ex]
        ] ++ show y ++ "\n"
        | y <- [sy..ey]
    ]
    where
        ((sx, sy), (ex, ey)) = bitmapRange bitmap


pieces :: [[[Bool]]]
pieces = cycle $ map (map (map chToB)) [
        [
            "####"
        ],
        [
            ".#.",
            "###",
            ".#."
        ],
        [
            "..#",
            "..#",
            "###"
        ],
        [
            "#",
            "#",
            "#",
            "#"
        ],
        [
            "##",
            "##"
        ]
    ]
    where
        chToB '#' = True
        chToB '.' = False

initSimulation :: [Integer] -> SimulatorState
initSimulation wind = SimulatorState {
    simulatorStateBitmap = Bitmap {
        bitmapRange = ((0, 0), (8, 0)),
        bitmapFallenOccupied = Set.empty,
        bitmapFloor = 0,
        bitmapWalls = (0, 8)
    },
    simulatorStateWind = wind,
    simulatorStatePieces = pieces
}

matchBlock :: Bitmap -> (Index, Index) -> (Index, Index) -> Bool
matchBlock bitmap ((sx1, sy1), (ex1, ey1)) ((sx2, sy2), (ex2, ey2)) = and [
        bitmapIsFallenOccupied (x1, y1) bitmap == bitmapIsFallenOccupied (x2, y2) bitmap |
        (x1, x2) <- zip [sx2..ex2] [sx1..ex1],
        (y1, y2) <- zip [sy2..ey2] [sy1..ey1]
    ]

matchWindow :: SimulatorState -> Integer -> [(Integer, Integer, Integer)]
matchWindow state window =
    -- filter (\(y1, y2, _) -> y1 /= y2) $
    map (\y -> (y + 5, sy + 5, y - sy)) $
    filter (\y -> matchBlock bitmap ((sx, sy + 5), (ex, sy + 5 + window)) ((sx, y + 5), (ex, y + 5 + window))) [(sy+1)..(ey - window)]
    where
        bitmap = simulatorStateBitmap state
        ((sx, sy), (ex, ey)) = bitmapRange bitmap


printResult file = do
    -- Reading file
    contents <- readFile file

    -- -- Task specific constants
    -- let startIndex = (500,0)

    -- Main logic
    let -- Parse input to wind pattern
        wind = parseInput contents
        simulation = initSimulation $ cycle wind
    --     -- Make bitmap from lines and set start index
    --     bitmap = bitmapSetStartIndex startIndex $ bitmapFromLines lines
    --     -- Create modified bitmap with floor
    --     (_, (_, bitmapLowestPointY)) = bitmapRange bitmap
    --     bitmapWithFloor = bitmapSetFloor (bitmapLowestPointY + 2) bitmap

    --     -- Simulate falling objects until no more objects can come to rest
        iterationsPart1 = iterateFalling simulation
        i105 = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 105 iterationsPart1
        i140 = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 140 iterationsPart1
        a35 = i140 - i105

        -- i2022a = i105 + a35 * ((2022 - 105) `div` 35)
        -- i2022m = (2022 - 105) `mod` 35
        -- i2022s = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take (105+i2022m) iterationsPart1
        -- i2022r = i2022s - i105
        -- i2022 = i2022a + i2022r
        -- i1000000000000a = i105 + a35 * ((1000000000000 - 105) `div` 35)
        -- i1000000000000m = (1000000000000 - 105) `mod` 35
        -- i1000000000000s = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take (105+i1000000000000m) iterationsPart1
        -- i1000000000000r = i1000000000000s - i105
        -- i1000000000000 = i1000000000000a + i1000000000000r

        i2093 = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 2093 iterationsPart1
        i3838 = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 3838 iterationsPart1
        a1745 = i3838 - i2093
        
        -- i2022a = i2093 + a1745 * ((2022 - 2093) `div` 1745)
        -- i2022m = (2022 - 2093) `mod` 1745
        -- i2022s = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take (2093+i2022m) iterationsPart1
        -- i2022r = i2022s - i2093
        i2022 = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 2022 iterationsPart1 -- i2022a + i2022r
        i1000000000000a = i2093 + a1745 * ((1000000000000 - 2093) `div` 1745)
        i1000000000000m = (1000000000000 - 2093) `mod` 1745
        i1000000000000s = negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take (2093+i1000000000000m) iterationsPart1
        i1000000000000r = i1000000000000s - i2093
        i1000000000000 = i1000000000000a + i1000000000000r

    --     iterationsPart2 = iterateFalling bitmapWithFloor

    --     -- Calculate result - how many objects can come to rest
    --     resultP1 = length iterationsPart1 -- Without floor
    --     resultP2 = length iterationsPart2 -- With floor



    -- -- Output handling
    -- print $ length wind
    --
    -- filter (not . null . snd) $
    -- mapM print $ zip [((length wind*10)+1)..] $ map (flip matchWindow (toInteger $ length wind * 5)) $ drop (length wind*10) iterationsPart1
    -- mapM print $ zip [((1500)+1)..] $ map (flip matchWindow (toInteger $ 2750)) $ drop 1500 iterationsPart1

    -- filter (\ (_, _, f, s) -> f == 3068 || s == 1514285714288)
    -- filter (\ (_, _, s) -> s == 1514285714288)
    -- mapM print $ map (\ (i, v) -> (i, v, (v * 1000000000000) `div` i)) $zip [1..] $ map (negate . snd . fst . bitmapRange . simulatorStateBitmap) $ iterationsPart1

    -- putStrLn $ drawBitmap $ simulatorStateBitmap $ last $ take 150 iterationsPart1
    
    -- putStrLn $ drawBitmap $ simulatorStateBitmap $ last $ take 2093 iterationsPart1
    -- putStrLn $ drawBitmap $ simulatorStateBitmap $ last $ take (length wind + 2) iterationsPart1
    -- putStrLn $ drawBitmap $ simulatorStateBitmap $ last $ take (length wind * 2) iterationsPart1
    -- putStrLn $ drawBitmap $ simulatorStateBitmap $ last $ take (length wind * 2 + 1) iterationsPart1
    -- putStrLn $ drawBitmap $ simulatorStateBitmap $ last $ take (length wind * 2 + 2) iterationsPart1

    -- print $ negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take (length wind) iterationsPart1
    -- print $ length wind
    -- print $ 2022 `div` length wind
    -- mapM putStrLn $ map drawBitmap $ map simulatorStateBitmap $ take 5 iterationsPart1

    -- print $ negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 2022 iterationsPart1
    -- print $ negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 2093 iterationsPart1
    -- print i105
    -- print i140
    -- print a35
    -- print l35
    -- print n35
    print i2022
    -- print i2022m
    print i1000000000000
    -- print i1000000000000m

    -- mapM putStrLn $ map show $ map (negate.snd.fst) $ map bitmapRange $ map simulatorStateBitmap $ take 1000000000000 iterationsPart1
    -- print $ negate $ snd $ fst $ bitmapRange $ simulatorStateBitmap $ last $ take 1000000000000 $ iterationsPart1
    -- -- -- putStrLn $ intercalate "\n" $ map drawBitmap iterationsPart1
    -- -- -- putStrLn $ intercalate "\n" $ map drawBitmap iterationsPart2
    -- putStrLn $ drawBitmap $ last iterationsPart1
    -- putStrLn $ drawBitmap $ last iterationsPart2
    -- -- Results
    -- putStrLn $ "'" ++ file ++ "': '" ++
    --     show resultP1 ++ "' | '" ++
    --     show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    -- printResult "./data/cal17-1.test.txt" -- 3068 | 1514285714288
    printResult "./data/cal17-1.orig.txt"
