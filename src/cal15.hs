#!/usr/bin/env runhaskell

import System.IO
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set


-- Index in a 2D grid
type Index = (Int, Int)

-- Sensor with range where cannot be another beacon
data Sensor = Sensor {
    sensorPosition :: Index,
    sensorRange :: Int
} deriving (Show, Eq)

-- Position of beacon
type Beacon = Index

-- Map of sensors
data SearchMap = SearchMap {
    searchMapRange :: (Index, Index),
    searchMapSensors :: [Sensor],
    searchMapBeacons :: Set.Set Beacon
} deriving (Show, Eq)


-- Remove all occurrences of string inside string
removeSubstring :: (Eq a) => [a] -> [a] -> [a]
removeSubstring needle = concat . splitOn needle

-- Calculate Manhattan distance
distance :: Index -> Index -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Get proper boundary to cover whole sensor
sensorBoundary :: Sensor -> (Index, Index)
sensorBoundary sensor = ((px-range, py-range), (px+range, py+range))
    where
        (px, py) = sensorPosition sensor
        range = sensorRange sensor

-- Is sensor in range of given point
sensorInRange :: Index -> Sensor -> Bool
sensorInRange index sensor = distanceFromSensor <= range
    where
        distanceFromSensor = distance index $ sensorPosition sensor
        range = sensorRange sensor

-- Is there beacon on that position
searchMapIsBeacon :: Index -> SearchMap -> Bool
searchMapIsBeacon index searchMap = Set.member index $ searchMapBeacons searchMap

-- It this point visible from any sensor
searchMapVisibleFromSensor :: Index -> SearchMap -> Bool
searchMapVisibleFromSensor index searchMap = any (sensorInRange index) $ searchMapSensors searchMap

-- Are we sure there is nothing on that position?
searchMapProvenEmpty :: Index -> SearchMap -> Bool
searchMapProvenEmpty index searchMap =
    not (searchMapIsBeacon index searchMap) &&
    searchMapVisibleFromSensor index searchMap

-- Parse input line to sensor and it's beacon
parseLine :: [String] -> (Sensor, Beacon)
parseLine [
        "Sensor", "at", ('x':'=':x), ('y':'=':y),
        "closest", "beacon", "is", "at", ('x':'=':bx), ('y':'=':by)
    ] = (sensor, beacon)
    where
        fixedX = removeSubstring "," x
        fixedY = removeSubstring ":" y
        fixedBX = removeSubstring "," bx
        fixedBY = by
        sensorPos = (read fixedX, read fixedY)
        beacon = (read fixedBX, read fixedBY)
        sensor = Sensor {
            sensorPosition = sensorPos,
            sensorRange = distance sensorPos beacon
        }

-- Parse input into SearchMap
parseInput :: String -> SearchMap
parseInput input = SearchMap {
    searchMapRange = mapRange,
    searchMapSensors = sensors,
    searchMapBeacons = Set.fromList beaconsNoDuplicity
}
    where
        inputLines = map words $ lines input
        -- Parse input lines and use unzip to make separate lists of all sensors and beacons
        (sensors, beacons) = unzip $ map parseLine inputLines
        beaconsNoDuplicity = nub beacons -- Deduplicate beacons
        (allX, allY) =
            unzip $ -- 5. Split x and y coordinates into separate lists
            beacons ++ ( -- 4. Add also all indexes of beacons (this should be redundant)
                uncurry (++) $ -- 3. And join them into single long list of indexes
                unzip $ -- 2. Split to two lists of indexes
                map sensorBoundary sensors -- 1. Get boundaries of all sensors
            )
        -- Find boundaries
        sx = minimum allX
        sy = minimum allY
        ex = maximum allX
        ey = maximum allY
        mapRange = ((sx, sy), (ex, ey))


-- For how many points on one line are we sure there are no beacons?
resultPart1 :: Int -> SearchMap -> Int
resultPart1 y searchMap = length $ filter (flip searchMapProvenEmpty searchMap) $ [(x, y) | x <- [sx..ex]]
    where ((sx, _), (ex, _)) = searchMapRange searchMap

-- Find end of sensor range on line
moveBySensor :: Index -> Sensor -> Index
moveBySensor (_, y) sensor = (sx + distanceX + 1, y)
    where
        (sx, sy) = sensorPosition sensor
        range = sensorRange sensor
        distanceX = range - abs (y-sy)

-- Search a line for position outside of all sensors
checkLine :: Index -> Int -> SearchMap -> Maybe Index
checkLine index@(x, _) maxX searchMap = if x > maxX then Nothing else result
    where
        sensorsInRange = filter (sensorInRange index) $ searchMapSensors searchMap
        result = case sensorsInRange of
            [] -> Just index
            sensor:_ -> checkLine (moveBySensor index sensor) maxX searchMap

-- Is there any point in range 0-maxXY which is outside of all sensors?
-- Calculate (x * 4000000 + y) for it
resultPart2 :: Int -> SearchMap -> Int
resultPart2 maxXY searchMap = result
    where
        -- Calculate search limits
        ((sx, sy), (ex, ey)) = searchMapRange searchMap
        nsx = max 0 sx
        nsy = max 0 sy
        nex = min maxXY ex
        ney = min maxXY ey
        -- List of all indexes outside of all sensors
        validIndexes = catMaybes $ [checkLine (nsx, y) nex searchMap | y <- [nsy..ney]]
        [(x, y)] = validIndexes -- We expect only one possible solution
        result = x * 4000000 + y


printResult file targetY maxXY = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Parse input to SearchMap
        searchMap = parseInput contents

        -- Part #1
        resultP1 = resultPart1 targetY searchMap
        -- Part #2
        resultP2 = resultPart2 maxXY searchMap

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal15-1.test.txt" 10 20 -- 26 | 56000011
    -- printResult "./data/cal15-1.orig.txt" 2000000 4000000
