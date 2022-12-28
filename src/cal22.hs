#!/usr/bin/env runhaskell

-- import Debug.Trace (traceShowId, traceShow, trace)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.List.Split (splitOn)
import Data.Ix (range, inRange)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


-- Index in a 2D grid
type Index = (Int, Int)

-- Index in a 2D grid with direction
type Position = (Int, Int, Direction)

-- Range in 2D grid
type Range = (Index, Index)


-- Representation of direction
data Direction
    = DRight
    | DDown
    | DLeft
    | DUp
      deriving (Show, Eq)

-- List of all directions
directions :: [Direction]
directions = [
        DRight,
        DDown,
        DLeft,
        DUp
    ]

-- Move associated to direction
directionMove :: Direction -> Index
directionMove DRight = (1,  0)
directionMove DLeft  = (-1, 0)
directionMove DDown  = (0,  1)
directionMove DUp    = (0, -1)

-- List of all directions with their moves
directionsMoves :: [(Direction, Index)]
directionsMoves = zip directions $ map directionMove directions

-- Number associated with direction - for score and implementation of Hashable
directionValue :: Direction -> Int
directionValue DRight = 0
directionValue DDown = 1
directionValue DLeft = 2
directionValue DUp = 3

instance Hashable Direction where
    hashWithSalt v dir = v + directionValue dir
    hash dir = directionValue dir

-- Basic plus of two directions
-- (can also be implemented arithmetically with plus and modulo)
directionRotate :: Direction -> Direction -> Direction
directionRotate DRight DRight = DDown
directionRotate DRight DDown  = DLeft
directionRotate DRight DLeft  = DUp
directionRotate DRight DUp    = DRight

directionRotate DDown  DRight = DLeft
directionRotate DDown  DDown  = DUp
directionRotate DDown  DLeft  = DRight
directionRotate DDown  DUp    = DDown

directionRotate DLeft  DRight = DUp
directionRotate DLeft  DDown  = DRight
directionRotate DLeft  DLeft  = DDown
directionRotate DLeft  DUp    = DLeft

directionRotate DUp    DRight = DRight
directionRotate DUp    DDown  = DDown
directionRotate DUp    DLeft  = DLeft
directionRotate DUp    DUp    = DUp

-- Basic minus of two directions
-- (can also be implemented arithmetically with minus and modulo)
directionRotateInverse :: Direction -> Direction -> Direction
directionRotateInverse DRight DRight = DUp
directionRotateInverse DRight DDown  = DLeft
directionRotateInverse DRight DLeft  = DDown
directionRotateInverse DRight DUp    = DRight

directionRotateInverse DDown  DDown  = DUp
directionRotateInverse DDown  DLeft  = DLeft
directionRotateInverse DDown  DUp    = DDown
directionRotateInverse DDown  DRight = DRight

directionRotateInverse DLeft  DLeft  = DUp
directionRotateInverse DLeft  DUp    = DLeft
directionRotateInverse DLeft  DRight = DDown
directionRotateInverse DLeft  DDown  = DRight

directionRotateInverse DUp    DUp    = DUp
directionRotateInverse DUp    DRight = DLeft
directionRotateInverse DUp    DDown  = DDown
directionRotateInverse DUp    DLeft  = DRight


-- Representation of cube side
data CubeSide
    = CFront
    | CRight
    | CBack
    | CLeft
    | CTop
    | CBottom
      deriving (Show, Eq)

-- Number associated with cube side - for implementation of Hashable
cubeSideValue :: CubeSide -> Int
cubeSideValue CFront = 0
cubeSideValue CRight = 1
cubeSideValue CBack = 2
cubeSideValue CLeft = 3
cubeSideValue CTop = 4
cubeSideValue CBottom = 5

instance Hashable CubeSide where
    hashWithSalt v side = v + cubeSideValue side
    hash side = cubeSideValue side

-- Bitmap of 2D space with possibility of undefined indexes
type Bitmap = HashMap Index Bool

-- Map of possible moves - next position for each possible position
type MoveMap = HashMap Position Position

-- Map of locations of all cube sides
type CubeMap = HashMap CubeSide (Index, Direction)

-- Representation of all possible actions
data Action
    = ActionLeft
    | ActionRight
    | ActionMove
      deriving (Show, Eq)


-- Convert a line to a list of actions
parseActions :: String -> [Action]
parseActions "" = []
parseActions ('L':as) = ActionLeft : parseActions as
parseActions ('R':as) = ActionRight : parseActions as
parseActions a = replicate num ActionMove ++ parseActions as
    where
        -- Split a to get number and rest of input
        (numStr, as) = span (\c -> c /= 'L' && c /= 'R') a
        num = read numStr

-- Parse character for bitmap representation
parseWallChar :: Char -> Index -> Maybe (Index, Bool)
parseWallChar ' ' _ = Nothing -- Void - don't save in bitmap
parseWallChar '.' i = Just (i, False) -- Free - Save False to bitmap
parseWallChar '#' i = Just (i, True) -- Wall - Save True to bitmap

-- Parse list of lines to a bitmap of walls and size of this map
parseWallMap :: [String] -> (Bitmap, Range)
parseWallMap input = (HashMap.fromList mapItems, mapRange)
    where
        mapItems =
            catMaybes $
            concat $
            map (\(y, l) ->
                map (\(x, c) -> parseWallChar c (x, y)) $
                zip [1..] l
            ) $
            zip [1..] input
        indices = map fst mapItems
        allX = map fst indices
        allY = map snd indices
        minX = minimum allX
        minY = minimum allY
        maxX = maximum allX
        maxY = maximum allY
        mapRange = ((minX, minY), (maxX, maxY))

-- Parse input into wall map, size of wall map and list of actions
parseInput :: String -> (Bitmap, Range, [Action])
parseInput input = (wallMap, wallMapRange, actions)
    where
        [wallMapStr, [actionsStr]] = splitOn [""] $ lines input
        (wallMap, wallMapRange) = parseWallMap wallMapStr
        actions = parseActions actionsStr


-- Initial implementation of cube edge transformation - hardcoded for test input
-- Great for debugging of real automatic mapping
-- cubeEdgeForLegacy :: Index -> Direction -> Position
-- cubeEdgeForLegacy (x, y) DRight
--     | y <= 4 = (16, 13 - y, DLeft)
--     | y <= 8 = (17 - (y - 4), 9, DDown)
--     | otherwise = (12, y - 8, DLeft)
-- cubeEdgeForLegacy (x, y) DDown
--     | x <= 4 = (8 + x, 12, DUp)
--     | x <= 8 = (9, 13 - (x - 4), DRight)
--     | x <= 12 = (5 - (x - 8), 8, DUp)
--     | otherwise = (1, 9 - (x - 12), DRight)
-- cubeEdgeForLegacy (x, y) DLeft
--     | y <= 4 = (9 - y, 5, DDown)
--     | y <= 8 = (17 - (y - 4), 12, DUp)
--     | otherwise = (4 + y - 8, 8, DUp)
-- cubeEdgeForLegacy (x, y) DUp
--     | x <= 4 = (13 - x, 1, DDown)
--     | x <= 8 = (9, x - 4, DRight)
--     | x <= 12 = (5 - (x - 8), 5, DDown)
--     | otherwise = (12, 9 - (x - 12), DLeft)


-- Rotates the position 90 degrees clockwise.
indexRotate90 :: Int -> Index -> Index
indexRotate90 size (x, y) = (size - y + 1, x)

-- Rotates the position 180 degrees clockwise.
indexRotate180 :: Int -> Index -> Index
indexRotate180 size (x, y) = (size - x + 1, size - y + 1)

-- Rotates the position 270 degrees clockwise.
indexRotate270 :: Int -> Index -> Index
indexRotate270 size (x, y) = (y, size - x + 1)

-- Flips the position vertically
indexFlipX :: Int -> Index -> Index
indexFlipX size (x, y) = (size - x + 1, y)

-- Flips the position horizontally
indexFlipY :: Int -> Index -> Index
indexFlipY size (x, y) = (x, size - y + 1)

-- Keeps the position unchanged
indexKeep :: Int -> Index -> Index
indexKeep _ = id


-- Rotate position to match new facing of cube side
-- Solves this task:
-- I'm going to the direction1 and I've reached the edge of cube,
-- I know I'll be going to the direction2 on the upcoming cube side,
-- What is my position on the upcoming cube side once I make next step?
indexRotate :: Direction -> Direction -> Int -> Index -> Index
indexRotate DRight DRight s = indexFlipX s
indexRotate DRight DDown  s = indexFlipX s . indexRotate270 s
indexRotate DRight DLeft  s = indexFlipY s
indexRotate DRight DUp    s = indexFlipX s . indexRotate90 s

indexRotate DDown  DRight s = indexFlipY s . indexRotate90 s
indexRotate DDown  DDown  s = indexFlipY s
indexRotate DDown  DLeft  s = indexFlipY s . indexRotate270 s
indexRotate DDown  DUp    s = indexFlipX s

indexRotate DLeft  DRight s = indexFlipY s
indexRotate DLeft  DDown  s = indexFlipX s . indexRotate90 s
indexRotate DLeft  DLeft  s = indexFlipX s
indexRotate DLeft  DUp    s = indexFlipX s . indexRotate270 s

indexRotate DUp    DRight s = indexFlipY s . indexRotate270 s
indexRotate DUp    DDown  s = indexFlipX s
indexRotate DUp    DLeft  s = indexFlipY s . indexRotate90 s
indexRotate DUp    DUp    s = indexFlipY s

-- Transform position to match rotation of real index of cube side, instead of grid index
-- Takes direction of cube side in grid (which way is a top of that side facing)
indexTransformation :: Direction -> Int -> Index -> Index
indexTransformation DRight = indexRotate270
indexTransformation DDown  = indexRotate180
indexTransformation DLeft  = indexRotate90
indexTransformation DUp    = indexKeep

-- Transform position rotation of cube side back to the grid index
-- Takes direction of cube side in grid (which way is a top of that side facing)
indexTransformationInverse :: Direction -> Int -> Index -> Index
indexTransformationInverse DRight = indexRotate90
indexTransformationInverse DDown  = indexRotate180
indexTransformationInverse DLeft  = indexRotate270
indexTransformationInverse DUp    = indexKeep


-- Find new cube side and our direction, once we reach the edge going given direction
cubeEdge :: CubeSide -> Direction -> (CubeSide, Direction)
cubeEdge CFront  DRight = (CRight,  DRight)
cubeEdge CRight  DRight = (CBack,   DRight)
cubeEdge CBack   DRight = (CLeft,   DRight)
cubeEdge CLeft   DRight = (CFront,  DRight)
cubeEdge CTop    DRight = (CRight,  DDown )
cubeEdge CBottom DRight = (CLeft,   DUp   )

cubeEdge CFront  DDown  = (CBottom, DUp   )
cubeEdge CRight  DDown  = (CBottom, DRight)
cubeEdge CBack   DDown  = (CBottom, DDown )
cubeEdge CLeft   DDown  = (CBottom, DLeft )
cubeEdge CTop    DDown  = (CFront,  DDown )
cubeEdge CBottom DDown  = (CFront,  DUp   )

cubeEdge CFront  DLeft  = (CLeft,   DLeft )
cubeEdge CRight  DLeft  = (CFront,  DLeft )
cubeEdge CBack   DLeft  = (CRight,  DLeft )
cubeEdge CLeft   DLeft  = (CBack,   DLeft )
cubeEdge CTop    DLeft  = (CLeft,   DDown )
cubeEdge CBottom DLeft  = (CRight,  DUp   )

cubeEdge CFront  DUp    = (CTop,    DUp   )
cubeEdge CRight  DUp    = (CTop,    DLeft )
cubeEdge CBack   DUp    = (CTop,    DDown )
cubeEdge CLeft   DUp    = (CTop,    DRight)
cubeEdge CTop    DUp    = (CBack,   DDown )
cubeEdge CBottom DUp    = (CBack,   DUp   )


-- Range of cube side inside grid
cubeSideRange :: Int -> Index -> (Index, Index)
cubeSideRange sideSize (x, y) = ((x, y), (x+sideSize-1, y+sideSize-1))

-- Transform grid index to cube side index
cubeSideIndexFor :: Int -> Index -> Direction -> Index -> Index
cubeSideIndexFor sideSize (sx, sy) sideDirection (x, y) =
        indexTransformation sideDirection sideSize cornerIndex
    where cornerIndex = (x-sx+1, y-sy+1)

-- Transform cube side index back to grid index
gridIndexFor :: Int -> Index -> Direction -> Index -> Index
gridIndexFor sideSize (sx, sy) sideDirection cubeIndex = (x+sx-1, y+sy-1)
    where (x, y) = indexTransformationInverse sideDirection sideSize cubeIndex

-- Find next position from given index, assuming we are standing on the edge of cube
cubeEdgeFor :: CubeMap -> Int -> Index -> Direction -> Position
cubeEdgeFor cubeMap sideSize index direction = (x, y, nextDirection)
    where
        -- Find cube side of index
        [(cubeSide, (cubeSideIndex, cubeSideDirection))] =
            filter (
                flip inRange index .
                cubeSideRange sideSize .
                fst . snd
            ) $
            HashMap.toList cubeMap

        -- Convert index to cube side coordinates
        cubeIndex = cubeSideIndexFor sideSize cubeSideIndex cubeSideDirection index
        -- Convert direction to correspond to direction on the cube side
        cubeDirection = directionRotateInverse direction cubeSideDirection

        -- Get upcoming cube side and direction of movement on that side
        (nextSide, nextCubeDirection) = cubeEdge cubeSide cubeDirection
        -- Get our position on the upcoming side once we make a step
        nextCubeIndex = indexRotate cubeDirection nextCubeDirection sideSize cubeIndex

        -- Lookup additional info about upcoming side
        Just (nextSideIndex, nextSideDirection) = HashMap.lookup nextSide cubeMap

        -- Convert position on the upcoming side to position on the grid
        (x, y) = gridIndexFor sideSize nextSideIndex nextSideDirection nextCubeIndex
        -- Convert our direction on the upcoming side to the direction on the grid
        nextDirection = directionRotate nextCubeDirection nextSideDirection

        -- Legacy position for debugging - might be partially broken
        -- legacyPosition = cubeEdgeForLegacy index direction

-- Add two index together, overflowing to the other side of range
indexAdd :: Range -> Index -> Index -> Index
indexAdd ((sx, sy), (ex, ey)) (x1, y1) (x2, y2) = (
        ((x1 + x2 - sx) `mod` (ex + 2 - sx)) + sx,
        ((y1 + y2 - sy) `mod` (ey + 2 - sy)) + sy
    )

-- Find position of next step for basic grid which does not represent a cube
nextIndexForNonCube :: Bitmap -> Range -> Index -> Index -> Direction -> Position
nextIndexForNonCube wallMap wallMapRange initialIndex@(ix, iy) index direction =
    case HashMap.lookup nextIndex wallMap of
        Just False -> (nx, ny, direction) -- Free - directly in front of us
        Just True -> (ix, iy, direction) -- Wall - directly in front of us
        Nothing -> nextIndexForNonCube wallMap wallMapRange initialIndex nextIndex direction -- Void - wrap around
    where
        nextIndex@(nx, ny) = indexAdd wallMapRange index $ directionMove direction

-- Find position of next step for a grid representing a cube part of task
nextIndexForCube :: CubeMap -> Int -> Bitmap -> Range -> Index -> Direction -> Position
nextIndexForCube cubeMap sideSize wallMap wallMapRange index@(x, y) direction =
    case HashMap.lookup nextIndex wallMap of
        Just False -> nextPosition -- Free - directly in front of us
        Just True -> currentPosition -- Wall - directly in front of us
        Nothing -> case HashMap.lookup cubeIndex wallMap of -- Void - we are on the edge
            Just False -> cubePosition -- Free - on the other side of edge
            Just True -> currentPosition -- Wall - on the other side of edge
            -- Nothing -> traceShow (index, direction, cubePosition) undefined -- Debug
    where
        currentPosition = (x, y, direction)
        nextIndex@(nx, ny) = indexAdd wallMapRange index $ directionMove direction
        nextPosition = (nx, ny, direction)
        cubePosition@(cubeX, cubeY, _) = cubeEdgeFor cubeMap sideSize index direction
        cubeIndex = (cubeX, cubeY)

-- Find position of next step in a grid
nextIndexFor :: Maybe (CubeMap, Int) -> Bitmap -> Range -> Index -> Direction -> Position
nextIndexFor Nothing wallMap wallMapRange index direction =
    nextIndexForNonCube wallMap wallMapRange index index direction
nextIndexFor (Just (cubeMap, sideSize)) wallMap wallMapRange index direction =
    nextIndexForCube cubeMap sideSize wallMap wallMapRange index direction

-- Make all possible moves from given position and collect their results to a list
makeMoves :: Maybe (CubeMap, Int) -> Bitmap -> Range -> Index -> [(Position, Position)]
makeMoves cube wallMap wallMapRange index@(x, y) =
    case HashMap.lookup index wallMap of
        Just False -> paths
        otherwise -> [] -- Given position is not free - nothing will be moving from there
    where
        -- Go through all directions and find their next position
        paths = map (\dir -> (
                (x, y, dir),
                nextIndexFor cube wallMap wallMapRange index dir
            )) directions

-- Make a map of all possible moves - next position for each position
makeMoveMap :: Maybe (CubeMap, Int) -> Bitmap -> Range -> MoveMap
makeMoveMap cube wallMap wallMapRange = HashMap.fromList $ concat $ map (makeMoves cube wallMap wallMapRange) $ range wallMapRange


-- Find index of first free index in first line of grid
findStartIndex :: Bitmap -> Index -> Index
findStartIndex wallMap index@(x, y) = case HashMap.lookup index wallMap of
    Just False -> (x, y)
    otherwise -> findStartIndex wallMap (x+1, y)

-- Add starting direction to an index
startPositionFor :: Index -> Position
startPositionFor (x, y) = (x, y, DRight)


-- Prepare info about neighbor on the 'neighborDirection' side of given side
-- Takes into account how is current side rotated in relation to the grid
cubeSideNeighborFor :: Int -> CubeSide -> Index -> Direction -> Direction -> (CubeSide, Index, Direction)
cubeSideNeighborFor sideSize side (x, y) direction neighborDirection = (nextSide, neighborIndex, neighborRotation)
    where
        -- Actual direction whin given side
        realDirection = directionRotateInverse neighborDirection direction
        -- Neighbor side and direction of movement related to realDirection
        (nextSide, nextDirection) = cubeEdge side realDirection
        -- Rotation of neighbor side in relation to given side
        neighborRotation = directionRotate direction $ directionRotateInverse realDirection nextDirection
        -- Move direction of current index
        (dx, dy) = directionMove neighborDirection
        -- Left upper corner index of nextSide
        neighborIndex = (x + dx*sideSize, y + dy*sideSize)

-- Prepare info about all possible neighbors of given side as a list
cubeSideNeighbors :: Int -> CubeSide -> Index -> Direction -> [(CubeSide, Index, Direction)]
cubeSideNeighbors sideSize side index direction =
    map (cubeSideNeighborFor sideSize side index direction) directions

-- Recursively check all neighbors of given side, generating list of actual sides with their info
checkCubeSideNeighbors :: Int -> Bitmap -> Set Index -> CubeSide -> Index -> Direction -> [(CubeSide, (Index, Direction))]
checkCubeSideNeighbors sideSize wallMap visited side index direction =
    concat $
    map (\(nSide, nIndex, nDirection) -> checkCubeSide sideSize wallMap visited nSide nIndex nDirection) $
    cubeSideNeighbors sideSize side index direction

-- Check if info about given side corresponds to a valid side and that side was not yet evaluated.
-- For those sides returns a list with input side and all its neighbors which were not yet evaluated.
checkCubeSide :: Int -> Bitmap -> Set Index -> CubeSide -> Index -> Direction -> [(CubeSide, (Index, Direction))]
checkCubeSide sideSize wallMap visited side index direction =
    case (index `Set.member` visited, HashMap.lookup index wallMap) of
        (True, _) -> [] -- Already processed
        (_, Nothing) -> [] -- Outside of map
        (False, Just _) -> -- Valid side
            (side, (index, direction)) : -- Put it in list
            checkCubeSideNeighbors sideSize wallMap nextVisited side index direction -- And evaluate all its neighbors
    where
        nextVisited = Set.insert index visited

-- Make a map of all cube sides within a grid
makeCubeMap :: Int -> Bitmap -> Index -> CubeMap
makeCubeMap sideSize wallMap startIndex =
    HashMap.fromList $
    checkCubeSide sideSize wallMap Set.empty CFront startIndex DUp


-- Using a map of all possible moves. apply action to the current position
applyAction :: MoveMap -> Action -> Position -> Position
applyAction _ ActionLeft (x, y, dir) = (x, y, directionRotate dir DLeft)
applyAction _ ActionRight (x, y, dir) = (x, y, directionRotate dir DRight)
applyAction moveMap ActionMove position = fromJust $ HashMap.lookup position moveMap -- fromMaybe position


-- Convert point in bitmap with optional path to char
drawPoint :: Bitmap -> [(Index, Position)] -> Index -> Char
drawPoint wallMap path index@(x, y) = 
    case (HashMap.lookup index wallMap, lookup (x, y) path) of
        (Just False, Just (_, _, DRight)) -> '>'
        (Just False, Just (_, _, DDown)) -> 'v'
        (Just False, Just (_, _, DLeft)) -> '<'
        (Just False, Just (_, _, DUp)) -> '^'
        (Just True, Nothing) -> '#'
        (Just False, Nothing) -> '.'
        (Nothing, Nothing) -> ' '

-- Convert a bitmap with optional path to a string
drawMap :: Bitmap -> Range -> [(Index, Position)] -> String
drawMap wallMap ((sx, sy), (ex, ey)) path = concat [
        [
            drawPoint wallMap path (x, y)
            | x <- [sx..ex]
        ] ++ "\n"
        | y <- [sy..ey]
    ]

result :: Position -> MoveMap -> [Action] -> ([(Index, Position)], Int)
result startPosition moveMap actions = (path, score)
    where
        positions = scanl (flip (applyAction moveMap)) startPosition actions
        (x, y, dir) = last positions
        path = map (\(x, y, dir) -> ((x, y), (x, y, dir))) $ reverse positions
        score = 1000 * y + 4 * x + directionValue dir


printResult file sideSize = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Parse input to wall map, range of wall map and actions
        (wallMap, wallMapRange, actions) = parseInput contents

        -- Index and position of path start
        startIndex = findStartIndex wallMap (1, 1)
        startPosition = startPositionFor startIndex
        
        -- Map of all possible moves within basic wallMap grid with wraparound edges
        moveMap = makeMoveMap Nothing wallMap wallMapRange
        
        -- Map of cube sides within wallMap grid
        cubeMap = makeCubeMap sideSize wallMap startIndex
        -- Map of all possible moves within wallMap grid representing a cube
        moveMapCube = makeMoveMap (Just (cubeMap, sideSize)) wallMap wallMapRange


        -- Part #1 - Path withing basic grid with wraparound edges
        (pathP1, resultP1) = result startPosition moveMap actions
        -- Part #2 - Path withing grid representing a cube
        (pathP2, resultP2) = result startPosition moveMapCube actions

    -- Output handling
    -- putStrLn $ drawMap wallMap wallMapRange pathP1
    -- putStrLn $ drawMap wallMap wallMapRange pathP2
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal22-1.test.txt" 4 -- 6032 | 5031
    -- printResult "./data/cal22-1.orig.txt" 50
