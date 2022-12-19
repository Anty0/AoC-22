#!/usr/bin/env runhaskell

import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set


-- Index in a 3D grid
type Index = (Int, Int, Int)

-- Bitmap of 3D space
data Bitmap = Bitmap {
    bitmapRange :: (Index, Index),
    bitmapMaterial :: Set Index,
    bitmapOutside :: Set Index
} deriving (Show, Eq)


-- Create empty bitmap
bitmapEmpty :: Bitmap
bitmapEmpty = Bitmap {
    bitmapRange = ((0,0,0), (0,0,0)),
    bitmapMaterial = Set.empty,
    bitmapOutside = Set.empty
}

-- Modify range of bitmap to include index
bitmapExtendRange :: Index -> Bitmap -> Bitmap
bitmapExtendRange (x, y, z) bitmap = bitmap {
    bitmapRange = newRange
}
    where
        ((sx, sy, sz), (ex, ey, ez)) = bitmapRange bitmap
        nsx = min sx x
        nex = max ex x
        nsy = min sy y
        ney = max ey y
        nsz = min sz z
        nez = max ez z
        newRange = ((nsx, nsy, nsz), (nex, ney, nez))

-- Modify range of bitmap to be one point more wide
bitmapExtendRangeByOne :: Bitmap -> Bitmap
bitmapExtendRangeByOne bitmap = bitmap {
    bitmapRange = newRange
}
    where
        ((sx, sy, sz), (ex, ey, ez)) = bitmapRange bitmap
        newRange = ((sx-1, sy-1, sz-1), (ex+1, ey+1, ez+1))

-- Insert new material to bitmap and update range accordingly
bitmapInsertMaterial :: Index -> Bitmap -> Bitmap
bitmapInsertMaterial point bitmap = (bitmapExtendRange point bitmap) {
    bitmapMaterial = Set.insert point $ bitmapMaterial bitmap
}

-- Insert new outside to bitmap and update range accordingly
bitmapInsertOutside :: Index -> Bitmap -> Bitmap
bitmapInsertOutside point bitmap = (bitmapExtendRange point bitmap) {
    bitmapOutside = Set.insert point $ bitmapOutside bitmap
}

-- Check if index is in range of bitmap
bitmapInRange :: Index -> Bitmap -> Bool
bitmapInRange (x,y,z) bitmap =
        x >= sx && y >= sy && z >= sz &&
        x <= ex && y <= ey && z <= ez
    where
        ((sx, sy, sz), (ex, ey, ez)) = bitmapRange bitmap


-- Parse input into list of points
parseInput :: String -> [Index]
parseInput = map ((\[x,y,z] -> (x,y,z)) . map read . splitOn ",") . lines

-- Find all direct neighbors of index in 3D space
indexNeighbors :: Index -> [Index]
indexNeighbors (x,y,z) = [
        (x+1,y,z),
        (x-1,y,z),
        (x,y+1,z),
        (x,y-1,z),
        (x,y,z+1),
        (x,y,z-1)
    ]

-- Count sides of all cubes in 3D space, excluding touching sides
countSides :: [Index] -> (Bitmap, Int)
countSides [] = (bitmapEmpty, 0)
countSides (point:ps) = (
        -- Include current point in bitmap for other recursive calls
        bitmapInsertMaterial point bitmap,
        n + -- All recursively counted sides
        totalSides - -- Plus sides of this cube
        (occupiedSides * 2) -- Minus all occupied sides, twice -
        -- once for current cube and once for the other cube as it's side was counted previously
    )
    where
        -- Recursive call - count sides of all other points before counting this one
        (bitmap, n) = countSides ps
        -- Neighbor indexes of current point
        neighbors = indexNeighbors point
        -- Count of sides of current point - this is always 6
        totalSides = length neighbors
        -- Count of occupied sides of current point - between 0-6
        occupiedSides = length $ filter (flip Set.member $ bitmapMaterial bitmap) neighbors

-- Count all sides accessible from outside
-- Uses flood fill to fill all outside points and counts all neighbor indexes occupied by material
countOutsideSides :: Bitmap -> [Index] -> Int
countOutsideSides _ [] = 0 -- End of flood fill queue
countOutsideSides bitmap (point:ps) = c + countOutsideSides updatedBitmap (ps ++ additionalSides)
    where
        -- If this point is already counted - skip it
        skip = (Set.member point $ bitmapMaterial bitmap) || (Set.member point $ bitmapOutside bitmap)
        -- Neighbor indexes of current point
        neighbors = indexNeighbors point
        -- Split neighbor indexes to occupied and not occupied indexes by material
        (occupiedSides, freeSides) = partition (flip Set.member $ bitmapMaterial bitmap) neighbors
        -- Count occupied indexes - we can see one side for each direct neighbor
        c = if skip then 0 else length occupiedSides
        -- Add unoccupied sides to queue for flood fill
        additionalSides = if skip then [] else filter (flip bitmapInRange bitmap) freeSides
        -- Update bitmap to include point - mark it as checked to avoid checking it multiple times
        updatedBitmap = bitmapInsertOutside point bitmap

printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Parse input to points
        points = parseInput contents

        -- Part #1 - Count sides of all cubes in 3D space
        (bitmap, resultP1) = countSides points

        -- Allow outside to reach boundary
        outsideBitmap = bitmapExtendRangeByOne bitmap
        
        -- Part #2 - Count all sides accessible from outside
        -- We assume point (0,0,0) is always empty and outside
        resultP2 = countOutsideSides outsideBitmap [(0,0,0)]

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "' | '" ++
        show resultP2 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal18-1.test.txt" -- 10 | 10
    printResult "./data/cal18-2.test.txt" -- 64 | 58
    -- printResult "./data/cal18-1.orig.txt"
