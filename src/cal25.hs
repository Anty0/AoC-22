#!/usr/bin/env runhaskell

-- Convert a char to it's numeric representation in SNAFU numeric model
snafuFromChar :: Char -> Int
snafuFromChar '2' = 2
snafuFromChar '1' = 1
snafuFromChar '0' = 0
snafuFromChar '-' = -1
snafuFromChar '=' = -2

-- Convert a number in range 0-4 to it's representation in SNAFU numeric model
-- Returns char representation of number and carry value for numbers which need to be represented by multiple chars
snafuToChar :: Int -> (Char, Int)
snafuToChar 4 = ('-', 1)
snafuToChar 3 = ('=', 1)
snafuToChar 2 = ('2', 0)
snafuToChar 1 = ('1', 0)
snafuToChar 0 = ('0', 0)

-- Convert a string representing a number in SNAFU numeric model to it's numeric representation
snafuFromStr :: String -> Int
snafuFromStr = foldl (\v c -> snafuFromChar c + v * 5) 0

-- Add next digit to SNAFU number, applying carry and returning string with new digit and new carry
snafuCarry :: (String, Int) -> Int -> (String, Int)
snafuCarry (valueStr, carry) num = (nextChar : valueStr, nextCarry + carryAdd)
    where
        -- Num with carry included
        carryNum = num + carry
        -- Did we overflow? If yes: add that overflow to carry
        carryAdd = carryNum `div` 5
        -- Get next SNAFU char and it's carry
        (nextChar, nextCarry) = snafuToChar (carryNum `mod` 5)

-- Convert a number to it's representation in SNAFU numeric model
snafuToStr :: Int -> String
snafuToStr =
    fst . -- 8. Throw away carry (which is 0 at this point)
    head . -- 7. Take a result
    dropWhile (((/=) 0) . snd) . -- 6. Make sure we have applied all carry
    iterate (flip snafuCarry 0) . -- 5. Allow continuing with zeros to apply all carry
    foldl snafuCarry ("", 0) . -- 4. Calculate whole number from all digits
    map (flip mod 5) . -- 3. Calculate modulo of 5 for each division by 5
    takeWhile (flip (>) 0) . -- 2. Stop iterating once we reach 0
    iterate (flip div 5) -- 1. Iterate over all integer divisions by 5


printResult file = do
    -- Reading file
    contents <- readFile file

    -- Main logic
    let -- Parse input to list of SNAFU numbers
        numbersSnafu = lines contents
        -- Convert them all to their numeric representations
        numbers = map snafuFromStr numbersSnafu
        -- Calculate sum of those numbers
        numbersTotal = sum numbers
        -- Convert the sum to it's SNAFU representation
        resultP1 = snafuToStr numbersTotal

    -- Output handling
    putStrLn $ "'" ++ file ++ "': '" ++
        show resultP1 ++ "'"

main = do
    -- Calculate and print result for each test file
    printResult "./data/cal25-1.test.txt" -- 2=-1=0
    -- printResult "./data/cal25-1.orig.txt"
