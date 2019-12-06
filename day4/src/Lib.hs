module Lib
    ( howManyPasswords
    , howManyPasswords2
    )
where

isSixDigits :: Int -> Bool
isSixDigits = (6 ==) . length . show

adjSame :: String -> Bool
adjSame (x : y : xs) | x == y    = True
                     | otherwise = adjSame (y : xs)
adjSame _ = False

neverDecreases :: Int -> String -> Bool
neverDecreases _ [] = True
neverDecreases l (x : xs) | l <= c    = neverDecreases c xs
                          | otherwise = False
    where c = read [x] :: Int

howManyPasswords :: Int -> Int -> Int
howManyPasswords start end
    | start > end
    = 0
    | isSixDigits start && (adjSame . show) start && neverDecreases
        0
        (show start)
    = 1 + howManyPasswords (start + 1) end
    | otherwise
    = howManyPasswords (start + 1) end

skipAll :: (Eq a) => a -> [a] -> [a]
skipAll _ [] = []
skipAll a l@(x : xs) | a == x    = skipAll a xs
                     | otherwise = l

adjExactly2Same :: String -> Bool
adjExactly2Same (x : y : z : xs)
    | x == y && y == z = adjExactly2Same (skipAll x xs)
    | x == y           = True
    | otherwise        = adjExactly2Same (y : z : xs)
adjExactly2Same (x : y : xs) | x == y    = True
                             | otherwise = adjExactly2Same (y : xs)
adjExactly2Same _ = False

howManyPasswords2 :: Int -> Int -> Int
howManyPasswords2 start end
    | start > end
    = 0
    | isSixDigits start && (adjExactly2Same . show) start && neverDecreases
        0
        (show start)
    = 1 + howManyPasswords2 (start + 1) end
    | otherwise
    = howManyPasswords2 (start + 1) end
