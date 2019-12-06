module Lib
    ( calcFuelList
    , calcFuelListRecursive
    )
where

calcFuel :: Int -> Int
calcFuel x =
    let modValue = (x `div` 3) - 2 in if modValue < 0 then 0 else modValue

calcFuelList :: [Int] -> Int
calcFuelList []       = 0
calcFuelList (x : xs) = calcFuel x + calcFuelList xs

calcFuelRecursive :: Int -> Int
calcFuelRecursive x =
    let fuelNeeded = calcFuel x
    in  fuelNeeded + if fuelNeeded > 0 then calcFuelRecursive fuelNeeded else 0

calcFuelListRecursive :: [Int] -> Int
calcFuelListRecursive []       = 0
calcFuelListRecursive (x : xs) = calcFuelRecursive x + calcFuelListRecursive xs

