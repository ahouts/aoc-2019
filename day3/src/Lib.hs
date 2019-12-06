module Lib
    ( getMinManhattanDist
    , getMinWireLengthDist
    , parseInstructions
    )
where

import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( isJust )

data Direction = Right | Down | Left | Up
type Instruction = (Direction, Int)
type Coordinate = (Int, Int)

parseInstruction :: String -> Instruction
parseInstruction s =
    let direction = case head s of
            'R' -> Lib.Right
            'D' -> Lib.Down
            'U' -> Lib.Up
            'L' -> Lib.Left
            _   -> undefined
        steps = read (tail s) :: Int
    in  (direction, steps)

parseInstructions :: String -> [Instruction]
parseInstructions x = map parseInstruction $ splitOn [','] x

invertDir :: Direction -> Direction
invertDir d = case d of
    Lib.Right -> Lib.Left
    Lib.Left  -> Lib.Right
    Lib.Up    -> Lib.Down
    Lib.Down  -> Lib.Up

move :: Int -> Coordinate -> Direction -> Coordinate
move dist (x, y) d = case d of
    Lib.Right -> (x + dist, y)
    Lib.Left  -> (x - dist, y)
    Lib.Up    -> (x, y + dist)
    Lib.Down  -> (x, y - dist)

decrementInstruction :: Instruction -> Maybe Instruction
decrementInstruction (_, 0) = Nothing
decrementInstruction (d, n) = Just (d, n - 1)

nextInstruction :: [Instruction] -> Maybe (Direction, [Instruction])
nextInstruction []       = Nothing
nextInstruction (x : xs) = case decrementInstruction x of
    Just next -> Just (fst x, next : xs)
    Nothing   -> nextInstruction xs

getCoordinates :: Coordinate -> [Instruction] -> [Coordinate]
getCoordinates _       [] = []
getCoordinates currPos xs = case nextInstruction xs of
    Just (d, ins) ->
        let nextPos = move 1 currPos d in nextPos : getCoordinates nextPos ins
    Nothing -> []

manhattanDist :: Coordinate -> Coordinate -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

intersectsWith :: Coordinate -> [Instruction] -> Coordinate -> Bool
intersectsWith _ [] _ = False
intersectsWith start@(sx, sy) ((direction, steps) : xs) pos@(x, y) =
    let headIntersects = case direction of
            Lib.Right -> sy == y && sx < x && x <= sx + steps
            Lib.Left  -> sy == y && sx - steps <= x && x < sx
            Lib.Up    -> sx == x && sy < y && y <= sy + steps
            Lib.Down  -> sx == x && sy - steps <= y && y < sy
    in  headIntersects || intersectsWith (move steps start direction) xs pos

getMinManhattanDist :: [Instruction] -> [Instruction] -> Int
getMinManhattanDist ins1 ins2 =
    let coords        = getCoordinates (0, 0) ins1
        intersections = filter (intersectsWith (0, 0) ins2) coords
        distances     = map (manhattanDist (0, 0)) intersections
    in  minimum distances

getWireLengthDist :: Coordinate -> [Instruction] -> Coordinate -> Int
getWireLengthDist _ [] _ = undefined
getWireLengthDist r@(sx, sy) (i@(d, s) : ls) c@(x, y) =
    if intersectsWith r [i] c
        then abs (sx - x) + abs (sy - y)
        else s + getWireLengthDist (move s r d) ls c

getMinWireLengthDist :: [Instruction] -> [Instruction] -> Int
getMinWireLengthDist ins1 ins2 =
    let
        coords        = getCoordinates (0, 0) ins1
        intersections = filter (intersectsWith (0, 0) ins2) coords
        distances     = map
            (\i ->
                getWireLengthDist (0, 0) ins1 i
                    + getWireLengthDist (0, 0) ins2 i
            )
            intersections
    in
        minimum distances
