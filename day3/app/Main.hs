module Main where

import           Lib

main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    let ins1        = parseInstructions line1
    let ins2        = parseInstructions line2
    let minManDist  = getMinManhattanDist ins1 ins2
    let minWireDist = getMinWireLengthDist ins1 ins2
    putStr "min manhattan distance: "
    print minManDist
    putStr "min wire distance: "
    print minWireDist
