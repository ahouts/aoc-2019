module Main where

import           Lib

main :: IO ()
main = do
    putStr "number of possible passwords part 1: "
    print $ howManyPasswords 130254 678275
    putStr "number of possible passwords part 2: "
    print $ howManyPasswords2 130254 678275
