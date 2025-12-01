module Main where

import Day1.Solution ( runSolveFirst, runSolveSecond )
import Paths_advent_of_code (getDataFileName)

main :: IO ()
main = do
    input <- getDataFileName "data/day1/input.txt"
    content <- readFile input
    let firstResult = runSolveFirst content
    putStrLn $ "Result: " ++ show firstResult
    let secondResult = runSolveSecond content
    putStrLn $ "Result second part: " ++ show secondResult     
