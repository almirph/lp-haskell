module Main where

import System.Random (randomRIO)

main :: IO ()
main = do  
    keyList <- randomList(4)
    putStrLn "?" 
    guesString <- getLine
    let guesList = map read $ words guesString :: [Int]
    hits <- compareGuesAndRandom guesList keyList 0 (0, 0)
    putStrLn(show keyList)
    putStrLn(show guesList)
    print(hits)
    print(keyList!!0)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO (1,6)
    rs <- randomList (n-1)
    return (r:rs) 

compareGuesAndRandom :: [Int] -> [Int] -> Int -> (Int, Int) -> IO(Int, Int)
compareGuesAndRandom guesList keyList 4 hits = do
    return hits
compareGuesAndRandom guesList keyList position hits = do
    let newPosition = position + 1
    if guesList!!position == keyList!!position
    then do
        print("igual")
        newHits <- compareGuesAndRandom guesList keyList newPosition hits
        return newHits
    else do
        newHits <- compareGuesAndRandom guesList keyList newPosition hits
        return newHits

