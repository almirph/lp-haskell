module Main where

import System.Random (randomRIO)

main :: IO ()
main = do  
    keyList <- randomList(4)
    putStrLn "?" 
    guesString <- getLine
    let guesList = map read $ words guesString :: [Int]
    hits <- compareGuesAndRandom guesList keyList [] 0 (0, 0)
    putStrLn(show keyList)
    putStrLn(show guesList)
    print(hits)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO (1,6)
    rs <- randomList (n-1)
    return (r:rs) 

compareGuesAndRandom :: [Int] -> [Int] -> [Int] -> Int -> (Int, Int) -> IO(Int, Int)
compareGuesAndRandom guesList keyList partials 4 hits = do
    return hits
compareGuesAndRandom guesList keyList partials position hits = do
    let newPosition = position + 1
    if guesList!!position == keyList!!position
    then do
        let (complete, _) = hits
        let (_, partial) = hits
        let hitsAux = (complete + 1, partial)
        newHits <- compareGuesAndRandom guesList keyList partials newPosition hitsAux
        return newHits
    else do
        let guesListPosition = guesList!!position
        let existsImPartial = isMember guesListPosition partials
        hadPartial <- comparePartial keyList 0 guesListPosition
        if hadPartial && not existsImPartial
        then do
            let (complete, _) = hits
            let (_, partial) = hits
            let hitsAux = (complete, partial + 1)
            let newPartial = partials ++ [guesListPosition]
            newHits <- compareGuesAndRandom guesList keyList newPartial newPosition hitsAux
            return newHits
        else do
            newHits <- compareGuesAndRandom guesList keyList partials newPosition hits
            return newHits

comparePartial :: [Int] -> Int -> Int -> IO(Bool)
comparePartial keyList 4 compareInt = do
    return False
comparePartial keyList position compareInt = do
    if compareInt == keyList!!position
    then return True
    else do
        let newPosition = position + 1
        newComparePartial <- comparePartial keyList newPosition compareInt
        return newComparePartial

isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs