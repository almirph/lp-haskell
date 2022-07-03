module Main where

import System.Random (randomRIO)

-- Feito por
-- Almir Philipe de Arruda 201635039
-- Vinícius de Castro Sampaio 201635002

main :: IO ()
main = do  
    keyList <- randomList(4)
    putStrLn("Digite 4 números entre 1 e 6 separados por 'espaço'.") 
    plays <- play keyList 0 (0, 0)
    putStr("Parabéns você acertou após " ++ show plays ++ " tentativas.") 

play :: [Int] -> Int -> (Int, Int) -> IO(Int) 
play keyList plays hits = do
    let (complete, _) = hits
    if complete < 4
    then do
        putStrLn "?"
        guesString <- getLine
        let guesList = map read $ words guesString :: [Int]
        let guesLen = getLength guesList
        if guesLen == 4
        then do
            isValid <- validate guesList 0
            if isValid
            then do
                newHits <- compareGuesAndRandom guesList keyList [] [False, False, False, False] 0 (0, 0)
                let (newComplete, _) = newHits
                let (_, partial) = newHits
                putStrLn("Completo " ++ show newComplete ++ " Parcial " ++ show partial)
                let newPlays = plays + 1
                play keyList newPlays newHits
            else do
                putStrLn("Entrada inválida, os dígitos devem estar entre 1 e 6, divididos por espaço.") 
                play keyList plays hits
        else do
            putStrLn("Entrada inválida, devem ser digitados 4 dígitos separados por espaço.") 
            play keyList plays hits
    else return plays

compareGuesAndRandom :: [Int] -> [Int] -> [Int] -> [Bool] -> Int -> (Int, Int) -> IO(Int, Int)
compareGuesAndRandom guesList keyList partials complets 4 hits = do
    hitsWithPartials <- compareAllPartials guesList keyList partials complets 0 hits
    return hitsWithPartials
compareGuesAndRandom guesList keyList partials complets position hits = do
    let newPosition = position + 1
    if guesList!!position == keyList!!position
    then do
        let (complete, _) = hits
        let (_, partial) = hits
        let hitsAux = (complete + 1, partial)
        let newComplets = replaceNth position True complets
        newHits <- compareGuesAndRandom guesList keyList partials newComplets newPosition hitsAux
        return newHits
    else do
        newHits <- compareGuesAndRandom guesList keyList partials complets newPosition hits
        return newHits

compareAllPartials :: [Int] -> [Int] -> [Int] -> [Bool] -> Int -> (Int, Int) -> IO(Int, Int)
compareAllPartials guesList keyList partials complets 4 hits = do
    return hits
compareAllPartials guesList keyList partials complets position hits = do
    let newPosition = position + 1
    let guesListPosition = guesList!!position
    let existsImPartial = isMember guesListPosition partials
    let completsPosition = complets!!position
    hadPartial <- comparePartial keyList complets 0 guesListPosition
    if hadPartial && not existsImPartial && not completsPosition
    then do
        let (complete, _) = hits
        let (_, partial) = hits
        let hitsAux = (complete, partial + 1)
        let newPartials = partials ++ [guesListPosition]
        newHits <- compareAllPartials guesList keyList newPartials complets newPosition hitsAux
        return newHits
    else do 
        newHits <- compareAllPartials guesList keyList partials complets newPosition hits
        return newHits

comparePartial :: [Int] -> [Bool] -> Int -> Int -> IO(Bool)
comparePartial keyList complets 4 compareInt = do
    return False
comparePartial keyList complets position compareInt = do
    let keyListPosition = keyList!!position
    let completsPosition = complets!!position
    if compareInt == keyListPosition && not completsPosition
    then return True
    else do
        let newPosition = position + 1
        newComparePartial <- comparePartial keyList complets newPosition compareInt
        return newComparePartial

isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO (1,6)
    rs <- randomList (n-1)
    return (r:rs) 

(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max
validate :: [Int] -> Int -> IO(Bool)
validate guesList 4 = do
    return True
validate guesList position = do
    let guesListPosition = guesList!!position
    if guesListPosition <? (1, 6)
    then do
        let newPosition = position + 1
        validateResult <- validate guesList newPosition
        return validateResult
    else do
        return False

getLength :: [a] -> Int
getLength [] = 0
getLength (_:xs) = 1 + getLength xs