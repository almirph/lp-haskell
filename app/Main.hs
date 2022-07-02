module Main where

import System.Random (randomRIO)

main :: IO ()
main = do  
    numList <- randomList(4)
    putStrLn "?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")
    putStrLn(show numList)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO (1,6)
    rs <- randomList (n-1)
    return (r:rs) 
