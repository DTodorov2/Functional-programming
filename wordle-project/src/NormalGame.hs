module NormalGame ( startGame ) where

import System.IO ( hFlush, stdout )
import ColorUtils ( greenColor, whiteColor, redColor, colorTheLetters )
import System.Exit ( exitSuccess )

-- да направя ли да може да се exit-ва от играта, ако напише exit
startGame :: String -> IO ()
startGame [] = do
    putStrLn (redColor ++ "Думата трябва да има дължина поне 1" ++ whiteColor)
    exitSuccess

-- to pass path as an argument e get the index of a word from a random generator and that is how i pick the word
startGame secretWord = do
    guess <- evaluateGuess secretWord (length secretWord)
    if secretWord == guess
        then do
            putStrLn ( greenColor ++ "Браво! Позна думата " ++ secretWord ++ whiteColor)
            exitSuccess
        else
            startGame secretWord 

evaluateGuess :: String -> Int -> IO String
evaluateGuess [] _ = do
    putStrLn (redColor ++ "Думата трябва да е с дължина поне 1!" ++ whiteColor)
    exitSuccess

evaluateGuess _ 0 = do 
    putStrLn (redColor ++ "Дължината трябва да е поне 1!" ++ whiteColor)
    exitSuccess

evaluateGuess actualWord wordLength = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ( redColor ++ "Думата трябва да бъде с дължина " ++ show wordLength ++ whiteColor)
            evaluateGuess actualWord wordLength
        else do
            return (colorTheLetters guess actualWord)
