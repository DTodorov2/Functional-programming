module NormalGame ( startNormalGame ) where

import ColorUtils ( greenColor, whiteColor, redColor, colorTheLetters, getColorMatching )
import System.Exit ( exitSuccess )
import System.IO ( hFlush, stdout )


startNormalGame :: [Char] -> IO b
startNormalGame [] = do
    putStrLn (redColor ++ "Думата трябва да има дължина поне 1" ++ whiteColor)
    exitSuccess

-- to pass path as an argument e get the index of a word from a random generator and that is how i pick the word
startNormalGame secretWord = evaluateGuess secretWord (length secretWord)
    

evaluateGuess :: String -> Int -> IO b
evaluateGuess [] _ = do
    putStrLn (redColor ++ "Думата трябва да е с дължина поне 1!" ++ whiteColor)
    exitSuccess

evaluateGuess _ 0 = do 
    putStrLn (redColor ++ "Дължината трябва да е поне 1!" ++ whiteColor)
    exitSuccess

evaluateGuess secretWord wordLength = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ( redColor ++ "Думата трябва да бъде с дължина " ++ show wordLength ++ whiteColor)
            evaluateGuess secretWord wordLength
        else if secretWord == guess
                then do
                    putStrLn ( greenColor ++ "Браво! Позна думата " ++ secretWord ++ whiteColor)
                    exitSuccess
                else do
                    let colorPattern = (getColorMatching guess secretWord)
                    (putStrLn (colorTheLetters colorPattern))
                    evaluateGuess secretWord wordLength
