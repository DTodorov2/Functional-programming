module IOOperations (loadDictionary, getIntFromUser, validateIntInputFromUser) where

import System.IO ( hFlush, stdout )
import System.Exit (exitSuccess)
import Data.Char (isDigit)
import ColorUtils (redColor, whiteColor)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Exception (try, IOException)

-- to make this with monad Either (for the case where the file cannot be opened)
loadDictionary :: FilePath -> Int -> IO [[Char]]
loadDictionary _ 0 = do
    putStr "Изход от програмата."
    exitSuccess

loadDictionary path lengthWords = do  
    result <- try (readFile path) :: IO (Either IOException String)
    case result of 
        Left err -> do
            putStrLn ("Error: " ++ show err)
            exitSuccess
        Right dictionaryWords -> do
            putStrLn "File rendered successfully!"
            let filteredWords = filter (\word -> length word == lengthWords) (lines dictionaryWords)
            if null filteredWords
                then do
                    putStrLn ( redColor ++ "Думите трябва да са с дължина от 3 до 9." ++ whiteColor)
                    newWordsLength <- getIntFromUser "Моля, въведете желаната дължина на думите (или 0 за изход): "
                    loadDictionary path newWordsLength
                else return filteredWords

getIntFromUser :: String -> IO Int
getIntFromUser message = do
    putStr message
    hFlush stdout
    lengthStr <- getLine
    if all isDigit lengthStr && not (null lengthStr)
        then return (read lengthStr :: Int)
        else do 
            putStrLn ( redColor ++ "Моля, въведете валидно число!" ++ whiteColor)
            getIntFromUser message

validateIntInputFromUser :: String -> Int -> Int -> IO Int
validateIntInputFromUser message lowerLimit upperLimit = do
    putStr message
    hFlush stdout
    numberStr <- getLine
    -- da kaja, che sum go vzel ot chata
    let number = fromMaybe (-1) (readMaybe numberStr)
    if number < lowerLimit || number > upperLimit
        then do
            putStrLn ( redColor ++ "Числото трябва да бъде между " ++ show lowerLimit ++ " и " ++ show upperLimit ++ whiteColor)
            validateIntInputFromUser message lowerLimit upperLimit
        else return number
