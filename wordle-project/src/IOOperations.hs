module IOOperations (loadDictionary, getLengthWords) where

import System.IO ( hFlush, stdout )
import System.Exit (exitSuccess)
import Data.Char (isDigit)
import ColorUtils (redColor, whiteColor)

-- to make this with monad Either (for the case where the file cannot be opened)
loadDictionary :: FilePath -> Int -> IO [[Char]]
loadDictonary _ 0 = do
    putStr "Изход от програмата."
    exitSuccess

loadDictionary path lengthWords = do  
    dictonaryWords <- readFile path
    let filteredWords = filter (\word -> length word == lengthWords) (lines dictonaryWords)
    if null filteredWords
        then do
            putStrLn ( redColor ++ "Думите трябва да са с дължина от 3 до 9." ++ whiteColor)
            newWordsLength <- getLengthWords
            loadDictonary path newWordsLength
        else
            return filteredWords

getLengthWords :: IO Int
getLengthWords = do
    putStr "Моля, въведете желаната дължина на думите (или 0 за изход): "
    hFlush stdout
    lengthStr <- getLine
    if all isDigit lengthStr && not (null lengthStr)
        then return (read lengthStr :: Int)
        else do 
            putStrLn ( redColor ++ "Моля, въведете валидно число!" ++ whiteColor)
            getLengthWords
