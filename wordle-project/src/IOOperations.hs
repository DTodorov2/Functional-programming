module IOOperations (loadDictonary, getLengthWords) where
import System.IO
import System.Exit (exitSuccess)
import Data.Char (isDigit)

loadDictonary :: FilePath -> Int -> IO [[Char]]
loadDictonary _ 0 = do
    putStr "Изход от програмата."
    exitSuccess

loadDictonary path lengthWords = do  
    dictonaryWords <- readFile path
    let filteredWords = filter (\word -> length word == lengthWords) (lines dictonaryWords)
    if null filteredWords
        then do
            putStrLn "\x1b[31mДумите трябва да са с дължина от 3 до 9.\x1b[0m"
            newWordsLength <- getLengthWords
            loadDictonary path newWordsLength
        else
            return filteredWords

getLengthWords :: IO Int
getLengthWords = do
    putStr "Моля, въведете желаната дължина на думите (или 0 за изход): "
    hFlush stdout
    lengthStr <- getLine
    -- to add clearing of the screen
    if all isDigit lengthStr && not (null lengthStr)
        then return (read lengthStr :: Int)
        else do 
            putStrLn "\x1b[31mМоля, въведете валидно число!\x1b[0m"
            getLengthWords
