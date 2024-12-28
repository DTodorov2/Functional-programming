module IOOperations (loadDictonary, getLengthWords) where
import System.IO
import System.Exit (exitSuccess)
import Data.Char (isDigit)

loadDictonary :: FilePath -> Int -> IO [[Char]]
loadDictonary _ 0 = do
    putStr "Изход от програмата."
    exitSuccess

loadDictonary path lengthWords = do  
    words <- readFile path
    let filteredWords = filter (\word -> length word == lengthWords) (lines words)
    if null filteredWords
        then do
            putStrLn "Думите трябва да са с дължина от 3 до 9."
            newWordsLength <- getLengthWords
            loadDictonary path newWordsLength
        else return filteredWords

getLengthWords :: IO Int
getLengthWords = do
    putStr "Моля, въведете желаната дължина на думите (или 0 за изход): "
    hFlush stdout
    lengthStr <- getLine
    -- to add clearing of the screen
    if all isDigit lengthStr && not (null lengthStr)
        then return (read lengthStr :: Int)
        else do 
            putStrLn "Моля, въведете валидно число!"
            getLengthWords
