module EasyGame ( startGameEasyMode ) where

import GameState ( GameState (..), emptyGameState, updateGameState, getAllFilters )
import System.IO ( hFlush, stdout )
import HelperMode ( getValidWords, hasContradictionWithFilters )
import System.Exit ( exitSuccess )
import ColorUtils
    ( greenColor,
      whiteColor,
      redColor,
      colorTheLetters,
      removeGreenLetters,
      removeAllYellowLetters, getColorMatching )
import IOOperations (loadDictionary)
import Color (Color(..))
import Data.List (intersect)
import ListUtils (indexToStartCountingFrom, emptyList)

checkGuessIsNotInDictionary :: Eq a => [a] -> [[a]] -> IO Bool
checkGuessIsNotInDictionary guess dictionary =
    if guess `notElem` dictionary
        then do
            putStrLn ( redColor ++ "Думата, която предлагаш, я няма в речника!" ++ whiteColor)
            return True
        else
            return False

-- TO DO: to add dict to be filled only with words with length equal to the length of actualWord
startGameEasyMode :: FilePath -> [Char] -> IO (IO b)
startGameEasyMode _ [] = do
    putStrLn (redColor ++ "Думата трябва да е с дължина поне 1" ++ whiteColor)
    exitSuccess

startGameEasyMode path secretWord = do
    dict <- loadDictionary path (length secretWord)
    return (evaluateGuessEasyMode secretWord (length secretWord) emptyGameState dict)

askForConfirmation :: [Char] -> IO Bool
askForConfirmation [] = do
    putStrLn (redColor ++ "Думата трябва да е с дължина поне 1" ++ whiteColor)
    exitSuccess

askForConfirmation word = do
    putStrLn ("Искаш ли да продължиш с дума '" ++ word ++ "' - y/n?")
    answer <- getLine
    case answer of
        "y" -> return True
        "n" -> return False
        _   -> do
                putStrLn ( redColor ++ "Отговорът трябва да е y или n" ++ whiteColor )
                askForConfirmation word

confirmWordUsage guess gameState dict secretWord = do
    notInDict <- checkGuessIsNotInDictionary guess dict
    if notInDict
        then
            askForConfirmation guess
        else do
            let colorPattern = getColorMatching guess secretWord
            hasContradiction <- hasContradictionWithFilters gameState colorPattern guess
            if not hasContradiction
                then do
                    askForConfirmation guess
                else do
                    let validWords = getValidWords [guess] (getAllFilters gameState)
                    if null validWords
                        then do
                        putStrLn (redColor ++ "Даваш дума, която противоречи с предишни отговори!" ++ whiteColor)
                        askForConfirmation guess
                    else return True



evaluateGuessEasyMode :: String -> Int -> GameState -> [[Char]] -> IO b
evaluateGuessEasyMode _ _ _ [] = do
    putStrLn "Речникът не трябва да е празен!"
    exitSuccess

evaluateGuessEasyMode secretWord secretWordLength gameState dict = do
    putStr "Моля, въведете опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if guess == secretWord
        then do
            putStrLn ( greenColor ++ "Браво! Позна думата " ++ secretWord ++ whiteColor )
            exitSuccess
        else do
            print (getAllFilters gameState)
            continue <- confirmWordUsage guess gameState dict secretWord
            if not continue
                then evaluateGuessEasyMode secretWord secretWordLength gameState dict
                else validateGuessLength guess secretWordLength secretWord gameState dict

validateGuessLength :: [Char] -> Int -> String -> GameState -> [[Char]] -> IO b
validateGuessLength guess wordLength secretWord gameState dict =
    if length guess /= wordLength
        then do
            putStrLn ( redColor ++ "Думата трябва да бъде с дължина " ++ show wordLength ++ whiteColor)
            evaluateGuessEasyMode secretWord wordLength gameState dict
        else do
            putStrLn (colorTheLetters guess secretWord)
            evaluateGuessEasyMode secretWord wordLength (updateGameState guess secretWord gameState) dict
