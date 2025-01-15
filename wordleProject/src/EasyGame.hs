module EasyGame ( startGameEasyMode ) where

import GameState ( GameState (..), emptyGameState, updateGameStateEasyMode )
import System.IO ( hFlush, stdout )
import HelperMode ()
import System.Exit ( exitSuccess )
import ColorUtils
    ( greenColor,
      whiteColor,
      redColor,
      colorTheLetters,
      getColorMatching )
import IOOperations ()
import Color (Color(..))
import ListUtils ( removeLettersFromFirstList, getLettersFromColor )
import Data.List ( nub )

startGameEasyMode :: [[Char]] -> [Char] -> IO b
startGameEasyMode _ [] = do
    putStrLn (redColor ++ "Думата трябва да е с дължина поне 1" ++ whiteColor)
    exitSuccess

startGameEasyMode dict secretWord =
    evaluateGuessEasyMode secretWord (length secretWord) emptyGameState dict

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
            let colorPattern = getColorMatching guess secretWord
            putStrLn (colorTheLetters colorPattern)
            evaluateGuessEasyMode secretWord wordLength (updateGameStateEasyMode guess secretWord gameState) dict

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

confirmWordUsage :: [Char] -> GameState -> [[Char]] -> [Char] -> IO Bool
confirmWordUsage guess gameState dict secretWord = do
    notInDict <- checkGuessIsNotInDictionary guess dict
    if notInDict
        then
            askForConfirmation guess
        else do
            let colorPattern = getColorMatching guess secretWord
            hasNoContradiction <- hasNoContradictionWithFilters gameState colorPattern
            if hasNoContradiction
                then do
                    return True
                else do
                    answer <- askForConfirmation guess
                    if answer
                        then return True
                        else return False

checkForValidGreenFilters :: GameState -> [(Int, Char, Color)] -> IO Bool
checkForValidGreenFilters gameState newFilters =
    let greenLettersNewFilters = getLettersFromColor Green newFilters
        greenLettersGameState = getLettersFromColor Green (greenLetters gameState)
        errorMessage = redColor ++ "На зелените позиции, на които вече са разкрити буквите, има други букви!" ++ whiteColor
    in  if null greenLettersGameState || all (`elem` greenLettersNewFilters) greenLettersGameState
        then return True
        else do
            putStrLn errorMessage
            return False

checkForValidYellowFilters :: [(Int, Char, Color)] -> GameState -> IO Bool
checkForValidYellowFilters newFilters gameState =
    let newGreenLetters = getLettersFromColor Green newFilters
        newYellowLetters = getLettersFromColor Yellow newFilters
        yellowLettersGameState = getLettersFromColor Yellow (yellowLetters gameState)
        actualGameStateYellowLetters = removeLettersFromFirstList yellowLettersGameState newGreenLetters
        errorMessage = redColor ++ "Отсъстват жълти букви, за които се знае, че присъстват в думата!" ++ whiteColor
    in  if null actualGameStateYellowLetters 
            then return True
            else if length newYellowLetters < length actualGameStateYellowLetters
                then do
                    putStrLn errorMessage
                    return False
                else if not (null newYellowLetters) && all (`elem` newYellowLetters) actualGameStateYellowLetters
                        then return True
                        else do
                            putStrLn errorMessage
                            return False

checkForGrayLetters :: [(Int, Char, Color)] -> GameState -> IO Bool
checkForGrayLetters newFilters gameState =
    let newGrayLetters = getLettersFromColor Gray newFilters
        grayLettersFromGameState = nub (getLettersFromColor Gray (grayLetters gameState))
        errorMessage = "В думата има сиви букви, за които вече се знае, че не се срещат в думата на базата на предишни ходове"
    in  if all (`notElem` grayLettersFromGameState) newGrayLetters
            then return True
            else do
                putStrLn ( redColor ++ errorMessage ++ whiteColor )
                return False

hasNoContradictionWithFilters :: GameState -> [(Int, Char, Color)] -> IO Bool
hasNoContradictionWithFilters gameState newFilters = do
    validGreenFilters <- checkForValidGreenFilters gameState newFilters
    if not validGreenFilters
        then return False
        else do
            validYellowFilters <- checkForValidYellowFilters newFilters gameState
            if not validYellowFilters
                then return False
                else do
                    validGrayFilters <- checkForGrayLetters newFilters gameState
                    if validGrayFilters
                        then return True
                        else return False

-- Checks if the guessed word is in the given dictionary
checkGuessIsNotInDictionary :: [Char] -> [[Char]] -> IO Bool
checkGuessIsNotInDictionary guess dictionary =
    if guess `notElem` dictionary
        then do
            putStrLn ( redColor ++ "Думата, която предлагаш, я няма в речника!" ++ whiteColor)
            return True
        else
            return False