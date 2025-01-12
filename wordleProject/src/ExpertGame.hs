module ExpertGame ( startGameExpertMode ) where

import System.IO ( hFlush, stdout )
import System.Exit ( exitSuccess )
import Color ( Color(Green, Yellow, Gray) )
import ListUtils ( getLettersFromColor, 
                   makeMapForLetterAndCountInWord, 
                   indexToStartCountingFrom, 
                   emptyList, 
                   getCountOfCurrentLetter, 
                   initialValue )
import GameState
import ColorUtils
import System.Random

startGameExpertMode :: String -> IO b
startGameExpertMode secretWord = evaluateGuessExpertGame secretWord (length secretWord) emptyGameState False

evaluateGuessExpertGame :: String -> Int -> GameState -> Bool -> IO b
evaluateGuessExpertGame secretWord wordLength gameState usedLie = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ( redColor ++ "Думата трябва да бъде с дължина " ++ show wordLength ++ whiteColor)
            evaluateGuessExpertGame secretWord wordLength gameState usedLie
            else if guess == secretWord
                then do
                    putStrLn (greenColor ++ "Браво! Позна думата " ++ secretWord ++ whiteColor)
                    exitSuccess
                else do
                    randomNumber <- randomRIO (1, 3) :: IO Int
                    let shouldLie = (not usedLie) && (randomNumber == 3)
                    putStrLn ("trqq li da luja: " ++ show shouldLie)
                    putStrLn (colorTheLettersExpertMode guess secretWord gameState shouldLie)
                    evaluateGuessExpertGame 
                        secretWord 
                        wordLength 
                        (if not shouldLie then updateGameState guess secretWord gameState else gameState)
                        (if usedLie then usedLie else shouldLie)

-- Colors the letters depending on the random generator
colorTheLettersExpertMode :: [Char] -> [Char] -> GameState -> Bool -> [Char]
colorTheLettersExpertMode guess secretWord gameState shouldLie =
    if not shouldLie
        then colorTheLetters (getColorMatching guess secretWord)
        else colorTheLettersFalsly 
                guess 
                gameState
                (makeMapForLetterAndCountInWord secretWord) 
                emptyList 
                indexToStartCountingFrom 
                initialValue
                (length secretWord)
                (getColorMatching guess secretWord)
                False

-- Reduces the count of occurances of the letters in the secret word
reduceCountOfLetterInMap :: Char -> [(Char, Int)] -> [(Char, Int)]
reduceCountOfLetterInMap _ [] = []
reduceCountOfLetterInMap targetLetter ((letter, count):restMap)
    | letter == targetLetter && count > 0   = (letter, count - 1) : restMap
    | otherwise                             = (letter, count) : reduceCountOfLetterInMap targetLetter restMap


-- Checks if a letter can be green
isGreen :: Char -> Int -> GameState -> Bool
isGreen x currentInd gameState =
    not (null [(ind, letter) | (ind, letter, Green) <- greenLetters gameState, ind == currentInd, letter == x])

-- Checks if a letter can be gray
isGray :: Char -> GameState -> Bool
isGray x gameState = x `elem` [ letter | (_, letter, Gray) <- grayLetters gameState]

-- Checks if the lie has been used
hasLied :: [Char] -> [(Int, Char, Color)] -> Int -> Bool
hasLied coloredLetter colorPattern index =
    let currentColor = colorOfTheLetter coloredLetter
        actualColor = head [ color | (ind, _, color) <- colorPattern, ind == index]
    in currentColor /= actualColor

colorTheLettersFalsly :: [Char] -> GameState -> [(Char, Int)] -> [Char] -> Int -> Int -> Int -> [(Int, Char, Color)] -> Bool -> [Char]
colorTheLettersFalsly [] _ _ coloredWord _ _ _ _ _ = coloredWord
colorTheLettersFalsly (x:restOfferWord) gameState mapCountForSecretWord coloredWord 
                       currentInd countFilterLetters lengthWord colorPattern lied =

    let coloredLetter = colorLetterFalsly x gameState mapCountForSecretWord 
                       currentInd countFilterLetters lengthWord colorPattern
        didLie = hasLied coloredLetter colorPattern currentInd
        incrementCountFilters = if colorOfTheLetter coloredLetter `elem` [Green, Yellow] 
                                    then countFilterLetters + 1 
                                    else countFilterLetters
        shouldLie = if didLie 
                        then didLie 
                        else lied
        neededColorPatterns = [ (ind, letter, color) | (ind, letter, color) <- colorPattern, ind >= currentInd]
   in
    if not lied
        then colorTheLettersFalsly restOfferWord 
                                   gameState 
                                   (reduceCountOfLetterInMap x mapCountForSecretWord) 
                                   (coloredWord ++ coloredLetter) 
                                   (currentInd + 1)
                                   incrementCountFilters 
                                   lengthWord 
                                   colorPattern 
                                   shouldLie
                                
        else coloredWord ++ colorTheLetters neededColorPatterns

-- Decided which color to paint the letter
colorLetterFalsly :: Char -> GameState -> [(Char, Int)] -> Int -> Int -> Int -> [(Int, Char, Color)] -> [Char]
colorLetterFalsly letter gameState mapCountForSecretWord currentInd countFilterLetters lengthWord colorPattern
  | isGreen letter currentInd gameState                         = changeColorTo Green letter
  | getCountOfCurrentLetter letter mapCountForSecretWord == 0   = processLetterWithCountZero letter 
                                                                   gameState countFilterLetters lengthWord
  | otherwise = let yellowFilters = [ (ind, ch) | (ind, ch, Yellow) <- yellowLetters gameState]
                in processOtherLetters letter yellowFilters currentInd gameState countFilterLetters 
                    lengthWord colorPattern

-- Processes letters that are in the secret word somewhere
processOtherLetters :: Char -> [(Int, Char)] -> Int -> GameState -> Int -> Int -> [(Int, Char, Color)] -> [Char]
processOtherLetters letter yellowFilters currentInd gameState countFilterLetters lengthWord colorPattern
    | letter `elem` map snd yellowFilters = processYellowLetter yellowFilters letter currentInd colorPattern
    | otherwise = processGrayLetter letter gameState countFilterLetters lengthWord 

processGrayLetter :: Char -> GameState -> Int -> Int -> [Char]
processGrayLetter letter gameState countFilterLetters lengthWord 
    | letter `elem` (getLettersFromColor Gray (grayLetters gameState)) = changeColorTo Gray letter
    | otherwise = if countFilterLetters < lengthWord - 1
                    then changeColorTo Yellow letter
                    else changeColorTo Gray letter

-- Checks if a letter is green in the secret word
isOriginallyGreen :: Char -> Int -> [(Int, Char, Color)] -> Bool
isOriginallyGreen letter currentInd colorPattern = 
    (currentInd, letter, Green) `elem` colorPattern

processYellowLetter :: [(Int, Char)] -> Char -> Int -> [(Int, Char, Color)] -> [Char]
processYellowLetter yellowFilters letter currentInd colorPattern
    | (currentInd, letter) `elem` yellowFilters = changeColorTo Yellow letter
    | otherwise = if isOriginallyGreen letter currentInd colorPattern
                    then changeColorTo Yellow letter
                    else changeColorTo Green letter

processLetterWithCountZero :: Char -> GameState -> Int -> Int -> [Char]
processLetterWithCountZero letter gameState countFilterLetters lengthWord
    | isGray letter gameState = changeColorTo Gray letter
    | otherwise = if countFilterLetters < lengthWord - 1
                    then changeColorTo Green letter
                    else changeColorTo Gray letter