module ExpertGame where

import System.IO ( hFlush, stdout )
import GameState 
    ( emptyGameState, 
      updateGameState, 
      GameState (greenLetters, yellowLetters, grayLetters), 
      getFiltersCount ) 
import System.Exit ( exitSuccess )
import Color ( Color(Green, Yellow, Gray) )
import ColorUtils 
    ( greenColor, 
      whiteColor, 
      redColor, 
      colorTheLetters, 
      changeColorTo,
      getColorMatching )
import ListUtils ( makeMapForLetterAndCountInWord, indexToStartCountingFrom, emptyList, getCountOfCurrentLetter )

startGameExpertMode :: String -> IO b
startGameExpertMode secretWord = evaluateGuessExpertGame secretWord (length secretWord) 
                                                emptyGameState indexToStartCountingFrom

evaluateGuessExpertGame :: (Eq a, Num a) => String -> Int -> GameState -> a -> IO b
evaluateGuessExpertGame secretWord wordLength gameState usedLie = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ( redColor ++ "Думата трябва да бъде с дължина " ++ show wordLength ++ whiteColor)
            evaluateGuessExpertGame secretWord wordLength gameState usedLie
            -- TO DO: To clear the screen after a difference in the length
            else if guess == secretWord
                then do
                    putStrLn (greenColor ++ "Браво! Позна думата " ++ secretWord ++ whiteColor)
                    exitSuccess
                else do
                    -- shouldLie <- randomRIO (0, 1 :: Int)
                    putStrLn (colorTheLettersExpertMode guess secretWord gameState usedLie)
                    evaluateGuessExpertGame 
                        secretWord 
                        wordLength 
                        (if usedLie /= 2 then updateGameState guess secretWord gameState else gameState) 
                        (usedLie + 1)

-- Colors the letters depending on the random generator
colorTheLettersExpertMode :: (Eq a, Num a) => [Char] -> [Char] -> GameState -> a -> [Char]
colorTheLettersExpertMode guess secretWord gameState usedLie =
    -- To be careful with the conditions in the if
    if usedLie /= 2
        then colorTheLetters guess secretWord
        else colorTheLettersFalsly 
                guess 
                gameState
                (makeMapForLetterAndCountInWord secretWord) 
                emptyList 
                indexToStartCountingFrom 
                --[ind | (ind, _, Green) <- greenLetters gameState]
                0--(getFiltersCount gameState)
                (length secretWord)
                (getColorMatching guess secretWord)
                False

-- Reduce the count of occurances of the letters in the secret word
reduceCountOfLetterInMap :: (Ord b, Num b, Eq t) => t -> [(t, b)] -> [(t, b)]
reduceCountOfLetterInMap _ [] = []
reduceCountOfLetterInMap targetLetter ((letter, count):restMap)
    | letter == targetLetter && count > 0   = (letter, count - 1) : restMap
    | otherwise                             = (letter, count) : reduceCountOfLetterInMap targetLetter restMap


-- Checks if a letter can be green
isGreen :: Char -> Int -> GameState -> Bool
isGreen x currentInd gameState =
    not (null [(ind, letter) | (ind, letter, Green) <- greenLetters gameState, ind == currentInd, letter == x])

-- isYellow :: Char -> Int -> GameState -> Bool
-- isYellow x currentInd gameState =
--     let listOfYellowFilters = [ (ind, letter) | (ind, letter, Yellow) <- yellowLetters gameState]
--     in x `elem` map snd listOfYellowFilters && 
--     (currentInd `elem` [ ind | (ind, letter) <- listOfYellowFilters, letter == x] || 
--       x `notElem` ([letter | (ind, letter, Gray) <- grayLetters gameState]))

-- Checks if a letter can be gray
isGray :: Char -> GameState -> Bool
isGray x gameState = x `elem` [ letter | (_, letter, Gray) <- grayLetters gameState]

-- може да си направя фунцкия determineColor и в нея да са ми проверките, а тук просто да минавам през буквте и да ъпдейтвам мапа
colorTheLettersFalsly [] _ _ coloredWord _ _ _ _ = coloredWord
colorTheLettersFalsly (x:restOfferWord) gameState mapCountForSecretWord coloredWord currentInd 
                                        countFilterLetters lengthWord colorPattern
  | isGreen x currentInd gameState     = colorTheLettersFalsly 
                                                    restOfferWord 
                                                    gameState 
                                                    (reduceCountOfLetterInMap x mapCountForSecretWord) 
                                                    (coloredWord ++ changeColorTo Green x) 
                                                    (currentInd + 1)  
                                                    (countFilterLetters + 1)
                                                    lengthWord
                                                    colorPattern
  | getCountOfCurrentLetter x mapCountForSecretWord == 0    = processLetterWithCountZero x 
                                                                gameState restOfferWord 
                                                                mapCountForSecretWord coloredWord 
                                                                currentInd
                                                                countFilterLetters
                                                                lengthWord
                                                                colorPattern
  | otherwise = let yellowFilters = [ (ind, letter) | (ind, letter, Yellow) <- yellowLetters gameState]
                in processOtherLetters x yellowFilters currentInd restOfferWord gameState 
                    mapCountForSecretWord coloredWord countFilterLetters lengthWord colorPattern

processOtherLetters letter yellowFilters currentInd restOfferWord gameState mapCountForSecretWord 
                    coloredWord countFilterLetters lengthWord colorPattern
    | letter `elem` map snd yellowFilters   = processYellowLetter yellowFilters letter currentInd 
                                                                  restOfferWord gameState mapCountForSecretWord 
                                                                  coloredWord countFilterLetters lengthWord colorPattern
    | otherwise = processGrayLetter letter mapCountForSecretWord 
                  coloredWord currentInd 
                  restOfferWord gameState countFilterLetters lengthWord colorPattern

processGrayLetter letter mapCountForSecretWord coloredWord currentInd 
                  restOfferWord gameState countFilterLetters lengthWord colorPattern
    | letter `elem` [ letter | (ind, letter, Gray) <- grayLetters gameState] = colorTheLettersFalsly 
                                                                                restOfferWord 
                                                                                gameState 
                                                                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                                                                (coloredWord ++ changeColorTo Gray letter) 
                                                                                (currentInd + 1) 
                                                                                countFilterLetters
                                                                                lengthWord
                                                                                colorPattern
    | otherwise = if countFilterLetters < lengthWord - 1
                    then 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Yellow letter) 
                        (currentInd + 1) 
                        (countFilterLetters + 1)
                        lengthWord
                        colorPattern
                    else 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Gray letter) 
                        (currentInd + 1) 
                        countFilterLetters
                        lengthWord
                        colorPattern

letterIsOriginallyGreen letter currentInd colorPattern = 
    (currentInd, letter, Green) `elem` colorPattern

processYellowLetter yellowFilters letter currentInd restOfferWord gameState mapCountForSecretWord coloredWord 
                    countFilterLetters lengthWord colorPattern
                    --[ind | (ind, x) <- yellowFilters] -> tva beshe vmsto yellowFilters
    |(currentInd, letter) `elem` yellowFilters  = colorTheLettersFalsly 
                                                                restOfferWord 
                                                                gameState 
                                                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                                                (coloredWord ++ changeColorTo Yellow letter) 
                                                                (currentInd + 1) 
                                                                (countFilterLetters + 1)
                                                                lengthWord
                                                                colorPattern
    | otherwise =   if letterIsOriginallyGreen letter currentInd colorPattern
                        then colorTheLettersFalsly 
                                restOfferWord 
                                gameState 
                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                (coloredWord ++ changeColorTo Yellow letter) 
                                (currentInd + 1) 
                                (countFilterLetters + 1) 
                                lengthWord 
                                colorPattern  
                        else 
                            colorTheLettersFalsly 
                                restOfferWord 
                                gameState 
                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                (coloredWord ++ changeColorTo Green letter) 
                                (currentInd + 1) 
                                (countFilterLetters + 1) 
                                lengthWord  
                                colorPattern    

processLetterWithCountZero letter gameState restOfferWord mapCountForSecretWord coloredWord currentInd 
                           countFilterLetters lengthWord colorPattern
    | isGray letter gameState    = colorTheLettersFalsly 
                                        restOfferWord 
                                        gameState 
                                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                        (coloredWord ++ changeColorTo Gray letter) 
                                        (currentInd + 1) 
                                        countFilterLetters
                                        lengthWord
                                        colorPattern

    | otherwise = if countFilterLetters < lengthWord - 1
                    then 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Yellow letter) 
                        (currentInd + 1) 
                        (countFilterLetters + 1)
                        lengthWord
                        colorPattern
                    else 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Gray letter) 
                        (currentInd + 1) 
                        countFilterLetters
                        lengthWord
                        colorPattern
