module ExpertGame where

import System.IO ( hFlush, stdout )
import GameState ( emptyGameState, updateGameState, GameState (greenLetters, yellowLetters, grayLetters), getFiltersCount ) 
import System.Exit ( exitSuccess )
import Color ( Color(Green, Yellow, Gray) )
import ColorUtils ( indexToStartCountingFrom, emptyList, greenColor, whiteColor, redColor, colorTheLetters, changeColorTo )
import ListUtils ( makeMapForLetterAndCountInWord, initialValue )

startGameExpertMode actualWord = evaluateGuessExpertGame actualWord (length actualWord) emptyGameState indexToStartCountingFrom

evaluateGuessExpertGame actualWord wordLength gameState usedLie = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ( redColor ++ "Думата трябва да бъде с дължина " ++ show wordLength ++ whiteColor)
            evaluateGuessExpertGame actualWord wordLength gameState usedLie
            -- TO DO: To clear the screen after a difference in the length
            else if guess == actualWord
                then do
                    putStrLn (greenColor ++ "Браво! Позна думата " ++ actualWord ++ whiteColor)
                    exitSuccess
                else do
                    -- shouldLie <- randomRIO (0, 1 :: Int)
                    putStrLn (colorTheLettersExpertMode guess actualWord gameState usedLie)
                    evaluateGuessExpertGame 
                        actualWord 
                        wordLength 
                        (if usedLie /= 2 then updateGameState guess actualWord gameState else gameState) 
                        (usedLie + 1)


colorTheLettersExpertMode guess actualWord gameState usedLie =
    -- To be careful with the conditions in the if
    if usedLie /= 2
        then colorTheLetters guess actualWord
        else colorTheLettersFalsly 
                guess 
                gameState
                (makeMapForLetterAndCountInWord actualWord) 
                emptyList 
                indexToStartCountingFrom 
                [ind | (ind, _, Green) <- greenLetters gameState]
                (getFiltersCount gameState)
                (length actualWord)

-- namalqm broikata na sreshtaniqta na bukvite v secretDumata
reduceCountOfLetterInMap _ [] = []
reduceCountOfLetterInMap targetLetter ((letter, count):restMap)
    | letter == targetLetter && count > 0   = (letter, count - 1) : restMap
    | otherwise                             = (letter, count) : reduceCountOfLetterInMap targetLetter restMap


-- vzemam broikata na bukvata ot map-a
getCountOfCurrentLetter _ [] = 0
getCountOfCurrentLetter targetLetter ((letter, count):restMapCountForSecretWord)
    | targetLetter == letter    = count
    | otherwise                 = getCountOfCurrentLetter targetLetter restMapCountForSecretWord

isGreen x currentInd gameState =
    not (null [(ind, letter) | (ind, letter, Green) <- greenLetters gameState, ind == currentInd, letter == x])

-- isYellow :: Char -> Int -> GameState -> Bool
-- isYellow x currentInd gameState =
--     let listOfYellowFilters = [ (ind, letter) | (ind, letter, Yellow) <- yellowLetters gameState]
--     in x `elem` map snd listOfYellowFilters && 
--     (currentInd `elem` [ ind | (ind, letter) <- listOfYellowFilters, letter == x] || 
--       x `notElem` ([letter | (ind, letter, Gray) <- grayLetters gameState]))

isGray x gameState = x `elem` [ letter | (_, letter, Gray) <- grayLetters gameState]

-- може да си направя фунцкия determineColor и в нея да са ми проверките, а тук просто да минавам през буквте и да ъпдейтвам мапа
colorTheLettersFalsly [] _ _ coloredWord _ _ _ _ = coloredWord
colorTheLettersFalsly (x:restOfferWord) gameState mapCountForSecretWord coloredWord currentInd indexesWithGreenLetters countFilterLetters lengthWord
  | isGreen x currentInd gameState     = colorTheLettersFalsly 
                                            restOfferWord 
                                            gameState 
                                            (reduceCountOfLetterInMap x mapCountForSecretWord) 
                                            (coloredWord ++ changeColorTo Green x) 
                                            (currentInd + 1) 
                                            indexesWithGreenLetters 
                                            countFilterLetters
                                            lengthWord
  | getCountOfCurrentLetter x mapCountForSecretWord == 0    = processLetterWithCountZero x 
                                                                gameState restOfferWord 
                                                                mapCountForSecretWord coloredWord 
                                                                currentInd indexesWithGreenLetters
                                                                countFilterLetters
                                                                lengthWord
  | otherwise = let yellowFilters = [ (ind, letter) | (ind, letter, Yellow) <- yellowLetters gameState]
                in processOtherLetters x yellowFilters currentInd restOfferWord gameState 
                    mapCountForSecretWord coloredWord indexesWithGreenLetters countFilterLetters lengthWord

processOtherLetters letter yellowFilters currentInd restOfferWord gameState mapCountForSecretWord coloredWord indexesWithGreenLetters countFilterLetters lengthWord
    | letter `elem` map snd yellowFilters   = processYellowLetter yellowFilters letter currentInd 
                                                                  restOfferWord gameState mapCountForSecretWord 
                                                                  coloredWord indexesWithGreenLetters countFilterLetters lengthWord
    | otherwise = processGrayLetter letter mapCountForSecretWord 
                  coloredWord currentInd indexesWithGreenLetters 
                  restOfferWord gameState countFilterLetters lengthWord

processGrayLetter letter mapCountForSecretWord coloredWord currentInd indexesWithGreenLetters restOfferWord gameState countFilterLetters lengthWord
    | letter `elem` [ letter | (ind, letter, Gray) <- grayLetters gameState] = colorTheLettersFalsly 
                                                                                restOfferWord 
                                                                                gameState 
                                                                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                                                                (coloredWord ++ changeColorTo Gray letter) 
                                                                                (currentInd + 1) 
                                                                                indexesWithGreenLetters
                                                                                countFilterLetters
                                                                                lengthWord
    | otherwise = if countFilterLetters < lengthWord - 1
                    then 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Yellow letter) 
                        (currentInd + 1) 
                        indexesWithGreenLetters
                        (countFilterLetters + 1)
                        lengthWord
                    else 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Gray letter) 
                        (currentInd + 1) 
                        indexesWithGreenLetters
                        countFilterLetters
                        lengthWord

processYellowLetter yellowFilters letter currentInd restOfferWord gameState mapCountForSecretWord coloredWord indexesWithGreenLetters countFilterLetters lengthWord
    | currentInd `elem` [ind | (ind, x) <- yellowFilters] = colorTheLettersFalsly 
                                                                restOfferWord 
                                                                gameState 
                                                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                                                (coloredWord ++ changeColorTo Yellow letter) 
                                                                (currentInd + 1) 
                                                                indexesWithGreenLetters
                                                                (countFilterLetters + 1)
                                                                lengthWord
    | otherwise = colorTheLettersFalsly 
                    restOfferWord 
                    gameState 
                    (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                    (coloredWord ++ changeColorTo Green letter) 
                    (currentInd + 1) 
                    indexesWithGreenLetters
                    (countFilterLetters + 1) 
                    lengthWord                         

processLetterWithCountZero letter gameState restOfferWord mapCountForSecretWord coloredWord currentInd indexesWithGreenLetters countFilterLetters lengthWord
    | isGray letter gameState    = colorTheLettersFalsly 
                                        restOfferWord 
                                        gameState 
                                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                        (coloredWord ++ changeColorTo Gray letter) 
                                        (currentInd + 1) 
                                        indexesWithGreenLetters
                                        countFilterLetters
                                        lengthWord

    | otherwise = if countFilterLetters < lengthWord - 1
                    then 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Yellow letter) 
                        (currentInd + 1) 
                        indexesWithGreenLetters
                        (countFilterLetters + 1)
                        lengthWord
                    else 
                        colorTheLettersFalsly 
                        restOfferWord 
                        gameState 
                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                        (coloredWord ++ changeColorTo Gray letter) 
                        (currentInd + 1) 
                        indexesWithGreenLetters
                        countFilterLetters
                        lengthWord
