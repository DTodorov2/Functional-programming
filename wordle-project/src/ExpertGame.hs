module ExpertGame where

import NormalGame
    ( colorTheLetters, changeColor )
import System.IO ( hFlush, stdout )
import EasyGame
    ( GameState(usedGrayLetters, usedGreenLetters, usedYellowLetters),
      emptyGameState,
      updateGameState )
import Data.List (group, sort)
import System.Exit (exitSuccess)

startGameExpertMode actualWord = evaluateGuessExpertGame actualWord (length actualWord) emptyGameState 1

evaluateGuessExpertGame actualWord wordLength gameState usedLie = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ("\x1b[31mДумата трябва да бъде с дължина " ++ show wordLength ++ "\x1b[0m")
            evaluateGuessExpertGame actualWord wordLength gameState usedLie
            -- TO DO: To clear the screen after a difference in the length
            else if guess == actualWord
                then do
                    putStrLn ("\x1b[32mБраво! Позна думата " ++ actualWord ++ "!\x1b[0m")
                    exitSuccess
                else do
                    -- shouldLie <- randomRIO (0, 1 :: Int)
                    putStrLn (colorTheLettersExpertMode guess actualWord gameState usedLie)
                    evaluateGuessExpertGame actualWord wordLength (if usedLie /= 2 then updateGameState guess actualWord gameState else gameState) (usedLie + 1)


colorTheLettersExpertMode guess actualWord gameState usedLie =
    -- To be careful with the conditions in the if
    if usedLie /= 2
        then colorTheLetters guess actualWord
        else colorTheLettersFalsly guess gameState (makeMapForLetterAndCountInWord actualWord) [] 1 [ind | (ind, _, "gr") <- usedGreenLetters gameState]

-- pravq list ot vida [(letter, count)] za tainata duma
makeMapForLetterAndCountInWord secretWord =
    let groupedLetters = group (sort secretWord)
    in map (\group -> (head group, length group)) groupedLetters

-- filters e ot vida [(ind, letter, color)]
-- tazi funkciq vrushta offer dumata bez zelenite bukvi na poziciite, koito sa otkriti v dosegashnite filtri
removeAllGreenLettersFromOfferWord [] _ newWord _ = newWord
removeAllGreenLettersFromOfferWord (x:restOfferWord) gameState newWord ind =
    let listOfGreenLetters = [(index, letter) | (index, letter, "gr") <- usedGreenLetters gameState]
    in if x `elem` map snd listOfGreenLetters
        then
            case filter (\(index, letter) -> letter == x && index == ind) listOfGreenLetters of
                [] -> removeAllGreenLettersFromOfferWord restOfferWord gameState (newWord ++ [x]) (ind + 1)
                _  -> removeAllGreenLettersFromOfferWord restOfferWord gameState newWord (ind + 1)
        else removeAllGreenLettersFromOfferWord restOfferWord gameState (newWord ++ [x]) (ind + 1)

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

colorTheLettersFalsly [] _ _ coloredWord _ _ = coloredWord
colorTheLettersFalsly (x:restOfferWord) gameState mapCountForSecretWord coloredWord currentInd listOfIndexesWithGreenLetters
  | not (null [(currentInd, x, "gr") | (ind, letter, "gr") <- usedGreenLetters gameState, ind == currentInd, letter == x])      = colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColor "gr" x) (currentInd + 1) listOfIndexesWithGreenLetters
  | getCountOfCurrentLetter x mapCountForSecretWord == 0    = if x `elem` [ letter | (ind, letter, "g") <- usedGrayLetters gameState]
                then colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColor "g" x) (currentInd + 1) listOfIndexesWithGreenLetters
                else colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColor "y" x) (currentInd + 1) listOfIndexesWithGreenLetters
  | otherwise = let listOfYellowFilters = [ (ind, letter) | (ind, letter, "y") <- usedYellowLetters gameState]
                in if x `elem` map snd listOfYellowFilters
                    then
                        if currentInd `elem` [ind | (ind, x) <- listOfYellowFilters]
                            then colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColor "y" x) (currentInd + 1) listOfIndexesWithGreenLetters
                            else colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColor "gr" x) (currentInd + 1) listOfIndexesWithGreenLetters
                    else
                        if x `elem` [ letter | (ind, letter, "g") <- usedGrayLetters gameState]
                            then colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColor "g" x) (currentInd + 1) listOfIndexesWithGreenLetters
                            else colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColor "y" x) (currentInd + 1) listOfIndexesWithGreenLetters
