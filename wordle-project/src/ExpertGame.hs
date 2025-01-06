module ExpertGame where

import System.IO ( hFlush, stdout )
import GameState ( emptyGameState, updateGameState, GameState (greenLetters, yellowLetters, grayLetters) ) 

import System.Exit ( exitSuccess )
import Color (Color(Green, Yellow, Gray))
import ColorUtils ( indexToStartCountingFrom, emptyList, greenColor, whiteColor, redColor, colorTheLetters, changeColorTo )
import ListUtils (makeMapForLetterAndCountInWord)

startGameExpertMode :: String -> IO b
startGameExpertMode actualWord = evaluateGuessExpertGame actualWord (length actualWord) emptyGameState indexToStartCountingFrom

evaluateGuessExpertGame :: (Eq a, Num a) => String -> Int -> GameState -> a -> IO b
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


colorTheLettersExpertMode :: (Eq a, Num a) => [Char] -> [Char] -> GameState -> a -> [Char]
colorTheLettersExpertMode guess actualWord gameState usedLie =
    -- To be careful with the conditions in the if
    if usedLie /= 2
        then colorTheLetters guess actualWord
        else colorTheLettersFalsly guess gameState 
                (makeMapForLetterAndCountInWord actualWord) 
                emptyList 
                indexToStartCountingFrom 
                [ind | (ind, _, Green) <- greenLetters gameState]

-- pravq list ot vida [(letter, count)] za tainata duma


-- filters e ot vida [(ind, letter, color)]
-- tazi funkciq vrushta offer dumata bez zelenite bukvi na poziciite, koito sa otkriti v dosegashnite filtri
-- removeAllGreenLettersFromOfferWord :: [Char] -> GameState -> [Char] -> Int -> [Char]
-- removeAllGreenLettersFromOfferWord [] _ newWord _ = newWord
-- removeAllGreenLettersFromOfferWord (x:restOfferWord) gameState newWord ind =
--     let listOfGreenLetters = [(index, letter) | (index, letter, Green) <- greenLetters gameState]
--     in if x `elem` map snd listOfGreenLetters
--         then
--             case filter (\(index, letter) -> letter == x && index == ind) listOfGreenLetters of
--                 [] -> removeAllGreenLettersFromOfferWord restOfferWord gameState (newWord ++ [x]) (ind + 1)
--                 _  -> removeAllGreenLettersFromOfferWord restOfferWord gameState newWord (ind + 1)
--         else removeAllGreenLettersFromOfferWord restOfferWord gameState (newWord ++ [x]) (ind + 1)

-- namalqm broikata na sreshtaniqta na bukvite v secretDumata
reduceCountOfLetterInMap :: (Ord b, Num b, Eq t) => t -> [(t, b)] -> [(t, b)]
reduceCountOfLetterInMap _ [] = []
reduceCountOfLetterInMap targetLetter ((letter, count):restMap)
    | letter == targetLetter && count > 0   = (letter, count - 1) : restMap
    | otherwise                             = (letter, count) : reduceCountOfLetterInMap targetLetter restMap


-- vzemam broikata na bukvata ot map-a
getCountOfCurrentLetter :: (Num a, Eq t) => t -> [(t, a)] -> a
getCountOfCurrentLetter _ [] = 0
getCountOfCurrentLetter targetLetter ((letter, count):restMapCountForSecretWord)
    | targetLetter == letter    = count
    | otherwise                 = getCountOfCurrentLetter targetLetter restMapCountForSecretWord

isGreen :: Char -> Int -> GameState -> Bool
isGreen x currentInd gameState =
    not (null [(ind, letter) | (ind, letter, Green) <- greenLetters gameState, ind == currentInd, letter == x])

-- isYellow :: Char -> Int -> GameState -> Bool
-- isYellow x currentInd gameState =
--     let listOfYellowFilters = [ (ind, letter) | (ind, letter, Yellow) <- yellowLetters gameState]
--     in x `elem` map snd listOfYellowFilters && 
--     (currentInd `elem` [ ind | (ind, letter) <- listOfYellowFilters, letter == x] || 
--       x `notElem` ([letter | (ind, letter, Gray) <- grayLetters gameState]))

isGray :: Char -> GameState -> Bool
isGray x gameState = x `elem` [ letter | (_, letter, Gray) <- grayLetters gameState]

-- може да си направя фунцкия determineColor и в нея да са ми проверките, а тук просто да минавам през буквте и да ъпдейтвам мапа
colorTheLettersFalsly :: (Ord a, Num a) => [Char] -> GameState -> [(Char, a)] -> [Char] -> Int -> t -> [Char]
colorTheLettersFalsly [] _ _ coloredWord _ _ = coloredWord
colorTheLettersFalsly (x:restOfferWord) gameState mapCountForSecretWord coloredWord currentInd indexesWithGreenLetters
  | isGreen x currentInd gameState     = colorTheLettersFalsly 
                                            restOfferWord 
                                            gameState 
                                            (reduceCountOfLetterInMap x mapCountForSecretWord) 
                                            (coloredWord ++ changeColorTo Green x) 
                                            (currentInd + 1) 
                                            indexesWithGreenLetters
  | getCountOfCurrentLetter x mapCountForSecretWord == 0    = processLetterWithCountZero x 
                                                                gameState restOfferWord 
                                                                mapCountForSecretWord coloredWord 
                                                                currentInd indexesWithGreenLetters
            --if isGray x gameState
--                 then colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColorTo Gray x) (currentInd + 1) listOfIndexesWithGreenLetters
--                 else colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColorTo Yellow x) (currentInd + 1) listOfIndexesWithGreenLetters
  | otherwise = let yellowFilters = [ (ind, letter) | (ind, letter, Yellow) <- yellowLetters gameState]
                in processOtherLetters x yellowFilters currentInd restOfferWord gameState 
                    mapCountForSecretWord coloredWord indexesWithGreenLetters
                    -- if x `elem` map snd yellowFilters
                    -- then
                    --     if currentInd `elem` [ind | (ind, x) <- yellowFilters]
                    --         then colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColorTo Yellow x) (currentInd + 1) listOfIndexesWithGreenLetters
                    --         else colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColorTo Green x) (currentInd + 1) listOfIndexesWithGreenLetters
                    -- else
                    --     if x `elem` [ letter | (ind, letter, Gray) <- grayLetters gameState]
                    --         then colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColorTo Gray x) (currentInd + 1) listOfIndexesWithGreenLetters
                    --         else colorTheLettersFalsly restOfferWord gameState (reduceCountOfLetterInMap x mapCountForSecretWord) (coloredWord ++ changeColorTo Yellow x) (currentInd + 1) listOfIndexesWithGreenLetters

processOtherLetters letter yellowFilters currentInd restOfferWord gameState mapCountForSecretWord coloredWord indexesWithGreenLetters
    | letter `elem` map snd yellowFilters   = processYellowLetter yellowFilters letter currentInd 
                                                                  restOfferWord gameState mapCountForSecretWord 
                                                                  coloredWord indexesWithGreenLetters
    | otherwise = processGrayLetter letter mapCountForSecretWord 
                  coloredWord currentInd indexesWithGreenLetters 
                  restOfferWord gameState

processGrayLetter :: (Ord a, Num a) => Char -> [(Char, a)] -> [Char] -> Int -> t -> [Char] -> GameState -> [Char]
processGrayLetter letter mapCountForSecretWord coloredWord currentInd indexesWithGreenLetters restOfferWord gameState
    | letter `elem` [ letter | (ind, letter, Gray) <- grayLetters gameState] = colorTheLettersFalsly 
                                                                                restOfferWord 
                                                                                gameState 
                                                                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                                                                (coloredWord ++ changeColorTo Gray letter) 
                                                                                (currentInd + 1) 
                                                                                indexesWithGreenLetters
    | otherwise = colorTheLettersFalsly 
                    restOfferWord 
                    gameState 
                    (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                    (coloredWord ++ changeColorTo Yellow letter) 
                    (currentInd + 1) 
                    indexesWithGreenLetters

processYellowLetter :: (Ord a, Num a) => [(Int, Char)] -> Char -> Int -> [Char] -> GameState -> [(Char, a)] -> [Char] -> t -> [Char]
processYellowLetter yellowFilters letter currentInd restOfferWord gameState mapCountForSecretWord coloredWord indexesWithGreenLetters
    | currentInd `elem` [ind | (ind, x) <- yellowFilters] = colorTheLettersFalsly 
                                                                restOfferWord 
                                                                gameState 
                                                                (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                                                (coloredWord ++ changeColorTo Yellow letter) 
                                                                (currentInd + 1) 
                                                                indexesWithGreenLetters
    | otherwise = colorTheLettersFalsly 
                    restOfferWord 
                    gameState 
                    (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                    (coloredWord ++ changeColorTo Green letter) 
                    (currentInd + 1) 
                    indexesWithGreenLetters                          

processLetterWithCountZero :: (Ord a, Num a) => Char -> GameState -> [Char] -> [(Char, a)] -> [Char] -> Int -> t -> [Char]
processLetterWithCountZero letter gameState restOfferWord mapCountForSecretWord coloredWord currentInd indexesWithGreenLetters 
    | isGray letter gameState    = colorTheLettersFalsly 
                                        restOfferWord 
                                        gameState 
                                        (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                                        (coloredWord ++ changeColorTo Gray letter) 
                                        (currentInd + 1) 
                                        indexesWithGreenLetters

    | otherwise = colorTheLettersFalsly 
                    restOfferWord 
                    gameState 
                    (reduceCountOfLetterInMap letter mapCountForSecretWord) 
                    (coloredWord ++ changeColorTo Yellow letter) 
                    (currentInd + 1) 
                    indexesWithGreenLetters
