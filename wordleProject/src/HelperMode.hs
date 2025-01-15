module HelperMode ( getListOfColors, getFilteredDict, getValidWords, startGameHelperMode, getBestWord ) where

import IOOperations ( getIntFromUser, loadDictionary )
import Data.Foldable ( maximumBy )
import Data.Ord ( comparing )
import System.IO ( hFlush, stdout )
import System.Exit ( exitSuccess )
import Color ( Color(..), parseStringToColor )
import ColorUtils ( getColorMatching, removeGreenLetters, redColor, whiteColor, greenColor, removeAllYellowLetters)
import ListUtils ( isValidIndex, indexToStartCountingFrom, emptyList, getLettersFromColor)
import GameState()


startGameHelperMode :: FilePath -> IO ()
startGameHelperMode path = do
    wordLength <- getIntFromUser "Моля, въведете желаната дължина на думите (или 0 за изход): "
    dict <- loadDictionary path wordLength
    evaluateGuessHelper dict wordLength

evaluateGuessHelper :: [[Char]] -> Int -> IO b
evaluateGuessHelper [] _ = do
    putStrLn ( redColor ++ "Някой от предишните ти отговори е бил лъжа или думата не е в речника!" ++ whiteColor)
    exitSuccess

evaluateGuessHelper dict wordLength = do
    bestWord <- getBestWord dict
    putStrLn ("Предложената дума е: " ++ bestWord)
    listOfColors <- getListOfColors bestWord emptyList indexToStartCountingFrom (length bestWord)
    let listOfGreenLetters = [ letter | (_, letter, Green) <- listOfColors]
        newDict = filter (/= bestWord) dict
    if length listOfGreenLetters == wordLength
        then do
            putStrLn ( greenColor ++ "Браво, позна думата: " ++ bestWord ++ whiteColor)
            exitSuccess
        else
             evaluateGuessHelper (getFilteredDict newDict listOfColors) wordLength

-- Gets the input from the user for the colors of the letters
getListOfColors :: [Char] -> [(Int, Char, Color)] -> Int -> Int -> IO [(Int, Char, Color)]
getListOfColors [] listOfColors _ _ = return listOfColors
getListOfColors (x:xs) listOfColors ind wordLength
  | length (x:xs) /= wordLength   = do
    putStr ( redColor ++ "Думата не е с коректна дължина! Опитай пак: " ++ whiteColor)
    getListOfColors (x:xs) listOfColors ind wordLength
  | otherwise = do
                putStrLn "Напиши комбинацията от букви, която отговаря на съответния цвят: (gr - green, y - yellow, g - gray):"
                putStr ("Цвят за буква " ++ [x] ++ ": ")
                hFlush stdout
                colorStr <- getLine
                case parseStringToColor colorStr of
                    Just c -> getListOfColors xs ((ind, x, c) : listOfColors) (ind + 1) (wordLength - 1)
                    Nothing -> do
                        putStrLn (redColor ++ "Невалиден цвят!" ++ whiteColor)
                        getListOfColors (x:xs) listOfColors ind wordLength

-- Gets words which contain green letters at the needed positions
getWordsMatchingGreenLetters  :: [[Char]] -> [(Int, Char, Color)] -> [[Char]]
getWordsMatchingGreenLetters dict colorMatchingList =
    let greenLettersFromList = [(ind, letter) | (ind, letter, Green) <- colorMatchingList]
    in filter (matchesGreenLetters greenLettersFromList) dict
    where matchesGreenLetters conditionList word =
            all (\(ind, ch) -> isValidIndex ind word && (word !! (ind - 1) == ch)) conditionList

first :: (a, b, c) -> a
first (f, _, _) = f

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, c) = c

-- Checks if a word contains all yellow letters (not at the positions of the green ones)
matchesYellowLettersFilters :: [(Int, Char)] -> (Int, [Char], [Char]) -> Bool
matchesYellowLettersFilters yellowLettersFromList pair =
    let wordWithoutGreen = second pair
        wholeWord = third pair
    in all (\(ind, ch) -> isValidIndex ind wholeWord && (((wholeWord !! (ind - 1)) /= ch) && ch `elem` wordWithoutGreen)) yellowLettersFromList

getWordsWithYellowAndGreenLetters :: [[Char]] -> [(Int, Char, Color)] -> [(Int, [Char])]
getWordsWithYellowAndGreenLetters initialDict colorMatchingList =
    let wordsWithGreenLetters = zip [1..] (getWordsMatchingGreenLetters initialDict colorMatchingList)
        yellowLettersFromList = [ (ind, letter) | (ind, letter, Yellow) <- colorMatchingList]
        indexesOfGreenLetters = [ind | (ind, _, Green) <- colorMatchingList]
        wordsWithoutGreenLetters = 
            map (\(ind, word) -> 
            (ind, removeGreenLetters indexesOfGreenLetters word indexToStartCountingFrom emptyList, word)) 
            wordsWithGreenLetters
        wordsMatchingYellowLettersFilters = filter (matchesYellowLettersFilters yellowLettersFromList) wordsWithoutGreenLetters
        indexesOfWordsWithYellowLetters = map first wordsMatchingYellowLettersFilters
    in filter (\(ind, _) -> ind `elem` indexesOfWordsWithYellowLetters) wordsWithGreenLetters

doesntMatchGrayLetters :: [Char] -> (Int, [Char]) -> Bool
doesntMatchGrayLetters grayLettersFromList pair = all (\ch -> ch `notElem` snd pair) grayLettersFromList

-- Gets the words which match all filters
getValidWords :: [[Char]] -> [(Int, Char, Color)] -> [(Int, [Char])]
getValidWords initialDict colorMatchingList =
    let wordsWithYellowAndGreenLetters = getWordsWithYellowAndGreenLetters initialDict colorMatchingList
        grayLettersFromList = getLettersFromColor Gray colorMatchingList
        yellowLettersFromList = getLettersFromColor Yellow colorMatchingList
        indexesOfGreenLetters = [ ind | (ind, _, Green) <- colorMatchingList]
        wordsWithoutGreenLetters = map (\(ind, word) ->
            (ind, removeGreenLetters indexesOfGreenLetters word indexToStartCountingFrom emptyList))
            wordsWithYellowAndGreenLetters
        wordsWithoutYellowLetters = map (\(ind, word) -> (ind, removeAllYellowLetters yellowLettersFromList emptyList word))
            wordsWithoutGreenLetters
        wordsWithoutGrayLetters = filter (doesntMatchGrayLetters grayLettersFromList) wordsWithoutYellowLetters
        indexesOfWordsWithoutGrayLetters = map fst wordsWithoutGrayLetters
    in filter (\(ind, _) -> ind `elem` indexesOfWordsWithoutGrayLetters) wordsWithYellowAndGreenLetters

getFilteredDict :: [[Char]] -> [(Int, Char, Color)] -> [[Char]]
getFilteredDict [] _ = []
getFilteredDict _ [] = []
getFilteredDict dict colorMatchingList  = [ word | (_, word) <- getValidWords dict colorMatchingList]

-- Calculates the coefficient of efficiency for every word
sumCoefficientForCurrentWord :: [Char] -> [[Char]] -> Int
sumCoefficientForCurrentWord potentialOfferWord dict =
    let calculateRemovedWords secretWord =
            let colorMatchingList = getColorMatching potentialOfferWord secretWord
                filteredDict = getFilteredDict dict colorMatchingList 
            in (length dict - length filteredDict)
    in sum (map calculateRemovedWords dict)

getBestWord :: [[Char]] -> IO [Char]
getBestWord [] = do
    putStrLn ( redColor ++ "Речникът не може да бъде празен!" ++ whiteColor )
    exitSuccess
getBestWord dict =
    let listOfPairsWordAndCoefficient = [(word, sumCoefficientForCurrentWord word dict) | word <- dict]
        maxPair = maximumBy (comparing snd) listOfPairsWordAndCoefficient
    in return (fst maxPair)

