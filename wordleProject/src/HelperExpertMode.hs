module HelperExpertMode ( startGameHelperExpertMode ) where

import Color (Color(Green))
import ColorUtils (redColor, whiteColor, greenColor)
import System.Exit (exitSuccess)
import HelperMode ( getListOfColors, getFilteredDict, getBestWord )
import ListUtils ( emptyList, indexToStartCountingFrom )
import Data.List (subsequences, sort, group)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import IOOperations ( loadDictionary, getIntFromUser )

type Answer = [(Int, Char, Color)]

getFilteredDictWithNMistakes :: Int -> [[Char]] -> [Answer] -> [[Char]]
getFilteredDictWithNMistakes maxMistakes dict listOfAnswers =
    let answersToConsider = length listOfAnswers - maxMistakes + 1
        validFilterCombinations = filter (\listOfFilters -> length listOfFilters == answersToConsider ) (subsequences listOfAnswers)
        concatenatedFilterCombinations = map concat validFilterCombinations
    in  if maxMistakes > length listOfAnswers
        then do
            (getFilteredDict dict (concat listOfAnswers))
        else do
            (concatMap (getFilteredDict dict) concatenatedFilterCombinations)
            

getBestWordFromDicts :: [[[Char]]] -> IO [Char]
getBestWordFromDicts dicts = do
    if null (getNotEmptyDictionaries dicts)
        then do
            putStrLn ( redColor ++ "Излъгал си и няма повече думи в речника!" ++ whiteColor)
            exitSuccess
        else do
            bestWordsFromAllDicts <- mapM getBestWord [dict | dict <- dicts, not (null dict)]
            let sortedWords = sort bestWordsFromAllDicts
                groupedWords = group sortedWords
                mapWordCount = map (\currGroup -> (head currGroup, length currGroup)) groupedWords
            return (fst (maximumBy (comparing snd) mapWordCount))

getNotEmptyDictionaries :: [[[Char]]] -> [[[Char]]]
getNotEmptyDictionaries dicts = [dict | dict <- dicts, not (null dict)]

evaluateGuessHelperExpertMode :: [[Char]] -> Int -> Int -> [Answer] -> IO b
evaluateGuessHelperExpertMode dict wordLength maxMistakes listOfAnswers = do
    bestWord <- getBestWordFromDicts [dict]
    putStrLn ("Предложената дума е: " ++ bestWord)
    listOfColors <- getListOfColors bestWord emptyList indexToStartCountingFrom (length bestWord)
    let filteredDicts = getFilteredDictWithNMistakes (maxMistakes + 1) dict (listOfAnswers ++ [listOfColors])
    if null filteredDicts
        then do
            putStrLn ( redColor ++ "Излъгал си и няма повече думи в речника!" ++ whiteColor)
            exitSuccess
        else do
            let listOfGreenLetters = [ letter | (_, letter, Green) <- listOfColors]
                newDict = filter (/= bestWord) dict
            if length listOfGreenLetters == wordLength
                then do
                    putStrLn ( greenColor ++ "Браво, позна думата: " ++ bestWord ++ whiteColor)
                    exitSuccess
                else
                    evaluateGuessHelperExpertMode (getFilteredDict newDict listOfColors) wordLength maxMistakes 
                                                  (listOfAnswers ++ [listOfColors])

startGameHelperExpertMode :: FilePath -> IO ()
startGameHelperExpertMode path = do
    wordLength <- getIntFromUser "Моля, въведете желаната дължина на думите (или 0 за изход): "
    dict <- loadDictionary path wordLength
    maxMistakes <- getIntFromUser "Моля, въведете максимум позволени грешки: "
    evaluateGuessHelperExpertMode dict wordLength maxMistakes emptyList