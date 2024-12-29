module HelperMode where

import IOOperations ( getLengthWords, loadDictonary )
import NormalGame (removeOccuranceFromActualWord)
import EasyGame ( askForConfirmation )
import Data.Foldable ( maximumBy )
import Data.Ord ( comparing )
import System.IO ( hFlush, stdout )
import System.Exit (exitSuccess)
getInitialDictionary :: Foldable t => Int -> [t a] -> [t a]
getInitialDictionary wordLength dict = [ word | word <- dict, length word == wordLength]

-- in the beggining the listOfColors is [] and the ind is 0
-- returns the listOfColors [(ind / -1, (letter, color))], for example, for the current word
getListOfColors :: Num t => [Char] -> [(t, (Char, String))] -> t -> Int -> IO [(t, (Char, String))]
getListOfColors [] listOfColors _ _ = return listOfColors
getListOfColors (x:xs) listOfColors ind wordLength
  | length (x:xs) /= wordLength   = do
    putStr "\x1b[32mДумата не е с коректна дължина! Опитай пак: \x1b[0m"
    return []
  | otherwise = do
                putStrLn "Напиши комбинацията от букви, която отговаря на съответния цвят: (gr - green, y - yellow, g - gray):"
                putStr ("Цвят за буква " ++ [x] ++ ": ")
                hFlush stdout
                color <- getLine
                if color `elem` ["gr", "y", "g"]
                    then if color == "gr"
                        then getListOfColors xs ((ind + 1, (x, color)) : listOfColors) (ind + 1) (wordLength - 1)
                        else getListOfColors xs ((-1, (x, color)) : listOfColors) (ind + 1) (wordLength - 1)
                    else do
                        putStrLn "\x1b[32mНевалиден цвят!\x1b[0m"
                        getListOfColors (x:xs) listOfColors ind wordLength

getWordsWithGrayLetters :: (Foldable t, Eq a) => [t a] -> [a] -> [t a]
getWordsWithGrayLetters dict [] = dict
getWordsWithGrayLetters dict grayLetters = [word | word <- dict, all (`notElem` word) grayLetters]

getWordsWithYellowLetters :: (Foldable t, Eq a) => [t a] -> [a] -> [t a]
getWordsWithYellowLetters dict [] = dict
getWordsWithYellowLetters dict yellowLetters = [word | word <- dict, all (`elem` word) yellowLetters]

getWordsWithGreenLetters :: Eq a => [[a]] -> [(Int, a)] -> [[a]]
getWordsWithGreenLetters dict [] = dict
getWordsWithGreenLetters dict greenPairs = [word | word <- dict, all (\(ind, letter) -> word !! (ind - 1) == letter) greenPairs]

-- words that do not contraverse the previous answers
-- if the returned list is empty, then the player has lied
getFilteredDict :: Eq a => [[a]] -> [(Int, (a, String))] -> [[a]]
getFilteredDict dict listOfColors =
    let greenPairs = [(ind, letter) | (ind, (letter, color)) <- listOfColors, color == "gr"]
        yellowLetters = [letter | (_, (letter, color)) <- listOfColors, color == "y"]
        initialGrayLetters = [letter | (_, (letter, color)) <- listOfColors, color == "g"]
        actualGrayLetters = removeNeededGrayLetters initialGrayLetters yellowLetters greenPairs []
    in getWordsWithGreenLetters
            (getWordsWithYellowLetters
                (getWordsWithGrayLetters dict actualGrayLetters)
            yellowLetters)
        greenPairs

removeNeededGrayLetters :: (Foldable t, Eq a1) => [a1] -> t a1 -> [(a2, a1)] -> [a1] -> [a1]
removeNeededGrayLetters [] _ _ newGrayLetters = newGrayLetters
removeNeededGrayLetters (x:restGray) yellowLetters greenPairs newGrayList =
    let greenLetters = [ letter | (_, letter) <- greenPairs ]
    in (if (x `elem` yellowLetters) || (x `elem` greenLetters)
        then removeNeededGrayLetters restGray yellowLetters greenPairs newGrayList
        else removeNeededGrayLetters restGray yellowLetters greenPairs (x:newGrayList))

getYellowLettersFromList :: (Eq a1, Num a2) => [a1] -> [(a1, a1)] -> [(a2, (a1, String))] -> [(a2, (a1, String))]
getYellowLettersFromList _ [] lettersList = lettersList
getYellowLettersFromList word ((f, s) : xs) lettersList = if f /= s && f `elem` word
    then getYellowLettersFromList (removeOccuranceFromActualWord word f) xs ((-1, (f, "y")) : lettersList)
    else  getYellowLettersFromList word xs lettersList

-- return list of colors like this [(ind / -1, (letter, color))]
getColorMatching :: (Enum a1, Eq a2, Num a1) => [a2] -> [a2] -> [(a1, (a2, String))]
getColorMatching offerWord secretWord =
    let pairs = zip offerWord secretWord
        greenLetters = [(i, (f, "gr")) | (i, (f, s)) <- zip [1..] pairs,  f == s]
        yellowLetters = getYellowLettersFromList secretWord pairs []
        grayLetters = [(-1, (f, "g"))| (f, s) <- pairs, f /= s, f `notElem` secretWord]
    in greenLetters ++ yellowLetters ++ grayLetters

-- sums the removed words for a current word
sumCoefficientForCurrentWord :: Eq a2 => [a2] -> [[a2]] -> Int
sumCoefficientForCurrentWord potentialOfferWord dict =
    let calculateRemovedWords secretWord =
            let colorMatchingList = getColorMatching potentialOfferWord secretWord
                filteredDict = getFilteredDict dict colorMatchingList
            in length dict - length filteredDict
    in sum (map calculateRemovedWords dict)


getBestWord :: Eq a2 => [[a2]] -> IO [a2]
getBestWord dict =
    let listOfPairsWordAndCoefficient = [(word, sumCoefficientForCurrentWord word dict) | word <- dict]
    in if null listOfPairsWordAndCoefficient
        then do
            putStrLn "\x1b[31mНякой от предишните ти отговори е бил лъжа!\x1b[0m"
            exitSuccess
        else do
            let bestWord = fst (maximumBy (comparing snd) listOfPairsWordAndCoefficient)
            return bestWord

getWordFromPlayer :: Int -> IO String
getWordFromPlayer wordLength = do
    putStr "Въведете дума по Ваш избор: "
    hFlush stdout
    word <- getLine
    if length word /= wordLength
        then do
            putStrLn ("\x1b[31mДумата трябва да е с дължина " ++ show wordLength ++ "!\x1b[0m")
            getWordFromPlayer wordLength
        else
            return word

evaluateGuessHelper :: [[Char]] -> Int -> IO ()
evaluateGuessHelper dict wordLength = do
    bestWord <- getBestWord dict
    -- use pattern matching -> if the dict is empty, then head dict will throw an error!
    putStrLn ("Предложената дума е: " ++ bestWord)
    confirmationAnswer <- askForConfirmation bestWord
    if confirmationAnswer
        then do
            listOfColors <- getListOfColors bestWord [] 0 (length bestWord)
            let listOfGreenLetters = [ letter | (_, (letter, color)) <- listOfColors, color == "gr"]
                newDict = filter (\word -> word /= bestWord) dict
            if length listOfGreenLetters == wordLength
                then
                    putStrLn ("\x1b[32mБраво, позна думата: " ++ bestWord ++ "\x1b[0m")
                else do
                    evaluateGuessHelper (getFilteredDict newDict listOfColors) wordLength
        else do
            word <- getWordFromPlayer wordLength
            listOfColors <- getListOfColors word [] 0 (length word)
            let listOfGreenLetters = [ letter | (_, (letter, color)) <- listOfColors, color == "gr"]
                newDict = filter (/= bestWord) dict
            if length listOfGreenLetters == wordLength
                then
                    putStrLn ("\x1b[32mБраво, позна думата: " ++ word ++ "\x1b[0m")
                else do
                    evaluateGuessHelper (getFilteredDict newDict listOfColors) wordLength

startGameHelperMode :: FilePath -> IO ()
startGameHelperMode path = do
    wordLength <- getLengthWords
    dict <- loadDictonary path wordLength
    evaluateGuessHelper dict wordLength
