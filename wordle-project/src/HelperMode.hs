module HelperMode where

import IOOperations ( getLengthWords )
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
    putStr "Думата не е с коректна дължина! Опитай пак: "
    return []
  | otherwise = do
                putStrLn "Напиши комбинацията от букви, която отговаря на съответния цвят: (gr - green, y - yellow, g - gray):"
                putStr ("Цвят за буква " ++ [x] ++ ": ")
                hFlush stdout
                color <- getLine
                if color `elem` ["gr", "y", "g"]
                    then if color == "gr"
                        then getListOfColors xs ((ind, (x, color)) : listOfColors) (ind + 1) (wordLength - 1)
                        else getListOfColors xs ((-1, (x, color)) : listOfColors) (ind + 1) (wordLength - 1)
                    else do
                        putStrLn "Невалиден цвят!"
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
getFilteredDict dict listOfColors = do
    let greenPairs = [(ind, letter) | (ind, (letter, color)) <- listOfColors, color == "gr"]
        yellowLetters = [letter | (_, (letter, color)) <- listOfColors, color == "y"]
        initialGrayLetters = [letter | (_, (letter, color)) <- listOfColors, color == "g"]
        actualGrayLetters = removeNeededLetters initialGrayLetters yellowLetters []
    putStrLn "printq zeleni dvoiki"
    print greenPairs
    putStrLn "printq julti dvoiki"
    print yellowLetters
    putStrLn "printq sivi dvoiki"
    print actualGrayLetters
    return (getWordsWithGreenLetters (getWordsWithYellowLetters (getWordsWithGrayLetters dict actualGrayLetters) yellowLetters) greenPairs)

removeNeededLetters [] _ newGrayLetters = newGrayLetters
removeNeededLetters (x:restGray) yellowLetters newGrayList = if x `elem` yellowLetters 
    then removeNeededLetters restGray yellowLetters newGrayList
    else removeNeededLetters restGray yellowLetters (x:newGrayList)

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

sumCoefficientForCurrentWord potentialOfferWord dict = do
    let calculateRemovedWords secretWord = do
            let colorMatchingList = getColorMatching potentialOfferWord secretWord
            putStrLn "printq offer word"
            print potentialOfferWord
            putStrLn "printq secret word"
            print secretWord
            putStrLn "printq color maching list"
            print colorMatchingList
            filteredDict <- getFilteredDict dict colorMatchingList
            return (length dict - length filteredDict)
    results <- mapM calculateRemovedWords dict
    return $ sum results
    --sum (map calculateRemovedWords dict)


getBestWord :: (Show a2, Eq a2) => [[a2]] -> IO [a2]
getBestWord dict = do
    listOfPairsWordAndCoefficient <- mapM (\word -> do
        coefficient <- sumCoefficientForCurrentWord word dict
        return (word, coefficient)) dict
    putStrLn "prntqt list ot dvoiki ot duma i koeficient"
    print listOfPairsWordAndCoefficient
    if null listOfPairsWordAndCoefficient
        then do
            putStrLn "\x1b[31mНякой от предишните ти отговори е бил лъжа!\x1b[0m"
            exitSuccess
        else do
            let bestWord = fst (maximumBy (comparing snd) listOfPairsWordAndCoefficient)
            return bestWord
-- getBestWord dict = do
--     let listOfPairsWordAndCoefficient = [(word, sumCoefficientForCurrentWord word dict) | word <- dict]
--     print listOfPairsWordAndCoefficient
--         -- reverse, so that I can return he first word with highest coefficient
--     return (fst (maximumBy (comparing snd) listOfPairsWordAndCoefficient))

getWordFromPlayer :: Int -> IO String
getWordFromPlayer wordLength = do
    putStr "Въведете дума по Ваш избор: "
    word <- getLine
    if length word /= wordLength
        then do
            putStrLn ("Думата трябва да е с дължина" ++ show wordLength)
            getWordFromPlayer wordLength
        else
            return word

evaluateGuessHelper :: [[Char]] -> Int -> IO ()
evaluateGuessHelper dict wordLength = do
    bestWord <- getBestWord dict
    -- use pattern matching -> if the dict is empty, then head dict will throw an error!
    putStrLn ("Предложената дума е: " ++ bestWord)
    confirmationAnswer <- askForConfirmation
    if confirmationAnswer
        then do
            listOfColors <- getListOfColors bestWord [] 0 (length bestWord)
            print listOfColors
            let listOfGreenLetters = [ letter | (_, (letter, color)) <- listOfColors, color == "gr"]
            if length listOfGreenLetters == wordLength
                then
                    putStrLn ("Браво, позна думата: " ++ bestWord)
                else do
                    filteredDct <- getFilteredDict dict listOfColors
                    putStrLn "tuk sym"
                    print filteredDct
                    evaluateGuessHelper filteredDct wordLength
        else do
            word <- getWordFromPlayer wordLength
            listOfColors <- getListOfColors word [] 0 (length word)
            let listOfGreenLetters = [ letter | (_, (letter, color)) <- listOfColors, color == "gr"]
            if length listOfGreenLetters == wordLength
                then
                    putStrLn ("Браво, позна думата: " ++ word)
                else do
                    filteredDct <- getFilteredDict dict listOfColors
                    print filteredDct
                    evaluateGuessHelper filteredDct wordLength

startGameHelperMode dict = do
    evaluateGuessHelper dict (length (head dict))
           
