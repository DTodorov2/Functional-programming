module HelperMode ( getValidWords, startGameHelperMode ) where

import IOOperations ( getLengthWords, loadDictionary )
import Data.Foldable ( maximumBy )
import Data.Ord ( comparing )
import System.IO ( hFlush, stdout )
import System.Exit ( exitSuccess )
import Color ( Color(..), parseStringToColor )
import ColorUtils ( getColorMatching, removeGreenLetters, redColor, whiteColor, greenColor)
import ListUtils (isValidIndex, removeLetterFromList, indexToStartCountingFrom, emptyList)

-- in the beggining the listOfColors is [] and the ind is 0
-- returns the listOfColors [(ind / -1, (letter, color))], for example, for the current word
getListOfColors :: Num a => [Char] -> [(a, Char, Color)] -> a -> Int -> IO [(a, Char, Color)]
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

-- работи и изкарва: ["apple","angle","cycle","eagle","maple"]
getWordsMatchingGreenLetters  :: Eq a => [[a]] -> [(Int, a, Color)] -> [[a]]
getWordsMatchingGreenLetters  dict colorMatchingList =
    let greenLetters = [(ind, letter) | (ind, letter, Green) <- colorMatchingList]
    in filter (matchesGreenLetters greenLetters) dict
    where matchesGreenLetters conditionList word = 
            all (\(ind, ch) -> isValidIndex ind word && (word !! (ind - 1) == ch)) conditionList

matchesYellowLettersFilters :: (Foldable t, Eq a1) => t (Int, a1) -> (a2, [a1]) -> Bool
matchesYellowLettersFilters yellowLetters pair =
    let word = snd pair
    in all (\(ind, ch) -> isValidIndex ind word && (((word !! (ind - 1)) /= ch) && ch `elem` word)) yellowLetters

-- raboti i izkarva [(1,"apple"),(3,"angle")]
getWordsWithYellowLetters :: (Num a, Enum a, Eq a, Eq b) => [[b]] -> [(Int, b, Color)] -> [(a, [b])]
getWordsWithYellowLetters initialDict colorMatchingList =
    let dictOfWordsWithGreenLetters = zip [1..] (getWordsMatchingGreenLetters initialDict colorMatchingList)
        yellowLetters = [ (ind, letter) | (ind, letter, Yellow) <- colorMatchingList]
        indexesOfGreenLetters = [ ind | (ind, _, Green) <- colorMatchingList]
        -- wordsWithoutGreenLetters = 
        --     map (\(ind, word) -> 
        --     (ind, removeGreenLetters indexesOfGreenLetters word indexToStartCountingFrom emptyList)) 
        --     dictOfWordsWithGreenLetters
        wordsMatchingYellowLettersFilters = filter (matchesYellowLettersFilters yellowLetters) dictOfWordsWithGreenLetters
        indexesOfWordsWithYellowLetters = map fst wordsMatchingYellowLettersFilters
    in filter (\(ind, _) -> ind `elem` indexesOfWordsWithYellowLetters) dictOfWordsWithGreenLetters

doesntMatchGrayLetters :: (Foldable t1, Foldable t2, Eq a1) => t1 (a2, a1) -> (a3, t2 a1) -> Bool
doesntMatchGrayLetters grayLetters pair = all (\(_, ch) -> ch `notElem` snd pair) grayLetters

-- dict e rechnika ot nachalnite dumi
-- raboti i izkarva [(1,"apple")]
-- -- vrushta vschki dumi, koito nqmat gray bukvi
getValidWords :: (Num a, Enum a, Eq a, Eq b) => [[b]] -> [(Int, b, Color)] -> [(a, [b])]
getValidWords initialDict colorMatchingList =
    let dictOfWordsWithYellowAndGreenLetters = getWordsWithYellowLetters initialDict colorMatchingList
        -- toest [(1,"apple"),(3,"angle")]
        grayLetters = [ (ind, letter) | (ind, letter, Gray) <- colorMatchingList]
        yellowLetters = [ letter | (_, letter, Yellow) <- colorMatchingList]
        indexesOfGreenLetters = [ ind | (ind, _, Green) <- colorMatchingList]
        wordsWithoutGreenLetters = map (\(ind, word) -> 
            (ind, removeGreenLetters indexesOfGreenLetters word indexToStartCountingFrom emptyList)) 
            dictOfWordsWithYellowAndGreenLetters
        wordsWithoutYellowLetters = map (\(ind, word) -> 
            (ind, removeAllYellowLetters yellowLetters emptyList word)) 
            wordsWithoutGreenLetters
        wordsWithoutGrayLetters = filter (doesntMatchGrayLetters grayLetters) wordsWithoutYellowLetters
        indexesOfWordsWithoutGrayLetters = map fst wordsWithoutGrayLetters
    in filter (\(ind, _) -> ind `elem` indexesOfWordsWithoutGrayLetters) dictOfWordsWithYellowAndGreenLetters

-- raboti, izkarva [(2,"apple")], pri vhod ["gnale","apple", "humbal", "eagle", "angle", "cycle", "maple", "nesho", "hahah"] [(4,'l',"gr"),(5,'e',"gr"),(2,'a',"y"),(1,'e',"g"),(3,'g',"g")]
getFilteredDict :: Eq b => [[b]] -> [(Int, b, Color)] -> [[b]]
getFilteredDict [] _ = []
getFilteredDict _ [] = []
getFilteredDict dict colorMatchingList = [ word | (_, word) <- getValidWords dict colorMatchingList]

removeAllYellowLetters :: Eq a => [a] -> [a] -> [a] -> [a]
removeAllYellowLetters [] newWord restWord = newWord ++ restWord
removeAllYellowLetters _ newWord [] = newWord
removeAllYellowLetters yellowLetters newWord (x:restWord) =
    if x `elem` yellowLetters
        then removeAllYellowLetters (removeLetterFromList yellowLetters x emptyList) newWord restWord
        else removeAllYellowLetters yellowLetters (newWord ++ [x]) restWord

sumCoefficientForCurrentWord :: Eq a => [a] -> [[a]] -> Int
sumCoefficientForCurrentWord potentialOfferWord dict =
    let calculateRemovedWords secretWord =
            let colorMatchingList = getColorMatching potentialOfferWord secretWord
                filteredDict = getFilteredDict dict colorMatchingList
            in (length dict - length filteredDict)
    in sum (map calculateRemovedWords dict)

getBestWord :: Eq a => [[a]] -> IO [a]
getBestWord [] = do
    putStrLn ( redColor ++ "Речникът не може да бъде празен!" ++ whiteColor )
    exitSuccess
getBestWord dict =
    let listOfPairsWordAndCoefficient = [(word, sumCoefficientForCurrentWord word dict) | word <- dict]
    in return (fst (maximumBy (comparing snd) listOfPairsWordAndCoefficient))

evaluateGuessHelper :: [[Char]] -> Int -> IO ()
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
        then
            putStrLn ( greenColor ++ "Браво, позна думата: " ++ bestWord ++ whiteColor)
        else
             evaluateGuessHelper (getFilteredDict newDict listOfColors) wordLength

startGameHelperMode :: FilePath -> IO ()
startGameHelperMode path = do
    wordLength <- getLengthWords
    dict <- loadDictionary path wordLength
    evaluateGuessHelper dict wordLength
