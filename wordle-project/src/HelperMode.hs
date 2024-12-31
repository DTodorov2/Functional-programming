module HelperMode (startGameHelperMode, getWordsWithYellowLetters, getWordsWithGreen, getWordsWithGrayLetters, removeYellowLetters, sumCoefficientForCurrentWord) where

import IOOperations ( getLengthWords, loadDictonary )
import EasyGame ( askForConfirmation )
import Data.Foldable ( maximumBy )
import Data.Ord ( comparing )
import System.IO ( hFlush, stdout )
import System.Exit (exitSuccess)

-- in the beggining the listOfColors is [] and the ind is 0
-- returns the listOfColors [(ind / -1, (letter, color))], for example, for the current word
getListOfColors :: Num t => [Char] -> [(t, Char, String)] -> t -> Int -> IO [(t, Char, String)]
getListOfColors [] listOfColors _ _ = return listOfColors
getListOfColors (x:xs) listOfColors ind wordLength
  | length (x:xs) /= wordLength   = do
    putStr "\x1b[31mДумата не е с коректна дължина! Опитай пак: \x1b[0m"
    return []
  | otherwise = do
                putStrLn "Напиши комбинацията от букви, която отговаря на съответния цвят: (gr - green, y - yellow, g - gray):"
                putStr ("Цвят за буква " ++ [x] ++ ": ")
                hFlush stdout
                color <- getLine
                if color `elem` ["gr", "y", "g"]
                    then getListOfColors xs ((ind + 1, x, color) : listOfColors) (ind + 1) (wordLength - 1)
                    else do
                        putStrLn "\x1b[31mНевалиден цвят!\x1b[0m"
                        getListOfColors (x:xs) listOfColors ind wordLength

-- работи и изкарва: ["apple","angle","cycle","eagle","maple"]
getWordsWithGreen :: Eq a => [[a]] -> [(Int, a, String)] -> [[a]]
getWordsWithGreen dict colorMatchingList =
    let greenLetters = [(ind, letter) | (ind, letter, "gr") <- colorMatchingList]
    in filter (matchesGreenLetters greenLetters) dict
    where matchesGreenLetters conditionList word = all (\(ind, ch) -> word !! (ind - 1) == ch) conditionList

matchesYellowLetters :: (Foldable t, Eq a1) => t (Int, a1) -> (a2, [a1]) -> Bool
matchesYellowLetters yellowLetters pair = all (\(ind, ch) -> (snd pair !! (ind - 1) /= ch) && ch `elem` snd pair) yellowLetters

-- raboti i izkarva [(1,"apple"),(3,"angle")]
getWordsWithYellowLetters :: (Num a, Enum a, Eq a, Eq b) => [[b]] -> [(Int, b, String)] -> [(a, [b])]
getWordsWithYellowLetters initialDict colorMatchingList =
    let dictOfWordsWithGreenLetters = zip [1..] (getWordsWithGreen initialDict colorMatchingList)
        yellowLetters = [ (ind, letter) | (ind, letter, "y") <- colorMatchingList]
        indexesOfGreenLetters = [ ind | (ind, _, "gr") <- colorMatchingList]
        wordsWithoutGreenLetters = map (\(ind, word) -> (ind, removeGreenLetters indexesOfGreenLetters word 1 [])) dictOfWordsWithGreenLetters
        wordsWithYellowLetters = filter (matchesYellowLetters yellowLetters) wordsWithoutGreenLetters
        indexesOfWordsWithYellowLetters = [ ind | (ind, _) <- wordsWithYellowLetters]
    in filter (\(ind, _) -> ind `elem` indexesOfWordsWithYellowLetters) dictOfWordsWithGreenLetters

doesntMatchGrayLetters :: (Foldable t1, Foldable t2, Eq a1) => t1 (a2, a1) -> (a3, t2 a1) -> Bool
doesntMatchGrayLetters grayLetters pair = all (\(_, ch) -> ch `notElem` snd pair) grayLetters

-- dict e rechnika ot nachalnite dumi
-- raboti i izkarva [(1,"apple")]
-- -- vrushta vschki dumi, koito nqmat gray bukvi
getWordsWithGrayLetters :: [[Char]] -> [(Int, Char, String)] -> [(Int, [Char])]
getWordsWithGrayLetters initialDict colorMatchingList =
    let dictOfWordsWithYellowAndGreenLetters = getWordsWithYellowLetters initialDict colorMatchingList
        -- toest [(1,"apple"),(3,"angle")]
        grayLetters = [ (ind, letter) | (ind, letter, "g") <- colorMatchingList]
        yellowLetters = [ letter | (_, letter, "y") <- colorMatchingList]
        indexesOfGreenLetters = [ ind | (ind, _, "gr") <- colorMatchingList]
        wordsWithoutGreenLetters = map (\(ind, word) -> (ind, removeGreenLetters indexesOfGreenLetters word 1 [])) dictOfWordsWithYellowAndGreenLetters
        wordsWithoutYellowLetters = map (\(ind, word) -> (ind, removeYellowLetters yellowLetters [] word)) wordsWithoutGreenLetters
        wordsWithoutGrayLetters = filter (doesntMatchGrayLetters grayLetters) wordsWithoutYellowLetters
        indexesOfWordsWithoutGrayLetters = [ ind | (ind, _) <- wordsWithoutGrayLetters]
    in filter (\(ind, _) -> ind `elem` indexesOfWordsWithoutGrayLetters) dictOfWordsWithYellowAndGreenLetters

-- raboti, izkarva [(2,"apple")], pri vhod ["gnale","apple", "humbal", "eagle", "angle", "cycle", "maple", "nesho", "hahah"] [(4,'l',"gr"),(5,'e',"gr"),(2,'a',"y"),(1,'e',"g"),(3,'g',"g")]
getFilteredDict :: [[Char]] -> [(Int, Char, String)] -> [[Char]]
getFilteredDict dict colorMatchingList = [ word | (_, word) <- (getWordsWithGrayLetters dict colorMatchingList)]

-- greenLetters e ot vida [(ind, (letter, "gr"))]
removeGreenLetters :: (Foldable t1, Eq t2, Num t2) => t1 t2 -> [a] -> t2 -> [a] -> [a]
removeGreenLetters _ [] _ newWord = newWord
removeGreenLetters listOfIndexesOfGreenLetters (x:restWord) currentInd newWord =
    if currentInd `elem` listOfIndexesOfGreenLetters
        then removeGreenLetters listOfIndexesOfGreenLetters restWord (currentInd + 1) newWord
        else removeGreenLetters listOfIndexesOfGreenLetters restWord (currentInd + 1) (newWord ++ [x])

removeLetterFromList :: Eq a => [a] -> a -> [a] -> [a]
removeLetterFromList [] _ newList = newList
removeLetterFromList (x:yellowLettersList) letter newList =
    if x == letter
        then removeLetterFromList yellowLettersList letter newList
        else removeLetterFromList yellowLettersList letter (x : newList)

removeYellowLetters :: Eq a => [a] -> [a] -> [a] -> [a]
removeYellowLetters [] newWord restWord = newWord ++ restWord
removeYellowLetters _ newWord [] = newWord
removeYellowLetters yellowLetters newWord (x:restWord) =
    if x `elem` yellowLetters
        then removeYellowLetters (removeLetterFromList yellowLetters x []) newWord restWord
        else removeYellowLetters yellowLetters (newWord ++ [x]) restWord

-- -- v nachaloto lista ot yellow shte e [], pairs = zip [1..] offerWord, index pochva ot 1
getYellowLetters :: (Eq t, Eq b, Num t) => [b] -> [b] -> [t] -> [(t, b, String)] -> t -> [(t, b, String)]
getYellowLetters [] _ _ listYellow _ = listYellow
getYellowLetters (x:restOfferWord) secretWordWithoutGreenLetters markedPositions listOfYellowLetters index =
    if (index `notElem` markedPositions) && (x `elem` secretWordWithoutGreenLetters)
        then getYellowLetters restOfferWord [letter | letter <- secretWordWithoutGreenLetters, letter /= x] (markedPositions ++ [index]) ((index, x, "y") : listOfYellowLetters) (index + 1)
        else getYellowLetters restOfferWord secretWordWithoutGreenLetters markedPositions listOfYellowLetters (index + 1)


getGrayLetters :: (Num a, Enum a, Foldable t, Eq a) => [b] -> t a -> [(a, b, String)]
getGrayLetters offerWord markedPositions =
    [(i, letter, "g") | (i, letter) <- zip [1..] offerWord, i `notElem` markedPositions]

-- -- работи и изкарва такъв резултат: [(4,'l',"gr"),(5,'e',"gr"),(2,'a',"y"),(1,'e',"g"),(3,'g',"g")]
-- -- тук правя реално списъка на буквите с цветовете и индексите
getColorMatching :: (Num a1, Enum a1, Eq a2, Eq a1) => [a2] -> [a2] -> [(a1, a2, String)]
getColorMatching offerWord secretWord =
    let pairs = zip offerWord secretWord
        greenLetters = [(i, letterOfferWord, "gr") | (i, (letterOfferWord, letterSecretWord)) <- zip [1..] pairs, letterOfferWord == letterSecretWord]
        listOfIndexesOfGreenLetters = [ind | (ind, _, _) <- greenLetters]
        secretWordWithoutGreenLetters = removeGreenLetters listOfIndexesOfGreenLetters secretWord 1 []
        offerWordWithoutGreenLetters = removeGreenLetters listOfIndexesOfGreenLetters offerWord 1 []
        yellowLetters = getYellowLetters offerWordWithoutGreenLetters secretWordWithoutGreenLetters listOfIndexesOfGreenLetters [] 1
        listOfIndexesOfYellowLetters = [ind | (ind, _, _) <- yellowLetters]
        grayLetters = getGrayLetters offerWord (listOfIndexesOfYellowLetters ++ listOfIndexesOfGreenLetters)
    in (greenLetters ++ yellowLetters ++ grayLetters)

sumCoefficientForCurrentWord :: [Char] -> [[Char]] -> Int
sumCoefficientForCurrentWord potentialOfferWord dict =
    let calculateRemovedWords secretWord =
            let colorMatchingList = getColorMatching potentialOfferWord secretWord
                filteredDict = getFilteredDict dict colorMatchingList
            in (length dict - length filteredDict)
    in sum (map calculateRemovedWords dict)

getBestWord :: [[Char]] -> IO [Char]
getBestWord dict =
    let listOfPairsWordAndCoefficient = [(word, sumCoefficientForCurrentWord word dict) | word <- dict]
    in if null listOfPairsWordAndCoefficient
        then do
            putStrLn "\x1b[31mНякой от предишните ти отговори е бил лъжа или думата не е в речника!\x1b[0m"
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
evaluateGuessHelper [] _ = do
    putStrLn "Речникът е празен!"
    exitSuccess
evaluateGuessHelper dict wordLength = do
    bestWord <- getBestWord dict
    putStrLn ("Предложената дума е: " ++ bestWord)
    confirmationAnswer <- askForConfirmation bestWord
    if confirmationAnswer
        then do
            listOfColors <- getListOfColors bestWord [] 0 (length bestWord)
            let listOfGreenLetters = [ letter | (_, letter, "gr") <- listOfColors]
                newDict = filter (/= bestWord) dict
            if length listOfGreenLetters == wordLength
                then
                    putStrLn ("\x1b[32mБраво, позна думата: " ++ bestWord ++ "\x1b[0m")
                else
                    evaluateGuessHelper (getFilteredDict newDict listOfColors) wordLength
        else do
            word <- getWordFromPlayer wordLength
            listOfColors <- getListOfColors word [] 0 (length word)
            let listOfGreenLetters = [ letter | (_, letter, "gr") <- listOfColors]
                newDict = filter (/= bestWord) dict
            if length listOfGreenLetters == wordLength
                then
                    putStrLn ("\x1b[32mБраво, позна думата: " ++ word ++ "\x1b[0m")
                else
                    evaluateGuessHelper (getFilteredDict newDict listOfColors) wordLength

startGameHelperMode :: FilePath -> IO ()
startGameHelperMode path = do
    wordLength <- getLengthWords
    dict <- loadDictonary path wordLength
    evaluateGuessHelper dict wordLength
