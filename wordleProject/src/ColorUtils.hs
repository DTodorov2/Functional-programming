module ColorUtils (greenColor, whiteColor, redColor, colorTheLetters, getColorMatching, 
                   removeGreenLetters, changeColorTo, removeAllYellowLetters, colorOfTheLetter) where

import Color ( Color(..) )
import Data.List ( sortBy, isPrefixOf )
import Data.Ord ( comparing )
import ListUtils ( indexToStartCountingFrom, emptyList, removeLetterFromList, getIndexes )

-- Color codes for terminal output
redColor :: String
redColor = "\x1b[31m"

greenColor :: String
greenColor = "\x1b[32m"

yellowColor :: String
yellowColor = "\x1b[33m"

grayColor :: String
grayColor = "\x1b[90m"

whiteColor :: String
whiteColor = "\x1b[0m"

-- Main function to get the color matching between the offer word and the secret word
getColorMatching :: [Char] -> [Char] -> [(Int, Char, Color)]
getColorMatching [] _ = []
getColorMatching _ [] = []
getColorMatching guess secretWord
    | length guess /= length secretWord = []
    | otherwise = 
        let pairs = zip guess secretWord
            greenLetters = findGreenLetters pairs
            indexesOfGreenLetters = getIndexes greenLetters
            secretWordWithoutGreenLetters = removeGreenLetters indexesOfGreenLetters secretWord indexToStartCountingFrom emptyList
            yellowLetters = findYellowLetters
                                guess 
                                secretWordWithoutGreenLetters 
                                secretWord
                                indexesOfGreenLetters 
                                emptyList
                                indexToStartCountingFrom
            indexesOfYellowLetters = getIndexes yellowLetters
            grayLetters = findGrayLetters guess (indexesOfYellowLetters ++ indexesOfGreenLetters)
        in  greenLetters ++ yellowLetters ++ grayLetters

-- Function to remove the green letters from the word, returns the modified word
removeGreenLetters :: [Int] -> [Char] -> Int -> [Char] -> [Char]
removeGreenLetters [] word _ _= word
removeGreenLetters _ [] _ newWord = newWord
removeGreenLetters listOfIndexesOfGreenLetters (x:restWord) currentInd newWord =
    if currentInd `elem` listOfIndexesOfGreenLetters
        then removeGreenLetters listOfIndexesOfGreenLetters restWord (currentInd + 1) newWord
        else removeGreenLetters listOfIndexesOfGreenLetters restWord (currentInd + 1) (newWord ++ [x])

--Function to remove the yellow letters from the word, returns the modified word
removeAllYellowLetters :: [Char] -> [Char] -> [Char] -> [Char]
removeAllYellowLetters [] newWord restWord = newWord ++ restWord
removeAllYellowLetters _ newWord [] = newWord
removeAllYellowLetters yellowLetters newWord (x:restWord) =
    if x `elem` yellowLetters
        then removeAllYellowLetters (removeLetterFromList yellowLetters x) newWord restWord
        else removeAllYellowLetters yellowLetters (newWord ++ [x]) restWord

-- Finds green letters that match in both words
findGreenLetters :: [(Char, Char)] -> [(Int, Char, Color)]
findGreenLetters [] = []
findGreenLetters pairs = 
     [(i, letterOfferWord, Green) | (i, (letterOfferWord, letterSecretWord)) <- zip [1..] pairs, 
                                                                                letterOfferWord == letterSecretWord]

-- Function to find yellow letters that are in the offer word but not in the same position in the secret word
findYellowLetters :: [Char] -> [Char] -> [Char] -> [Int] -> [(Int, Char, Color)] -> Int -> [(Int, Char, Color)] 
findYellowLetters _ [] _ _ yellowLetters _ = yellowLetters
findYellowLetters [] _ _ _ yellowLetters _ = yellowLetters
findYellowLetters (letter:remainingGuess) secretWordExcludingGreen secretWord usedPositions yellowLetters index =
    if index > length secretWord
        then yellowLetters
        else if canBeYellow letter secretWordExcludingGreen index usedPositions
               then findYellowLetters 
                       remainingGuess 
                       (removeOccuranceFromSecretWord secretWordExcludingGreen letter)
                       secretWord
                       (usedPositions ++ [index]) 
                       ((index, letter, Yellow) : yellowLetters) 
                       (index + 1)
               else findYellowLetters 
                       remainingGuess 
                       secretWordExcludingGreen
                       secretWord 
                       usedPositions 
                       yellowLetters 
                       (index + 1)

-- Checks if the letter can be yellow
canBeYellow :: Char -> [Char] -> Int -> [Int] -> Bool
canBeYellow letter secretWordExcludingGreen index usedPositions = 
    index `notElem` usedPositions && letter `elem` secretWordExcludingGreen

-- Helper function to remove a letter from the secret word
removeOccuranceFromSecretWord :: [Char] -> Char -> [Char]
removeOccuranceFromSecretWord [] _ = []
removeOccuranceFromSecretWord (x:xs) letter = 
    if x == letter 
        then xs 
        else x : removeOccuranceFromSecretWord xs letter

findGrayLetters :: [Char] -> [Int] -> [(Int, Char, Color)]
findGrayLetters guess markedPositions =
    [(i, letter, Gray) | (i, letter) <- zip [1..] guess, i `notElem` markedPositions]

colorTheLetters :: [(Int, Char, Color)] -> [Char]
colorTheLetters colorPattern = 
    let sortedColorPattern = sortBy (comparing  (\(i, _, _) -> i)) colorPattern
    in concatMap (\(_, letter, color) -> changeColorTo color letter) sortedColorPattern

-- to say that it is from chatGpt
changeColorTo :: Color -> Char -> [Char]
changeColorTo wantedColor letter
    | wantedColor == Green     = greenColor ++ [letter] ++ whiteColor
    | wantedColor == Yellow    = yellowColor ++ [letter] ++ whiteColor
    | wantedColor == Gray      = grayColor ++ [letter] ++ whiteColor
    | otherwise                = [letter]

colorOfTheLetter :: [Char] -> Color
colorOfTheLetter letter = 
    if greenColor `isPrefixOf` letter
        then Green
        else if yellowColor `isPrefixOf` letter
            then Yellow
            else Gray
            
    