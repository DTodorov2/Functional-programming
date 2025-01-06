module ColorUtils (greenColor, whiteColor, redColor, colorTheLetters, getColorMatching, 
                   removeGreenLetters, indexToStartCountingFrom, emptyList, changeColorTo) where
                    
import Color ( Color(..) )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import ListUtils ( indexToStartCountingFrom, emptyList, initialValue )

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
-- -- работи и изкарва такъв резултат: [(4,'l',"gr"),(5,'e',"gr"),(2,'a',"y"),(1,'e',"g"),(3,'g',"g")]
-- Main function to get the color matching between the offer word and the secret word
getColorMatching :: Eq a => [a] -> [a] -> [(Int, a, Color)]
getColorMatching [] _ = []
getColorMatching _ [] = []
getColorMatching guess secretWord
    | length guess /= length secretWord = []
    | otherwise = 
        let pairs = zip guess secretWord
            greenLetters = findGreenLetters pairs
            indexesOfGreenLetters = [ind | (ind, _, _) <- greenLetters]
            secretWordWithoutGreenLetters = removeGreenLetters indexesOfGreenLetters secretWord indexToStartCountingFrom emptyList
            guessWithoutGreenLetters = removeGreenLetters indexesOfGreenLetters guess indexToStartCountingFrom emptyList
            yellowLetters = findYellowLetters 
                                guessWithoutGreenLetters 
                                secretWordWithoutGreenLetters 
                                indexesOfGreenLetters 
                                emptyList
                                (findStartIndex indexesOfGreenLetters initialValue indexToStartCountingFrom)
            indexesOfYellowLetters = [ind | (ind, _, _) <- yellowLetters]
            grayLetters = findGrayLetters guess (indexesOfYellowLetters ++ indexesOfGreenLetters)
        in (greenLetters ++ yellowLetters ++ grayLetters)

-- greenLetters e ot vida [(ind, (letter, "gr"))]
-- Function to remove the green letters from the word, returns the modified word
removeGreenLetters :: (Eq t, Num t) => [t] -> [a] -> t -> [a] -> [a]
removeGreenLetters [] word _ _= word
removeGreenLetters _ [] _ newWord = newWord
removeGreenLetters listOfIndexesOfGreenLetters (x:restWord) currentInd newWord =
    if currentInd `elem` listOfIndexesOfGreenLetters
        then removeGreenLetters listOfIndexesOfGreenLetters restWord (currentInd + 1) newWord
        else removeGreenLetters listOfIndexesOfGreenLetters restWord (currentInd + 1) (newWord ++ [x])

-- Finds green letters that match in both words
findGreenLetters :: (Num a, Enum a, Eq b) => [(b, b)] -> [(a, b, Color)]
findGreenLetters [] = []
findGreenLetters pairs = 
     [(i, letterOfferWord, Green) | (i, (letterOfferWord, letterSecretWord)) <- zip [1..] pairs, 
                                                                                letterOfferWord == letterSecretWord]

-- -- v nachaloto lista ot yellow shte e [], pairs = zip [1..] offerWord, index pochva ot 1
-- Function to find yellow letters that are in the offer word but not in the same position in the secret word
findYellowLetters :: (Eq t, Eq b, Num t) => [b] -> [b] -> [t] -> [(t, b, Color)] -> t -> [(t, b, Color)]
findYellowLetters _ [] _ yellowLetters _ = yellowLetters
findYellowLetters [] _ _ yellowLetters _ = yellowLetters
findYellowLetters (x:remainingGuess) secretWordExcludingGreen usedPositions yellowLetters index =
    if theLetterCanBeYellow x index usedPositions secretWordExcludingGreen
        then findYellowLetters 
                remainingGuess 
                -- realno ne me interesuva tochno poziciqta, a che bukvata q ima v dumata i prosto trqq da q premahna vednuj
                (removeOccuranceFromSecretWord secretWordExcludingGreen x)
                (usedPositions ++ [index]) 
                ((index, x, Yellow) : yellowLetters) 
                (index + 1)
        else findYellowLetters 
                remainingGuess 
                secretWordExcludingGreen 
                usedPositions 
                yellowLetters 
                (index + 1)

-- Checks if the letter can be yellow
theLetterCanBeYellow :: (Foldable t1, Foldable t2, Eq a1, Eq a2) => a2 -> a1 -> t1 a1 -> t2 a2 -> Bool
theLetterCanBeYellow letter ind usedPositions secretWordExcludingGreen = 
    (ind `notElem` usedPositions) && (letter `elem` secretWordExcludingGreen)

-- Helper function to remove a letter from the secret word
removeOccuranceFromSecretWord :: Eq t => [t] -> t -> [t]
removeOccuranceFromSecretWord [] _ = []
removeOccuranceFromSecretWord (x:xs) letter = 
    if x == letter 
        then xs 
        else x : removeOccuranceFromSecretWord xs letter

findGrayLetters :: (Num a, Enum a, Foldable t, Eq a) => [b] -> t a -> [(a, b, Color)]
findGrayLetters offerWord markedPositions =
    [(i, letter, Gray) | (i, letter) <- zip [1..] offerWord, i `notElem` markedPositions]

--start винаги е 0
findStartIndex :: (Eq t1, Num t1, Num t2) => [t1] -> t1 -> t2 -> t2
findStartIndex [] _ startInd = startInd
findStartIndex (x:restListOfIndexesOfGreenLetters) start startInd =
    if (start + 1) /= x
        then startInd
        else findStartIndex restListOfIndexesOfGreenLetters (start + 1) (startInd + 1)

colorTheLetters :: [Char] -> [Char] -> [Char]
colorTheLetters guess actualWord = 
    let colorPattern = sortBy (comparing  (\(i, _, _) -> i)) (getColorMatching guess actualWord)
    in concatMap (\(_, letter, color) -> changeColorTo color letter) colorPattern

-- to say that it is from chatGpt
changeColorTo :: Color -> Char -> [Char]
changeColorTo wantedColor letter
    | wantedColor == Green     = greenColor ++ [letter] ++ whiteColor
    | wantedColor == Yellow    = yellowColor ++ [letter] ++ whiteColor
    | wantedColor == Gray      = grayColor ++ [letter] ++ whiteColor
    | otherwise                = [letter]
    