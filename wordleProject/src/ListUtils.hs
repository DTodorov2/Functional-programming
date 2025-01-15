module ListUtils where
import Data.List ( group, sort )
import Color

-- Constants
initialValue :: Int
initialValue = 0

indexToStartCountingFrom :: Int
indexToStartCountingFrom = 1

emptyList :: [a]
emptyList = []

isValidIndex :: Int -> [Char] -> Bool
isValidIndex index word = index >= 1 && index - 1 <= length word

removeLettersFromFirstList :: [Char] -> [Char] -> [Char]
removeLettersFromFirstList [] _ = []
removeLettersFromFirstList firstList [] = firstList
removeLettersFromFirstList firstList (x:restSecondList) = 
    removeLettersFromFirstList (removeLetterFromList firstList x) restSecondList

removeLetterFromList :: [Char] -> Char -> [Char]
removeLetterFromList yellowLettersList letter = filter (/= letter) yellowLettersList

makeMapForLetterAndCountInWord :: [Char] -> [(Char, Int)]
makeMapForLetterAndCountInWord secretWord =
    let groupedLetters = group (sort secretWord)
    in map (\group -> (head group, length group)) groupedLetters

-- Gets the count of a letter in the word from the list
getCountOfCurrentLetter :: Char -> [(Char, Int)] -> Int
getCountOfCurrentLetter _ [] = 0
getCountOfCurrentLetter targetLetter ((letter, count):restMapCountForSecretWord)
    | targetLetter == letter    = count
    | otherwise                 = getCountOfCurrentLetter targetLetter restMapCountForSecretWord

getIndexes :: [(Int, Char, Color)] -> [Int]
getIndexes list = [ ind | (ind, _, _) <- list]

getLettersFromColor :: Color -> [(Int, Char, Color)] -> [Char]
getLettersFromColor color colorPattern = 
     [letter | (_, letter, c) <- colorPattern, c == color]