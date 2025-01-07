module ListUtils where
import Data.List ( group, sort )

-- Constants
initialValue :: Int
initialValue = 0

indexToStartCountingFrom :: Int
indexToStartCountingFrom = 1

emptyList :: [a]
emptyList = []

isValidIndex :: Foldable t => Int -> t a -> Bool
isValidIndex index word = index >= 1 && index - 1 <= length word

removeLetterFromList :: Eq a => [a] -> a -> [a] -> [a]
removeLetterFromList [] _ newList = newList
removeLetterFromList (x:yellowLettersList) letter newList =
    if x == letter
        then removeLetterFromList yellowLettersList letter newList
        else removeLetterFromList yellowLettersList letter (x : newList)

makeMapForLetterAndCountInWord :: Ord a => [a] -> [(a, Int)]
makeMapForLetterAndCountInWord secretWord =
    let groupedLetters = group (sort secretWord)
    in map (\group -> (head group, length group)) groupedLetters
