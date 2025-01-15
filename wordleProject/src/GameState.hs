module GameState ( updateGameStateEasyMode, getFiltersCount, updateGameState, 
                 GameState(greenLetters, yellowLetters, grayLetters), getAllFilters, emptyGameState) where

import Data.List ( nub )
import Color ( Color(..) )
import ColorUtils ( getColorMatching )
import ListUtils ( makeMapForLetterAndCountInWord, emptyList, getLettersFromColor )

type GrayLetters = [(Int, Char, Color)]
type YellowLetters = [(Int, Char, Color)]
type GreenLetters = [(Int, Char, Color)]

data GameState = GameState {
    grayLetters :: GrayLetters,
    yellowLetters :: YellowLetters,
    greenLetters :: GreenLetters
    }

getAllFilters :: GameState -> [(Int, Char, Color)]
getAllFilters gameState =
    grayLetters gameState ++
    yellowLetters gameState ++
    greenLetters gameState

emptyGameState :: GameState
emptyGameState = GameState [] [] []

filterLettersByColor :: Color -> [(Int, Char, Color)] -> [(Int, Char, Color)]
filterLettersByColor color = filter (\(_, _, c) -> c == color)

newGrayLettersList :: [(Int, Char, Color)] -> [(Int, Char, Color)]
newGrayLettersList = filterLettersByColor Gray

newYellowLettersList :: [(Int, Char, Color)] -> [(Int, Char, Color)]
newYellowLettersList = filterLettersByColor Yellow

newGreenLettersList :: [(Int, Char, Color)] -> [(Int, Char, Color)]
newGreenLettersList =  filterLettersByColor Green

updateYellowLetters :: [(Int, Char, Color)] -> [(Char, Int)] -> [(Int, Char, Color)] -> [(Int, Char, Color)]
updateYellowLetters _ [] newList = newList
updateYellowLetters yellowLettersGameState (pair:restMapCountYellowLetter) newList =
    let letter = fst pair
        count = snd pair
        countLetterInGameState = length [ char | (_, char, _) <- yellowLettersGameState, char == letter]
    in  if countLetterInGameState < count
        then updateYellowLetters yellowLettersGameState restMapCountYellowLetter 
                (newList ++ replicate count (-1, letter, Yellow))
        else updateYellowLetters yellowLettersGameState restMapCountYellowLetter 
                (newList ++ replicate countLetterInGameState (-1, letter, Yellow))

updateGameStateEasyMode :: [Char] -> [Char] -> GameState -> GameState
updateGameStateEasyMode guess actualWord gameState =
    let colorPattern = getColorMatching guess actualWord
        yellowLettersForMap = getLettersFromColor Yellow colorPattern
        mapForLetterAndCountInWord = (makeMapForLetterAndCountInWord yellowLettersForMap)
    in  GameState {
        grayLetters = nub (grayLetters gameState ++ newGrayLettersList colorPattern),
        yellowLetters = updateYellowLetters (yellowLetters gameState) mapForLetterAndCountInWord emptyList,
        greenLetters = nub (greenLetters gameState ++ newGreenLettersList colorPattern)
        }

-- Using nub here just for optimization (if several times someone tells me the same filters, not to iterate through every single one)
updateGameState :: [Char] -> [Char] -> GameState -> GameState
updateGameState guess actualWord gameState =
    let colorMatchingPattern = getColorMatching guess actualWord
    in GameState {
        grayLetters = nub (grayLetters gameState ++ newGrayLettersList colorMatchingPattern),
        yellowLetters = nub (yellowLetters gameState ++ newYellowLettersList colorMatchingPattern),
        greenLetters = nub (greenLetters gameState ++ newGreenLettersList colorMatchingPattern)
        }

getFiltersCount :: GameState -> Int
getFiltersCount gameState =
    length [ letter | (_, letter, _) <- yellowLetters gameState ++ greenLetters gameState]


