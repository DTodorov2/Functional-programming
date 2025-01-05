module EasyGame where

import System.IO (hFlush, stdout)
import NormalGame (colorTheLetters)
import HelperMode ( getColorMatching, getWordsWithGrayLetters )
import Data.List (nub)
import System.Exit (exitSuccess)

type GrayLetters = [(Int, Char, [Char])]
type YellowLetters = [(Int, Char, [Char])]
type GreenLetters = [(Int, Char, [Char])]

data GameState = GameState {usedGrayLetters :: GrayLetters, usedYellowLetters :: YellowLetters, usedGreenLetters :: GreenLetters}

emptyGameState :: GameState
emptyGameState = GameState {usedGrayLetters = [], usedYellowLetters = [], usedGreenLetters = []}

checkGuessIsNotInDictionary :: (Foldable t, Eq a) => a -> t a -> IO Bool
checkGuessIsNotInDictionary guess dictionary = 
    if guess `notElem` dictionary
    then do
        putStrLn "\x1b[31mДумата, която предлагаш, я няма в речника!\x1b[0m"
        return True
    else
        return False

newGrayLettersList :: [(a, b, String)] -> [(a, b, String)]
newGrayLettersList colorPattern = [(ind, letter, "g") | (ind, letter, "g") <- colorPattern]

newYellowLettersList :: [(a, b, String)] -> [(a, b, String)]
newYellowLettersList colorPattern = [(ind, letter, "y") | (ind, letter, "y") <- colorPattern]

newGreenLettersList :: [(a, b, String)] -> [(a, b, String)]
newGreenLettersList colorPattern =  [(ind, letter, "gr") | (ind, letter, "gr") <- colorPattern]

updateGameState :: [Char] -> [Char] -> GameState -> GameState
updateGameState guess actualWord gameState =
    let colorMatchingPattern = getColorMatching guess actualWord
    in GameState {
        usedGrayLetters = nub (usedGrayLetters gameState ++ newGrayLettersList colorMatchingPattern),
        usedYellowLetters = nub (usedYellowLetters gameState ++ newYellowLettersList colorMatchingPattern),
        usedGreenLetters = nub (usedGreenLetters gameState ++ newGreenLettersList colorMatchingPattern)
        }

-- TO DO: to add dict to be filled only with words with length equal to the length of actualWord
startGameEasyMode :: Foldable t => String -> t [Char] -> IO ()
startGameEasyMode actualWord = evaluateGuessEasyMode actualWord (length actualWord) emptyGameState


askForConfirmation :: [Char] -> IO Bool
askForConfirmation word = do
    putStrLn ("Искаш ли да продължиш с дума '" ++ word ++ "' - y/n?")
    answer <- getLine
    if answer == "y"
        then return True 
        else if answer == "n"
            then return False 
            else do
                putStrLn "\x1b[31mОтговорът трябва да е y или n\x1b[0m"
                askForConfirmation word

confirmWordUsage :: Foldable t => [Char] -> GameState -> t [Char] -> IO Bool
confirmWordUsage guess gameState dict = do
    notInDict <- checkGuessIsNotInDictionary guess dict 
    if notInDict
        then 
            askForConfirmation guess
        else do
            if null (getWordsWithGrayLetters [guess] (usedGreenLetters gameState ++ usedYellowLetters gameState ++ usedGrayLetters gameState))
                then do
                    print "printq izpolzvani sivi bukvi"
                    print (usedGrayLetters gameState)
                    print "printq izpolzvani zeleni bukvi"
                    print (usedGreenLetters gameState)
                    print "printq izpolzvani julti bukvi"
                    print (usedYellowLetters gameState)
                    putStrLn "\x1b[31mДаваш дума, която противоречи с предишни отговори!\x1b[0m"
                    askForConfirmation guess
                else return True

evaluateGuessEasyMode :: Foldable t => String -> Int -> GameState -> t [Char] -> IO ()
evaluateGuessEasyMode actualWord wordLength gameState dict = do
    putStr "Моля, въведете опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    print guess
    print actualWord
    if guess == actualWord
        then do
            putStrLn ("\x1b[32mБраво! Позна думата " ++ actualWord ++ "!\x1b[0m")
            exitSuccess
        else do
            continue <- confirmWordUsage guess gameState dict
            if not continue
                then evaluateGuessEasyMode actualWord wordLength gameState dict
                else if length guess /= wordLength
                    then do
                        putStrLn ("\x1b[31mДумата трябва да бъде с дължина " ++ show wordLength ++ "\x1b[0m")
                        evaluateGuessEasyMode actualWord wordLength gameState dict
                    else do
                        putStrLn (colorTheLetters guess actualWord)
                        evaluateGuessEasyMode actualWord wordLength (updateGameState guess actualWord gameState) dict
