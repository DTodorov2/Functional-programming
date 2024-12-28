module EasyGame (startGameEasyMode) where

import System.IO (hFlush, stdout)
import NormalGame (colorTheLetters)

type GrayLetters = [Char]
type YellowLetters = [Char]
type GreenLetters = [(Int, Char)]

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

newGrayLettersList :: (Eq a, Foldable t) => t a -> [(a, a)] -> [a]
newGrayLettersList _ [] = []
newGrayLettersList actualWord newPairsLetters = [f | (f, s) <- newPairsLetters, f /= s, f `notElem` actualWord]

newYellowLettersList :: (Eq a, Foldable t) => t a -> [(a, a)] -> [a]
newYellowLettersList _ [] = []
newYellowLettersList actualWord newPairsLetters = [f | (f, s) <- newPairsLetters, f /= s, f `elem` actualWord]

newGreenLettersList :: (Num a, Enum a, Eq b) => [(b, b)] -> [(a, b)]
newGreenLettersList [] = []
newGreenLettersList newPairsLetters = [(i, f) | (i, (f, s)) <- zip [0..] newPairsLetters,  f == s]

updateGameState :: [Char] -> [Char] -> GameState -> GameState
updateGameState guess actualWord gameState =
    let newPairsLetters = zip guess actualWord
    in GameState {
        usedGrayLetters = usedGrayLetters gameState ++ newGrayLettersList actualWord newPairsLetters,
        usedYellowLetters = usedYellowLetters gameState ++ newYellowLettersList actualWord newPairsLetters,
        usedGreenLetters = usedGreenLetters gameState ++ newGreenLettersList newPairsLetters
        }

checkGuessForUsingUsedGrayLetters :: (Foldable t, Eq a) => t a -> [a] -> IO Bool
checkGuessForUsingUsedGrayLetters _ [] = return False
checkGuessForUsingUsedGrayLetters grayLetters (x:xs) = if x `elem` grayLetters
    then do
        putStrLn "\x1b[31mИзползваш използвана сива буква!\x1b[0m"
        return True
    else
        checkGuessForUsingUsedGrayLetters grayLetters xs


checkGuessIsGreenLetterMisplaced :: [(Int, Char)] -> [Char] -> Int -> IO Bool
checkGuessIsGreenLetterMisplaced _ [] _ = return False
checkGuessIsGreenLetterMisplaced greenLetters (x:xs) index =
    let
        isThereGuessedLetter = filter (\(f, s) -> f == index && s /= x) greenLetters
    in if not (null isThereGuessedLetter)
        then do
            putStrLn ("\x1b[31mИзползваш неправилна буква на позиция " ++ show (index + 1) ++ "\x1b[0m")
            return True
        else checkGuessIsGreenLetterMisplaced greenLetters xs (index + 1)

checkMissedYellowLetters :: Eq a => [a] -> [a] -> IO Bool
checkMissedYellowLetters [] [] = return False
checkMissedYellowLetters _ [] = do
        putStrLn "\x1b[31mНе си използвал всички жълти букви!\x1b[0m"
        return True

checkMissedYellowLetters yellowLetters (x:xs) =
    if x `elem` yellowLetters
        then checkMissedYellowLetters (filter (\l -> l /= x) yellowLetters) xs
        else checkMissedYellowLetters yellowLetters xs

startGameEasyMode :: Foldable t => String -> t [Char] -> IO ()
startGameEasyMode actualWord = evaluateGuessEasyMode actualWord (length actualWord) emptyGameState

askForConfirmation :: IO Bool
askForConfirmation = do
    putStrLn "Искаш ли да продължиш? - y/n"
    answer <- getLine
    if answer == "y"
        then return True 
        else if answer == "n"
            then return False 
            else do
                putStrLn "\x1b[31mОтговорът трябва да е y или n\x1b[0m"
                askForConfirmation

confirmWordUsage :: Foldable t => [Char] -> GameState -> t [Char] -> IO Bool
confirmWordUsage guess gameState dict = do
    notInDict <- checkGuessIsNotInDictionary guess dict 
    if notInDict
        then askForConfirmation 
        else do 
            usingUsedGrayLetters <- checkGuessForUsingUsedGrayLetters (usedGrayLetters gameState) guess
            if usingUsedGrayLetters
                then askForConfirmation
                else do 
                    misplacedGreenLetter <- checkGuessIsGreenLetterMisplaced (usedGreenLetters gameState) guess 0
                    if misplacedGreenLetter
                        then askForConfirmation
                        else do 
                            missedYellowLetters <- checkMissedYellowLetters (usedYellowLetters gameState) guess
                            if missedYellowLetters
                                then askForConfirmation
                                else return True

evaluateGuessEasyMode :: Foldable t => String -> Int -> GameState -> t [Char] -> IO ()
evaluateGuessEasyMode actualWord wordLength gameState dict = do
    putStr "Моля, въведете опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    continue <- confirmWordUsage guess gameState dict
    if not continue
        then evaluateGuessEasyMode actualWord wordLength gameState dict
        else if length guess /= wordLength
            then do
                putStrLn ("\x1b[31mДумата трябва да бъде с дължина " ++ show wordLength ++ "\x1b[0m")
                evaluateGuessEasyMode actualWord wordLength gameState dict
            else if guess == actualWord
                then do
                    putStrLn ("\x1b[32mБраво! Позна думата " ++ actualWord ++ "!\x1b[0m")
                else do
                    putStrLn (colorTheLetters guess actualWord)
                    evaluateGuessEasyMode actualWord wordLength (updateGameState guess actualWord gameState) dict
