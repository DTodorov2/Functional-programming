module HelperMode where

getInitialDictionary wordLength dict = [ word | word <- dict, length word == wordLength]

getListOfColors (x:xs) listOfColors ind wordLength =
    if ind == wordLength - 1
        then return listOfColors
        else do
            putStrLn "Напиши комбинацията от букви, която отговаря на съответния цвят: (gr - green, y - yellow, g - gray):"
            putStr ("Цвят за буква " ++ [x] ++ ": ")
            color <- getLine
            if color == "gr"
                then getListOfColors xs ((ind, (x, color)) : listOfColors) (ind + 1) wordLength
                else getListOfColors xs ((-1, (x, color)) : listOfColors) (ind + 1) wordLength

getWordsWithGrayLetters dict [] = dict
getWordsWithGrayLetters dict grayLetters = [word | word <- dict, all (`notElem` word) grayLetters]

getWordsWithYellowLetters dict [] = dict
getWordsWithYellowLetters dict yellowLetters = [word | word <- dict, all (`elem` word) yellowLetters]

getWordsWithGreenLetters dict [] = dict
getWordsWithGreenLetters dict greenPairs = [word | word <- dict, all (\(ind, letter) -> word !! ind == letter) greenPairs]

getFilteredDict dict listOfColors =
    let greenPairs = [(ind, letter) | (ind, (letter, color)) <- listOfColors, color == "gr"]
        yellowLetters = [letter | (ind, (letter, color)) <- listOfColors, color == "y"]
        grayLetters = [letter | (ind, (letter, color)) <- listOfColors, color == "g"]
    in getWordsWithGreenLetters (getWordsWithYellowLetters (getWordsWithGrayLetters dict grayLetters) yellowLetters) greenPairs

evaluateGuessHelper listOfColors dict wordLength =
    putStrLn ("Предложената дума е: " ++ head dict)
