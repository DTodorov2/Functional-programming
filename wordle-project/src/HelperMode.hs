module HelperMode where

getInitialDictionary wordLength dict = [ word | word <- dict, length word == wordLength]

getListOfColors [] listOfColors _ _ = return listOfColors
getListOfColors (x:xs) listOfColors ind wordLength
  | length (x:xs) /= wordLength   = do
    putStr "Думата не е с коректна дължина! Опитай пак: "
    return []
  | otherwise = do
                putStrLn "Напиши комбинацията от букви, която отговаря на съответния цвят: (gr - green, y - yellow, g - gray):"
                putStr ("Цвят за буква " ++ [x] ++ ": ")
                color <- getLine
                if color `elem` ["gr", "y", "g"]
                    then if color == "gr" 
                        then getListOfColors xs ((ind, (x, color)) : listOfColors) (ind + 1) wordLength
                        else getListOfColors xs ((-1, (x, color)) : listOfColors) (ind + 1) wordLength
                    else do
                        putStrLn "Невалиден цвят!"
                        getListOfColors (x:xs) listOfColors ind wordLength

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
    in getWordsWithGreenLetters
        (getWordsWithYellowLetters 
         (getWordsWithGrayLetters dict grayLetters) 
        yellowLetters) 
       greenPairs

evaluateGuessHelper listOfColors dict wordLength =
    putStrLn ("Предложената дума е: " ++ head dict)
