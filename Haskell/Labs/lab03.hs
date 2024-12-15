import Data.Char (toUpper, toLower, isSpace, isLower,chr, ord)
import Data.List (sort)
-- Да се дефинира функцията whisper str, която обръща в малки букви всички символи на низа str.
whisper :: [Char] -> [Char]
whisper = map toLower

-- >>> whisper "bANA1NA"
-- "bana1na"

-- Да се дефинира функцията removeSpaces str, която премахва всички интервали от символния низ str.
removeSpaces :: [Char] -> [Char]
removeSpaces [] = []
removeSpaces (x:xs)
    | isSpace x     = removeSpaces xs
    | otherwise     = x : removeSpaces xs

-- >>> removeSpaces "The Sound And The Fury"
-- "TheSoundAndTheFury"

-- Напишете функцията switchCaps str, която обръща малките букви в големи, а големите в малки.
switchCaps :: [Char] -> [Char]
switchCaps [] = []
switchCaps (x:xs)
    | isLower x     = toUpper x : switchCaps xs
    | otherwise     = toLower x : switchCaps xs

-- >>> switchCaps "baNaNA"
-- "BAnAna"

-- Дефинирайте функциите encrypt n str и decrypt n str, които имплементират Цезаров шифър с отместване n.
encrypt :: Int -> [Char] -> [Char]
encrypt n = foldr (\x acc -> chr ( if ((ord x + n) `mod` 91) < 65 then ((ord x + n) `mod` 91) + 65 else ((ord x + n) `mod` 91)) : acc) []

-- >>> encrypt 1 "ABC"
-- "BCD"

decrypt :: Integer -> [Char] -> [Char]
decrypt n = map (\x -> chr ( if ((ord x - fromIntegral n) `mod` 91) < 65 then ((ord x - fromIntegral n) `mod` 91) + 26 else ((ord x - fromIntegral n) `mod` 91)))
-- >>> decrypt 2 "CZAB"
-- "AXYZ"

-- Напишете функцията joinWords c strs, която слива няколко думи в една, използвайки за разделител символ c.
joinWords :: Char -> [[Char]] -> [Char]
joinWords c = foldr (\x acc -> if null acc then x else x ++ c : acc) []
-- >>> joinWords ',' ["One", "Two", "Three", "Four"]
-- "One,Two,Three,Four"

-- Трансформация на Бъроус-Уилър
rotate :: Int -> [Char] -> [Char]
rotate i str = b ++ a where
    (a, b) = splitAt i str

transfrom :: [Char] -> [Char]
transfrom str = map last (sort rotations) where
    rotations = [rotate i str | i <- [0 .. length str - 1]]

-- >>> transfrom "bAnaNa"
-- "banNaA"

-- Дефинирайте функцията indices x xs, която връща всички индекси на елементи от списъка xs, чиято стойност е равна на x.
indices :: Eq a => a -> [a] -> [Int]
indices x xs = [i | i <- [0 .. length xs - 1], (xs !! i) == x]

-- >>> indices 1 []
-- []

-- Дефинирайте функцията lastIndex x xs, която приема 2 аргумента - елемент x и списък xs- и връща индекса (0-базиран) 
-- на последното срещане на x в xs. Ако x не се среща в xs, функцията връща грешка.
lastIndex :: Eq a => a -> [a] -> Int
lastIndex x xs = if null (indices x xs) then error "not in the list" else maximum (indices x xs)

-- >>> lastIndex 3 [2, 4, 7]
-- not in the list

-- Брой на повторенията на най-малкото число в списъка: дефинирайте функцията countMin xs, която намира броя на срещанията 
-- на най-малкия елемент на списъка xs в него. Списъкът xs е несортиран и e съставен само от положителни цели числа.
countMin :: [Int] -> Int
countMin [] = 0
countMin xs = length (indices (minimum xs) xs)

-- >>> countMin []
-- 0

-- Просто пренареждане: Дефинирайте функцията primeReorder xs, която получава списък xs и връща нов списък ys. 
-- В началото на ys трябва да са елементите, които са били с индекс просто число в xs. След тях трябва да са всички останали.
-- Индексирането в xs започва от 2.
isPrime :: Int -> Bool
isPrime n = iter 2 n where
    iter :: Int -> Int -> Bool
    iter i num
        | n < 2              = False
        | i * i > n          = True
        | num `mod` i == 0   = False
        | otherwise          = iter (i + 1) num

-- >>> isPrime 5
-- True

getPrimesElements :: Eq a => [a] -> [a]
getPrimesElements xs = [xs !! (i - 2) | i <- [2 .. length xs - 1 + 2], isPrime i]

-- >>> getPrimesElements [2,3,4,5,6]
-- [2,3,5]

getNonPrimesElements :: Eq a => [a] -> [a]
getNonPrimesElements xs = [xs !! (i - 2) | i <- [2 .. length xs - 1 + 2], not (isPrime i)]

-- >>> getNonPrimesElements [2,3,4,5,6]
-- [4,6]

primeReorder :: Eq a => [a] -> [a]
primeReorder xs = getPrimesElements xs ++ getNonPrimesElements xs

-- >>> primeReorder "abcd"
-- "abdc"

-- Да се дефинира функция dedup xs, която премахва всички дубликати в xs.

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : dedup (filter (/= x) xs)
-- >>> dedup [1, 3, 7, 3, 5, 1]
-- [1,3,7,5]

-- Нaпишете функцията merge xs ys, която приема два списъка подредени в нарастващ ред 
-- и ги обединява в един списък, чийто елементи също са подредени в нарастващ ред.

merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- >>> merge [1, 3, 7] [2, 4, 6]
-- [1,2,3,4,6,7]

-- Използвайте функцията от предишната подточка и идеята, за да напишете функция mergesort xs, 
-- която приема списък xs и връща списък с елементите на xs сортирани в нарастващ ред.
mergesort :: (Eq a, Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right) where
    (left, right) = splitAt (length xs `div` 2) xs

-- >>> mergesort [2, 1, 3, 7, -16, 5] 
-- [-16,1,2,3,5,7]

-- Дефинирайте функцията subsets xs, която връща списък с всички подсписъци на списъка xs.
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

-- >>> subsets [1, 2, 3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-- Дефинирайте функцията pick k xs, която връща списък с всички възможни избора на k елемента от списъка xs.
pick :: Int -> [а] -> [[а]]
pick k xs = [x | x <- subsets xs, length x == k]

-- >>> pick 2 [1, 2, 3]
-- [[1,2],[1,3],[2,3]]

-- Напишете функцията maximize, която получава непразен списък от едноместни числови функции и връща нова едноместна 
-- числова функция на аргумент x, която дава стойността f x на тази фунция f от списъка, за която числото f x е най-голямо
-- по абсолютна стойност.
maximize :: (Ord b) => [a -> b] -> a -> b
maximize fs x = maximum [f x | f <- fs]

-- >>> maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5 
-- 1.5

-- Напишете функцията compose fs, която приема списък от едноаргументни функцуии и връща тяхната композиция, 
-- т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- >>> compose [(+1), (+1), (+1)] 7
-- 10

-- Дефинирайте безкрайния списък facts, който съдържа стойностите на n! за всяко n.
fact :: Int -> Int
fact n = helper 1 n where
    helper :: Int -> Int -> Int
    helper acc n1
        | n1 == 1 || n1 == 0    = acc
        | otherwise = helper (acc * n1) (n1 - 1)

facts :: [Integer]
facts = [fromIntegral (fact x) | x <- [0 ..]]
-- >>> take 10 facts
-- [1,1,2,6,24,120,720,5040,40320,362880]

-- Дефинирайте безкрайния списък points, който съдържа всички точки в равнината, чиито координати са цели числа.
points :: [(Int, Int)]
points = [(x,y) | x <- [0 ..], y <- [0 ..]]
-- >>> take 10 points
-- [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9)]
