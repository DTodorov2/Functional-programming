import Prelude hiding (length, foldr , foldl, reverse, init, product, zip, zipWith)

-- Намира дължината на списъка xs
-- Версия без foldr\l
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

-- Версия с foldr\l
-- length1 :: [a] -> Int
-- length1 (x:xs)= foldr (\x -> (1+)) 0 xs

-- >>> length [1,2,3]
-- 3

-- Проверява дали съществува елемент в xs, за който е изпълнен предикатът p (мислете any).
exists :: (a -> Bool) -> [a] -> Bool
exists p xs = not (null [x | x <- xs, p x])

-- >>> exists even [1,3]
-- False

-- Проверява дали предикатът p е изпълнен за всички елементи на списъка xs (мислете all).
forAll :: (a -> Bool) -> [a] -> Bool
forAll p xs = (length [x | x <- xs, p x]) == length xs

-- >>> forAll even [2,4,1]
-- False

-- Проверява дали елементът x се съдържа в списъка xs (какво значи "съдържа"?).
member :: Eq a => a -> [a] -> Bool
member x xs = not (null [m | m <- xs, x == m])

-- >>> member 1 [1,2,3]
-- True

-- Добавя елемента x на края на списъка xs.
-- Версия без foldr
push :: a -> [a] -> [a]
push x [] = [x]
push el (x:xs) = x : push el xs

-- >>> push 1 [2,3]
-- [2,3,1]

-- Версия с foldr
-- push1 :: a -> [a] -> [a]
-- push1 el xs = foldr (:) [el] xs

-- >>> push1 1 [1,2,3]
-- [1,2,3,1]

-- Връща списък с елементите на xs в обратен ред.
reverse :: [a] -> [a]
reverse xs = helper xs [] where
    helper [] l1 = l1
    helper (el:list) empty = helper list (el:empty)

-- >>> reverse [1,2,3]
-- [3,2,1]

-- Връща списък с всички елементи на xs без последния.
init :: [a] -> [a]
init [b] = []
init (x:list) = x : init list

-- >>> init [1,2,3,3]
-- [1,2,3]

-- Вкарва елемента x на позиция n в списъка xs (ако n е след края на xs, вкарваме x накрая).
insert :: a -> Integer -> [a] -> [a]
insert x n [] = if n > 0 then [x] else []
insert x n (el:list)
    | n == 0         = x : insert x (n - 1) (el:list)
    | otherwise      = el : insert x (n - 1) list

-- >>> insert 5 6 [1,2,3]
-- [1,2,3,5]

-- Пресмята op xs[0] (op xs[1] (op xs[2] ... (op xs[n] init) ... ))) (ако имаме подаден празен списък, резултатът е init).
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op init [] = init
foldr op init (x:xs) = x `op` foldr op init xs

-- >>> foldr (+) 0 [1, 2, 3, 4]
-- 10

-- Пресмята op ( ... (op (op (op xs[0]) xs[1]) xs[2]) ... xs[n] ) init (ако имаме подаден празен списък, резултатът е init).
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl op init [] = init
foldl op init (x:xs) = foldl op (op init x) xs

concatList :: [[a]] -> [a]
concatList = foldl (++) []

-- >>> concatList [["Hello", " "], ["World", "!"]]
-- ["Hello"," ","World","!"]

-- Намира произведението на елементите в списък от числа.
product :: [Integer] -> Integer
product xs = foldr (*) 1 xs

-- >>> product [1,2,3]
-- 6

-- "зип"-ва два списъка заедно (в наредени двойки).
zip :: [a] -> [b] -> [(a, b)]
zip [] l2 = []
zip l1 [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- >>> zip [1,2,3] ["a", "b", "c"]
-- [(1,"a"),(2,"b"),(3,"c")]

-- "зип"-ва два списъка заедно, използвайки комбинираща функция.
zipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith op [] l2 = []
zipWith op l1 [] = []
zipWith op (x:xs) (y:ys) = (op x y) : zipWith op xs ys

-- >>> zipWith (+) [1, 2, 3] [2, 4, 6]
-- [3,6,9]

-- Списък, получен от zig-zag-ването на два други такива.
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave _ _ = error "Lists must be of equal size"

-- >>> interleave [1,2,3] [4,5,6]
-- [1,4,2,5,3,6]

-- Списък от всички естествени числа.
-- nats :: [Integer]
-- nats = [1..]

-- >>> take 10 nats
-- [1,2,3,4,5,6,7,8,9,10]

-- Списъкът от всички питагорови тройки (ползвайте list comprehension).
pythagoreanTriples :: (Integral a) => [(a, a, a)]
pythagoreanTriples = [(x, y, z) | z <- [1 ..], y <- [1 .. z], x <- [1 .. y], x^2 + y^2 == z^2]

-- >>> take 5 (pythagoreanTriples)
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]

-- Списък от всички числа от редицата на Фибоначи.
fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

-- >>> take 8 fibs
-- [0,1,1,2,3,5,8,13]

-- Списъкът от всички прости чиста, позовавайки се на "решетото на Ератостен".

primes :: (Integral a) => [a]
primes = sieve [2 ..] where 
    sieve (x:xs) = x : sieve [z | z <- xs, z `mod` x /= 0]
-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]
