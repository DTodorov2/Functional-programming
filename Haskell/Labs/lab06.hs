
-- Напишете функция, която пресмята факториел
factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial 1 = 1
factorial n = n* factorial (n - 1)

-- >>> factorial 5
-- 120

-- Напишете функция, която приема 3 аргумента - коефициентите на квадратно уравнение. 
-- Функцията да връща броя на корените на квадратното уравнение.

roots :: Int -> Int -> Int -> Int
roots a b c =
    let l = (b^2 - 4*a*c)
    in if l > 0 then 2 else if l == 0 then 1 else 0

-- >>> roots 1 2 2
-- 0

-- Напишете функция, която по подадена едноместна функция и естествено число n, връща n-кратната композиция на подадената функция.

func :: (Eq t, Num t) => (b -> b) -> t -> b -> b
func f num
    | num == 0  = id
    | otherwise = f . func f (num - 1)

-- >>> (func succ 6) 7
-- 13

-- Напишете функция, която приема 2 естествени числа - a и b. Функцията да намира сумата на всички числа в интервала [a, b], 
-- които са точни квадрати.

sumSquares :: Int -> Int -> Int
sumSquares a b = sum [x * x| x <- [a .. floor (sqrt (fromIntegral b))]]

-- >>> sumSquares 1 36
-- 91

modulus :: (Int, Int) -> Int
modulus (x, y) = floor (sqrt (fromIntegral x* fromIntegral x + fromIntegral y* fromIntegral y))

-- >>> modulus (3, 4)
-- 5

-- Напишете функция, която приема наредена тройка от две цели числа и едно десетично число. 
--Функцията трябва да раздели третото число на първото и да добави към резултата второто. Aко деленето е невъзможно да се върне резултат 0

compute :: (Int, Int, Float) -> Float
compute (0, _, _) = 0.0
compute (x, y, z) = z / fromIntegral x + fromIntegral y

-- >>> compute (3, 4, 6.9)
-- 6.3

-- Напишете функция, която по подадено естествено число, проверява дали сумата от делителите му (без самото число) е равна на самото число.
magical :: Int -> Bool
magical num = (sum [x | x <- [1 .. num - 1], num `mod` x == 0]) == num

-- >>> magical 12
-- False

($$) :: Int -> Int -> Int
($$) a b = a + b + lcm a b

-- >>> 6 $$ 4
-- 22

-- Напишете функция accumulate, която прави същото като функциятa accumulate, която реализирахме на Scheme. 
-- Помислете как да я типизирате, така че да може да връща резултат от произволен тип.

-- accumulate :: (a -> b) -> a -> Int -> Int -> (Int -> Int) -> (Int -> Int)

accumulate :: Ord t1 => (t2 -> t3 -> t3) -> t3 -> t1 -> t1 -> (t1 -> t2) -> (t1 -> t1) -> t3
accumulate op nv x y term next = if x > y then nv else term x `op` accumulate op nv (next x) y term next

-- >>> accumulate (+) 0 1 10 id succ
-- 55

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) subs ++ subs where
    subs = subsets xs

-- >>> subsets [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations lst = [x:xs | x <- lst, xs <- permutations (filter (/= x) lst)]
-- >>> permutations [1, 2, 3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
