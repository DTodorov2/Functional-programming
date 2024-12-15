-- myAbs n изчислява модул от n
myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

-- isTriangle a b c, която връща дали числата a, b и c образуват валиден триъгълник
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = (a + b > c) && (a + c > b) && (b + c > a)
-- >>> isTriangle 3 4 5
-- True

-- countDays day month year, която приема ден, месец и година и връща броя на дните от началото на годината до съответния ден.
countDays :: Int -> Int -> Int -> Int
countDays day month year = sum (take (month - 1) daysInMonth) + day
    where
        isLeapYear :: Int -> Bool
        isLeapYear year = (mod year 4 == 0 && mod year 100 /= 0) || (mod year 400 == 0)
        daysInMonth = [31, if isLeapYear year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- >>> countDays 15 3 2023
-- 74

-- isPrime n, която връща дали числото n е просто.
isPrime :: Int -> Bool
isPrime n = (n > 1) && iter 2 where
    iter i
        | i * i > n      = True
        | n `mod` i == 0 = False
        | otherwise      = iter (i + 1)

-- >>> isPrime 3
-- True

-- isPrime със списък
isPrimeList :: Int -> Bool
isPrimeList n
    | n < 2 = False
    | otherwise = null [x | x <- [2 .. floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- >>> isPrimeList 4
-- False

-- sumDivisors n, която връща сбора на собствените делители на числото n.
sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. (n - 1)], n `mod` x == 0]

-- >>> sumDivisors 20
-- 22

-- sumDivisors second attempt
sumDivisorsSec :: Integer -> Integer
sumDivisorsSec n = iter 1 0 where
    iter j res
        | j == n            = res
        | n `mod` j == 0    = iter (j + 1) (res + j)
        | otherwise         = iter (j + 1) res

-- >>> sumDivisorsSec 20
-- 22

-- isPerfect n, която връща дали n е съвършено число.
isPerfect :: Integer -> Bool
isPerfect n = n == sumDivisorsSec n

-- >>> isPerfect 28
-- True

-- countBinaryDigits n, която връща броя на цифрите в двоичната репрезентация на n.
countBinaryDigits :: Integer -> Integer
countBinaryDigits n = iter n 0 where
    iter i count
        | i == 0            = count
        | otherwise    = iter (i `div` 2) (count + 1)

-- >>> countBinaryDigits 10
-- 4

-- isEvil n, която връща дали числото n е зло ("evil"). Наричаме едно число зло ако броя на битовете, които са 1 е четен.
isEvil :: Integer -> Bool
isEvil n = even (iter n 0) where
    iter i count
        | i == 0         = count
        | i `mod` 2 == 1 = iter (i `div` 2) (count + 1)
        | otherwise      = iter (i `div` 2) count

-- >>> isEvil 8
-- False

-- sumEvil a b, която връща сбора на злите числа в интервала [a .. b].
sumEvil :: Integer -> Integer -> Integer
sumEvil a b = sum [x | x <- [a .. b], isEvil x]

-- >>> sumEvil 0 100
-- 2475

-- compose f g x, която връща композицията на две едноместни функции f и g.
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- >>> compose (+1) (+1) 3
-- 5


