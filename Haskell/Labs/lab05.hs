import Prelude hiding (reverse, init, product, zip, zipWith, takeWhile, concatMap)
import Data.Char (isAlpha, isDigit)
zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip (x:xs) (y:ys) = (x, y): zip xs ys
zip _ _ = []
-- >>> take 50 $ zip [1..] ['a'..'z']
-- [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j'),(11,'k'),(12,'l'),(13,'m'),(14,'n'),(15,'o'),(16,'p'),(17,'q'),(18,'r'),(19,'s'),(20,'t'),(21,'u'),(22,'v'),(23,'w'),(24,'x'),(25,'y'),(26,'z')]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] [] = []
zipWith f (x:xs) (y:ys) = f x y: zipWith f xs ys
zipWith _ _ _ = []

-- >>> zipWith (+) [1,2,3] []
-- []

takeWhile :: (a -> Bool) -> [a] -> [a]
take p [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

-- >>> takeWhile (<5) [0..]
-- [0,1,2,3,4]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = [y | x <- xs, y <- f x]
-- >>> concatMap (\x -> [x,-x]) [1,2,3]
-- [1,-1,2,-2,3,-3]

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf (x:xs) ys = elem x ys && isInfixOf xs ys

-- >>> isInfixOf [3..5] [1..2]
-- False

maximumBy :: (a -> a -> Bool) -> [a] -> a
--


maximumBy f (x:xs) = foldl (\x y -> if f x y then x else y) x xs

-- >>> maximumBy (\x y -> snd x > snd y) [(4, 1), (5, 2), (0, 4), (2, 0), (3, 3)]
-- (0,4)

removeConsequtive :: Eq a => [a] -> [a]
removeConsequtive [] = []
removeConsequtive (x:xs) = x : removeConsequtive (dropWhile (== x) xs)

-- >>> removeConsequtive [1, 2, 2, 3, 3, 3, 2, 4]
-- [1,2,3,2,4]

pairSum :: Int -> [Int] -> [(Int, Int)]
pairSum sum xs = [(x, y) | x <- xs, y <- xs, x <= y , x + y == sum]

-- >>> pairSum 6 [1..10]
-- [(1,5),(2,4),(3,3)]

leftPad :: [a] -> Int -> a -> [a]
leftPad xs num ch = helper xs (num - length xs) ch where
    helper list newNum ch
        | newNum <= 0       = list
        | otherwise         = helper (ch: list) (newNum - 1) ch
-- >>> leftPad "Hello" 8 '+'
-- "+++Hello"

join :: [[Char]] -> Char -> [Char]
--join xs ch = drop 1 (foldr (++) [] (map (ch:) xs))
join [y] ch = y
join (x:xs) ch = x ++ [ch] ++ join xs ch

-- >>>  join ["Hello", "World", "My", "Friend"] ' '
-- "Hello World My Friend"

decode :: [Char] -> [Char]
decode [] = []
decode (x:xs)
    | isAlpha x     = x : decode xs
    | isDigit x     = let lengthNum = read (x : takeWhile isDigit xs) in replicate lengthNum (head (dropWhile isDigit xs)) ++ decode (dropWhile isDigit xs)

-- >>> decode "a12bd5c"
-- "abbbbbbbbbbbbbdcccccc"

groupBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupBy f [] = []
groupBy f xs = helper [] f xs where
    helper newList _ [] = newList
    helper newList func list = helper ((func (head list), filter (\x -> func x == func (head list)) list) : newList) func (filter (\x -> func x /= func (head list)) list)
-- >>> groupBy length [[1, 2, 3], [1], [7, 2], [0], [8, 1, 6]]
-- [(2,[[7,2]]),(1,[[1],[0]]),(3,[[1,2,3],[8,1,6]])]
