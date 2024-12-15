import Data.List (nub)

-- Да се дефинира функцията find xs key, която приема асоциативен списък xs и ключ key и връща стойността в xs асоциирана със key. 
-- Ако key не се среща в xs върнете грешка

find1 :: Eq p => [(p, b)] -> p -> (p, b)
find1 xs key =
    let keys = filter (\(x, y) -> x == key) xs
    in if not (null keys) then head keys else error "no such key exists"

-- >>> find1 [(1,2), (2,3), (4,5)] 5
-- no such key exists
-- Да се дефинира функцията groupBy f xs, която групира елементите на списъка xs на база стойността на f за всеки от тях.
groupBy1 :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy1 _ [] = []
groupBy1 f xs = (f (head xs), filter (\x -> f x == f (head xs)) xs) : groupBy1 f (filter (\x -> f x /= f (head xs)) xs)

-- >>> groupBy (\x -> x `mod` 3) [1,2,3,4]
-- [(1,[1,4]),(2,[2]),(0,[3])]

type Subject = String   -- Име на предмет
type Student = String   -- Име на студент
type Exam = Int         -- Идентификатор на даден изпит
type Note = Double      -- Оценка: число от 2 до 6
type Record = (Subject, Student, Exam, Note)


-- функция, която връща името на студента с най-висок среден успех.

topOfClass [] = []
topOfClass xs = fst (foldl1 (\(student,grades) (student1,grades1) -> if avg (getOnlyGrades grades) > avg (getOnlyGrades grades1) then (student,grades) else (student1,grades1)) (haha xs))


hardestExam xs = fst (foldl1 (\(ex,grades) (ex1,grades1) -> if avg (getOnlyGrades grades) < avg (getOnlyGrades grades1) then (ex,grades) else (ex1,grades1)) (haha1 xs))
haha1 xs = groupBy1 (\(_,_,exam,_) -> exam) xs
haha xs = groupBy1 (\(_,student,_,_) -> student) xs

perfectScorers xs sub = foldl (\nv (name, grades) -> if avg (getOnlyGrades grades) == 6.0 then name:nv else nv) [] (haha2 xs sub)
-- >>> perfectScorers [("haha", "Ivan", 1,2.3), ("nene", "Ivan", 1,6), ("dada", "Nikplaj", 1,2.3), ("nene", "Nikolaj", 1,6.0)] "nene"
-- ["Nikolaj","Ivan"]

haha2 xs subj = groupBy1 (\(_,student,_,_) -> student) (filter (\(sub,_,_,_) -> sub == subj) xs)
-- >>> haha2 [("haha", "Ivan", 1,2.3), ("nene", "Ivan", 1,5.7), ("dada", "Nikplaj", 1,2.3), ("nene", "Nikolaj", 1,2.3)] "nene"
-- [("Ivan",[("nene","Ivan",1,5.7)]),("Nikolaj",[("nene","Nikolaj",1,2.3)])]

avg :: (Fractional a, Foldable t) => t a -> a
avg xs = sum xs / fromIntegral (length xs)
-- >>> avg [2.3,5.7]
-- Variable not in scope: avg :: [a0_a150Q[tau:1]] -> t_a150N[sk:1]
getOnlyGrades :: Foldable t => t (a1, b, c, a2) -> [a2]
getOnlyGrades = foldr (\(_,_,_,grade) l -> grade : l) []
-- >>> getOnlyGrades [("haha","Ivan",1,2.3),("nene","Ivan",1,5.7)]
-- [2.3,5.7]

-- >>> topOfClass [("haha", "Ivan", 1,2.3), ("nene", "Ivan", 1,5.7), ("dada", "Nikplaj", 1,10.3), ("nene", "Nikolaj", 1,2.3)]
-- "Nikplaj"

-- >>> hardestExam [("haha", "Ivan", 2,2.3), ("nene", "Ivan", 1,5.7), ("dada", "Nikplaj", 1,10.3), ("nene", "Nikolaj", 2,2.3)]
-- 2

-- GRAPHS



buildGraph nodes edges = [(x, getEdges x edges) | x <- nodes]

getEdges node = foldr (\(x,y) l -> if x == node then y:l else if y == node then x:l else l) []

-- >>> getEdges 1 [(1,1),(1,2),(2,2),(3,1),(3,4)]
-- [1,2,3]

-- >>> buildGraph [1,2,3] [(1,1),(1,2),(2,2),(1,3),(3,4)]
-- [(1,[1,2,3]),(2,[1,2]),(3,[1,4])]

getReachable gr node = [node] ++ (snd (head (filter (\(x, y) -> x == node) gr)))
-- >>> getReachable [(1,[1,2,3]),(2,[1,2]),(3,[1,4])] 1
-- [1,1,2,3]

-- >>> connectedComponents [(1,[1,2,3]),(2,[1,2]),(3,[1,4]), (5, [6])]
-- Prelude.head: empty list

contains [] _ = False
contains (x:xs) ys = (x `elem` ys) || contains xs ys

-- >>> contains [2] [1,2,3]
-- False

type Graph = [(Node, [Node])]
type Node = Int

-- Главната функция, която намира всички двойки върхове на разстояние k


nodesAtDistance :: Graph -> Int -> [(Node, Node)]
nodesAtDistance graph k = concatMap (\node -> bfs graph node k) (nodes graph)

-- Извлича всички върхове от графа
nodes :: Graph -> [Node]
nodes = map fst

-- BFS функция, която намира всички двойки върхове на разстояние k



bfs :: Graph -> Node -> Int -> [(Node, Node)]
bfs graph start k = bfsHelper graph [(start, 0)] [] k (fst (head graph))

-- Рекурсивна помощна функция за BFS


bfsHelper :: Graph -> [(Node, Int)] -> [Node] -> Int -> t -> [(t, Node)]
bfsHelper _ [] _ _ _= []  -- Ако няма повече възли в опашката, спираме
bfsHelper graph ((node, dist):queue) visited k root
  | dist == k = [(root, node)]  -- Ако разстоянието е точно k, връщаме двойки
  | dist < k  = bfsHelper graph (queue ++ neighbors graph node dist) (node:visited) k root -- Продължаваме за разстояния по-малки от k
  | otherwise = []  -- Ако сме преминали разстоянието k, спираме рекурсията

-- Намира всички съседи на даден възел
neighbors :: Graph -> Node -> Int -> [(Node, Int)]
neighbors graph node dist = case lookup node graph of
  Just ns -> map (\n -> (n, dist + 1)) ns  -- Увеличаваме dist с 1
  Nothing -> []

-- >>> bfs [(1, [2, 3]), (2, [1, 4]), (3, [1]), (4, [2])] 1 3
-- [(1,2)]

