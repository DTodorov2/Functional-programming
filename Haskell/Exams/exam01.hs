data Tree = Empty | Node Int [Tree]
    deriving (Show)

prune :: Tree -> Tree
prune Empty = Empty
prune (Node v children) = Node v [t | t <- children, pred v t] where
    pred parent Empty = False
    pred parent (Node v _) = gcd parent v == 1

tree :: Tree
tree =
  Node
    10
    [ Node 3 [Empty, Node 3 [], Node 2 [Empty, Empty]],
      Node 7 [Node 14 [], Node 2 [], Node 1 [Empty], Node 15 [Empty]],
      Node 20 [Node 9 [Node 10 []]]
    ]

result :: Tree
result =
  Node
    10
    [ Node 3 [Node 2 []],
      Node 7 [Node 2 [], Node 1 [], Node 15 []]
    ]

-- >>> prune tree
-- Node 10 [Node 3 [Empty,Node 3 [],Node 2 [Empty,Empty]],Node 7 [Node 14 [],Node 2 [],Node 1 [Empty],Node 15 [Empty]]]

-- Да се реализира функция mergeFromMaybes, която приема два безкрайни списъка с елементи от тип Maybe a, 
-- слива ги и връща нов безкраен списък с елементи от тип a.

mergeFromMaybes :: [Maybe a] -> [Maybe a] -> [a]
mergeFromMaybes xs ys = [v | [Just v] <- zipWith pred xs ys] where 
    pred (Just v1) _  = [Just v1]
    pred Nothing (Just v2)  = [Just v2]
    pred _ _ = [Nothing]

toMaybes :: (a -> Bool) -> [a] -> [Maybe a]
toMaybes p = map (\x -> if p x then Just x else Nothing)

s1 :: [Maybe Int]
s1 = toMaybes even [1..]

s2 :: [Maybe Int]
s2 = toMaybes odd [1..]
-- >>> take 10 (mergeFromMaybes s1 s2)
-- [1,2,3,4,5,6,7,8,9,10]
