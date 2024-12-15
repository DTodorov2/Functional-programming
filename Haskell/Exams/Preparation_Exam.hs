isNPerm :: Int -> (Int -> Int) -> Bool
isNPerm n func = helper [0 .. n - 1] func [0 .. n - 1] where
    helper [] _ [] = True
    helper (x:xs) func secRange
        | func x `elem` secRange    = helper xs func (filter (\y -> y /= func x) secRange)
        | otherwise                 = False

-- >>> isNPerm 10 (\x -> (x + 2) `mod` 10)
-- True

maxCycle :: Int -> (Int -> Int) -> [[Int]]
maxCycle n func = take 1 (filter (\r -> length r >= 2) [createCycle x (filter (/= x) [0 .. (n - 1)]) [x] func | x <- [0 .. (n - 1)]]) where
    createCycle elToLookAt range list f
        | (f elToLookAt `elem` list) && (f elToLookAt == head (reverse list))  = reverse list
        | f elToLookAt `elem` range = createCycle (f elToLookAt) (filter (\y -> y /= f elToLookAt) range) ((f elToLookAt) : list) f
        | otherwise = []

-- >>> maxCycle 3 (\x -> (3 - x) `mod` 3)
-- [[1,2]]

roundToTwoDecimalPlaces :: Float -> Float
roundToTwoDecimalPlaces x = fromIntegral (round (x * 10)) / 10


movingAverage :: Integral a => [a] -> Int -> [Float]
movingAverage xs num = roundToTwoDecimalPlaces (fromIntegral (sum (take num xs)) / fromIntegral num) : movingAverage (tail xs) num

allAverages xs = [take 5 (movingAverage xs n) | n <- [2 .. ]]
-- >>> take 5 (allAverages [1076, 1356, 1918, 6252, 6766, 5525])
-- [[1216.0,1637.0,4085.0,6509.0,6145.5],[1450.0,3175.3,4978.7,6181.0,4097.0],[2650.5,4073.0,5115.2,4635.8,3072.8],[3473.6,4363.4,4092.2,3708.6,2458.2],[3815.5,3636.2,3410.2,3090.5,2048.5]]

getFirstElements :: [([Char], [[Char]])] -> [[Char]]
getFirstElements = foldr (\(x, _) y -> x : y) []

-- >>> getFirstElements [("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []), ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"])]
-- ["docs","ids","invoices","memes","family","funny","pics"]

getAllBoxes :: [([Char], [[Char]])] -> [[Char]]
getAllBoxes = foldr (\(_, x) y -> x ++ y) []

-- >>> getAllBoxes [("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []), ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"])]
-- ["ids","invoices","passport","new year","birthday","memes","family","funny"]

allObjects :: [([Char], [[Char]])] -> [[Char]]
allObjects xs = [x | x <- getAllBoxes xs, x `notElem` getFirstElements xs ]

-- >>> allObjects [("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []), ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"])]
-- ["passport","new year","birthday"]

getEmptyBoxesNames :: [([Char], [[Char]])] -> [[Char]]
getEmptyBoxesNames xs = [x | (x, y) <- xs, null y]
-- >>> getEmptyBoxesNames [("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []), ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"])]
-- ["invoices","memes"]

cleanUp :: [([Char], [[Char]])] -> [([Char], [[Char]])]
cleanUp xs = let xs' = foldr (\(x, y) l -> (x, filter (\m -> m `notElem` getEmptyBoxesNames xs) y) : l) [] xs
    in if xs /= xs' then cleanUp xs' else filter (\(x, y) -> not (null y)) xs
-- >>> cleanUp [("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []), ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"])]
-- [("docs",["ids"]),("ids",["passport"]),("family",["new year","birthday"]),("pics",["family"])]

getPairs :: [([Char], [[Char]])] -> [([Char], [[Char]])]
getPairs xs =
    let cleaned = cleanUp xs
        boxes = getAllBoxes xs
    in filter (\(x, y) -> x `notElem` boxes) (foldr (\(x, y) l -> (x, if length y == 1 then
        (let filered = filter (\(n, m) -> n == head y) cleaned
        in if null filered then y else snd (head filered)) else y) : l) [] cleaned)


-- >>> getPairs [("docs", ["ids", "invoices"]), ("ids", ["passport"]), ("invoices", []), ("memes", []), ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"])]
-- [("docs",["passport"]),("pics",["new year","birthday"])]


