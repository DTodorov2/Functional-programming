module Color where

data Color = Gray | Yellow | Green
    deriving (Eq, Show)

parseStringToColor strColor = case strColor of
    "gr" ->  Just Green
    "y"  ->  Just Yellow
    "g"  ->  Just Gray
    _    ->  Nothing