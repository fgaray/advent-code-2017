module Day1.B where


import Utils


solve :: [Int] -> Int
solve xs = sum . map (\(x, y) -> if x == y then x else 0) $ zipCircular middle xs xs
    where
        middle = length xs `div` 2
