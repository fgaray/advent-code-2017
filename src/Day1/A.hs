module Day1.A where


import Utils


solve :: [Int] -> Int
solve xs = sum . map (\(x, y) -> if x == y then x else 0) $ zipCircular 1 xs xs
