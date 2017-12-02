module Day2.B where

import Utils
import Data.List

solve :: [[Int]] -> Int
solve = sum . filter (/=0) . concat . map (mapOverNexts divisible)
    where
        divisible :: Int -> Int -> Int
        divisible x y
            | first `mod` second == 0 = first `div` second
            | otherwise = 0
            where
                first = max x y
                second = min x y
