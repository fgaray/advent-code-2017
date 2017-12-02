module Day2.A where

import Utils
import Data.List

solve :: [[Int]] -> Int
solve = sum . map (\(min, max) -> max - min) . map (\ls -> foldl' (\(min', max') x -> (min min' x, max max' x)) (head ls, head ls) ls)
