module Day4.A where

import qualified Data.Set as S
import Data.List

solve :: [[String]] -> Int
solve = foldl' (\acc x -> if x then acc + 1 else acc) 0
      . map (\(x, y) -> x == y)
      . map (\xs -> (length xs, S.size . foldr S.insert S.empty $ xs))
