module Day3.A where

import Prelude hiding (Left, Right)
import Utils
import Data.List
import Debug.Trace

data Movement =
      Right
    | Up
    | Left
    | Down
    deriving (Enum, Ord, Eq, Bounded, Show)


solve :: Int -> Int
solve n = manhattan (0, 0) $ (coords (0, 0) 1) !! (n - 1)


-- Infinite list
coords :: (Int, Int) -> Int -> [(Int, Int)]
coords (0, 0) 1 = (0, 0) : (1, 0) : (1, 1) : coords (1, 1) 2
coords (x, y) n = moved ++ coords (last moved) (n + 2)
    where
        moved :: [(Int, Int)]
        moved = 
            let a = foldl' (move' n) [] [Left, Down]
            in foldl' (move' (n + 1)) a [Right, Up]

        move' :: Int -> [(Int, Int)] -> Movement -> [(Int, Int)]
        move' n [] mov = move (x, y) n mov
        move' n l mov  = l ++ move (last l) n mov


move :: (Int, Int) -> Int -> Movement -> [(Int, Int)]
move (x, y) n Left  = map (\n' -> (x - n', y)) [1..n]
move (x, y) n Right = map (\n' -> (x + n', y)) [1..n]
move (x, y) n Down  = map (\n' -> (x, y - n')) [1..n]
move (x, y) n Up    = map (\n' -> (x, y + n')) [1..n]
