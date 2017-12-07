module Day5.B where

import qualified ZipList as Z
import Data.List
import Data.Maybe


solve :: [Int] -> Int
solve xs = snd $ fn (zipList xs, 0)
  where
    zipList :: [Int] -> Z.ZipList Int
    zipList (x:xs) = foldl' Z.insert (Z.const x) (reverse xs)

    fn :: (Z.ZipList Int, Int) -> (Z.ZipList Int, Int)
    fn (z, acc) = 
      case moved of
        Nothing -> (z, acc + 1)
        Just m -> fn (m, acc + 1)
      where
        nz = Z.setCurrent z (howMove (Z.current z))
        moved = Z.moveN (Z.current z) nz

        howMove :: Int -> Int
        howMove n
          | n >= 3    = (n - 1)
          | otherwise = (n + 1)
