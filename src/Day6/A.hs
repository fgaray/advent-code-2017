module Day6.A where

import qualified Data.Vector as V
import qualified Data.Set as S

solve :: [Int] -> Int
solve xs = iterate (V.fromList xs)



iterate :: V.Vector Int -> S.Set (V.Vector Int) -> Int
iterate v s
    | S.member v s  = 0
    | otherwise     = 1 + iterate v' (S.insert v' s)
    where
        v' =
            let index = indexBigger v
                n     = (v V.!! index) - 1
                v''   = V.imap (\i x -> if i == index then 1 else x) v
            in distribute n index v''

        indexBigger :: S.Vector Int -> Int
        indexBigger = fst . V.ifoldl' (\index (m, cur) x -> if x > m then (x, index) else (m, cur)) (0, 0)



