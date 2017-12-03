module Day3.B where


import Prelude hiding (iterate)
import Data.List (find)
import Day3.A (coords)
import Utils




solve :: Int -> Int
solve n = fst $ iterate n 1


-- Super inneficient, but I need to do this because of the infinite list in coords
-- We create matrix of size 1, then size 2 and so on in order to be able to
-- reverse it.
iterate :: Int -> Int -> (Int, (Int, Int))
iterate input n =
    let cs = take n (coords (0, 0) 1)
        replaced = reverse . replaceSum . reverse . addNumbersToCoords $ cs
    in
        case find (\(n, _) -> n > input) replaced of
            Nothing -> iterate input (n + 1)
            Just x -> x


addNumbersToCoords :: [(Int, Int)] -> [(Int, (Int, Int))]
addNumbersToCoords = map (\x -> (manhattan (0, 0) x, x))

replaceSum :: [(Int, (Int, Int))] -> [(Int, (Int, Int))]
replaceSum []          = []
replaceSum [(_, c)]    = [(1, c)]
replaceSum ((_, c):xs) =
    let replaced = replaceSum xs
    in (sum . map (\(n, _) -> n) . filter (\(_, x) -> eucledean c x < 2) $ replaced, c) : replaced
