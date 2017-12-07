module Day4.B where

import qualified Data.Map as M
import Data.List
import Data.Monoid


-- Each word creates a new map where we count the number of times a char is
-- there, if two words have the same number of characters the same time, then
-- we can build an anagram. An alternative is to use an array of 26 elements
-- where each element is the number of times that char is present in the word
type State = [M.Map Char Int]

addWord :: (State, All) -> String -> (State, All)
addWord (st, All False) s = ([], All False) -- Nothing to do, we know that the password is not valid
addWord (st, a) s         = (mapped : st, a <> check)
  where
    check :: All
    check = All . all id . map (/=mapped) $ st

    mapped :: M.Map Char Int
    mapped = createMap s

createMap :: String -> M.Map Char Int
createMap = foldl' add M.empty
  where
    add :: M.Map Char Int -> Char -> M.Map Char Int
    add st c =
      case M.lookup c st of
        Nothing -> M.insert c 1 st
        Just n  -> M.insert c (n + 1) st


solve :: [[String]] -> Int
solve = sum . map (toInt . getAll . snd) . map (foldl' addWord ([], mempty))
  where
    toInt :: Bool -> Int
    toInt False = 0
    toInt True  = 1
