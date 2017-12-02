module Utils where

import Data.Char (ord)

-- | Zips a list with other with dropping n elements in the second list
zipDiffer :: Int -> [a] -> [b] -> [(a, b)]
zipDiffer n xs ys = zip xs (drop n ys)

-- Zips a list creating a circular version of it
zipCircular :: Int -> [a] -> [b] -> [(a, b)]
zipCircular n xs ys = zipDiffer n xs (concat . repeat $ ys)

readDigitsString :: String -> [Int]
readDigitsString = map (\c -> ord c - 48)


readDigitsFile :: FilePath -> IO [Int]
readDigitsFile f = readFile f >>= return . readDigitsString

solveWithFile :: Show b => FilePath -> (String -> a) -> (a -> b) -> IO ()
solveWithFile file convert solve = do
    str <- readFile file
    let sol = solve . convert $ str
    print sol


readMatrix :: Read a => FilePath -> IO ([[a]])
readMatrix file = do
    str <- readFile file
    let ls = map words . lines $ str
        readed = map (map read) ls
    return readed

-- | Maps over a list the element over the next ones.
--
-- Ex: [1, 2, 3, 4]
--   1 with [2, 3, 4]
--   2 with [3, 4]
--
mapOverNexts :: (a -> a -> b) -> [a] -> [b]
mapOverNexts _ [] = []
mapOverNexts f (x:xs) = map (f x) xs ++ mapOverNexts f xs
