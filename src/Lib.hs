{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (lmain) where

import Options.Generic

import Day1.A
import Day1.B
import Day2.A
import Day2.B
import Day3.A
import Day3.B
import Day4.A
import Day4.B
import Day5.A
import Day5.B

import Utils

data Args =
      Day1A { file :: String }
    | Day1B { file :: String }
    | Day2A { file :: String }
    | Day2B { file :: String }
    | Day3A { dat  :: Int }
    | Day3B { dat  :: Int }
    | Day4A { file :: String }
    | Day4B { file :: String }
    | Day5A { file :: String }
    | Day5B { file :: String }
    deriving (Generic, Show)

instance ParseRecord Args

lmain :: IO ()
lmain = do
    x <- getRecord "Advent of Code 2017"
    case x of
        Day1A file -> solveWithFile file readDigitsString Day1.A.solve
        Day1B file -> solveWithFile file readDigitsString Day1.B.solve
        Day2A file -> readMatrix file >>= return . Day2.A.solve >>= print
        Day2B file -> readMatrix file >>= return . Day2.B.solve >>= print
        Day3A dat  -> return (Day3.A.solve dat) >>= print
        Day3B dat -> return (Day3.B.solve dat) >>= print
        Day4A file -> readFile file >>= return . Day4.A.solve . map words . lines >>= print
        Day4B file -> readFile file >>= return . Day4.B.solve . map words . lines >>= print
        Day5A file -> readFile file >>= return . Day5.A.solve . map read . lines >>= print
        Day5B file -> readFile file >>= return . Day5.B.solve . map read . lines >>= print
