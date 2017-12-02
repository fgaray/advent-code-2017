{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (lmain) where

import Options.Generic

import Day1.A
import Day1.B
import Day2.A
import Day2.B

import Utils

data Args =
      Day1A { file :: String }
    | Day1B { file :: String }
    | Day2A { file :: String }
    | Day2B { file :: String }
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