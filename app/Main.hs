module Main (main) where

import qualified Days.Day1  as D1
import qualified Days.Day2  as D2
import qualified Days.Day3  as D3
import qualified Days.Day4  as D4
import qualified Days.Day5  as D5
import qualified Days.Day6  as D6
import qualified Days.Day7  as D7
import qualified Days.Day8  as D8
import qualified Days.Day9  as D9
import qualified Days.Day10 as D10
import qualified Days.Day11 as D11
import qualified Days.Day12 as D12
import qualified Days.Day13 as D13
import qualified Days.Day14 as D14
import qualified Days.Day15 as D15
import qualified Days.Day16 as D16
import qualified Days.Day17 as D17
import qualified Days.Day18 as D18
import qualified Days.Day19 as D19
import qualified Days.Day20 as D20
import qualified Days.Day21 as D21
import qualified Days.Day22 as D22
import qualified Days.Day23 as D23
import qualified Days.Day24 as D24
import qualified Days.Day25 as D25

import Types

import System.Environment (getArgs)
import Numeric (readDec)
import Control.Monad (when)

-- copied from base-4.20.0.1
(!?) :: [a] -> Int -> Maybe a

xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

runners :: [ToRun -> String -> (String,String)]
runners = [D1.runner,
           D2.runner,
           D3.runner,
           D4.runner,
           D5.runner,
           D6.runner,
           D7.runner,
           D8.runner,
           D9.runner,
           D10.runner,
           D11.runner,
           D12.runner,
           D13.runner,
           D14.runner,
           D15.runner,
           D16.runner,
           D17.runner,
           D18.runner,
           D19.runner,
           D20.runner,
           D21.runner,
           D22.runner,
           D23.runner,
           D24.runner,
           D25.runner]

main :: IO ()
main = do
    args <- getArgs
    -- check if there is at least one argument and if 1st argument is valid
    if not (null args) &&
       let firstIntRead = readDec . head $ args in
       (not . null $ firstIntRead) &&
       (fst . head $ firstIntRead) `elem` [1..(length runners)]
        then do
            let partNum = args !? 1
            let dayNum = read (head args)
            let runs = ToRun {fstDay=maybe True (== "1") partNum, sndDay=maybe True (== "2") partNum}
            cont <- getContents
            let results = (runners !! (dayNum - 1)) runs cont
            when (fstDay runs) $ do
                putStrLn $ "p1: " ++ fst results
            when (sndDay runs) $ do
                putStrLn $ "p2: " ++ snd results
        else do
            putStrLn "1st argument must be a number from 0 to 25, corresponding to the day"
