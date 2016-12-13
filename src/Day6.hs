module Day6 where

import qualified Data.Map as M

import Data.List
import Data.Ord

type GetBy = (((Char,Int) -> (Char,Int) -> Ordering) -> [(Char,Int)] -> (Char,Int))

histo :: GetBy -> String -> Char
histo f s = fst . f (comparing snd) . M.toList . M.fromListWith (+) $ zip s [1,1..]

parseInput = transpose . lines

runFirst :: IO String
runFirst = map (histo maximumBy) . parseInput <$> readFile "./input/d6.txt"

runSecond :: IO String
runSecond = map (histo minimumBy) . parseInput <$> readFile "./input/d6.txt"

testInput = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
