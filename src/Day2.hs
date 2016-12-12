module Day2 where

import Data.Array
import Data.Traversable

data Move = U | R | D | L deriving (Show, Read)
type Pos = (Int,Int)

type Grid = Array (Int,Int) Int
grid :: Grid
grid = listArray ((0,0), (2,2)) [1..9]

startGrid :: Pos
startGrid = (1,1)

startPad :: Pos
startPad = (0,-2)

type Pad = Array (Int,Int) Char
pad :: Array (Int,Int) Char
pad = listArray ((-2,-2),(2,2)) $ concatMap (concatMap show) [[0,0,1,0,0],[0,2,3,4,0],[5,6,7,8,9]] ++ "0ABC0" ++ "00D00"

padCursorMove :: Array (Int,Int) Char -> Pos -> Move -> Pos
padCursorMove arr p@(py,px) m = case m of
    U -> inPad (py-1,px)
    R -> inPad (py,px+1)
    D -> inPad (py+1,px)
    L -> inPad (py,px-1)
  where
    inPad :: Pos -> Pos
    inPad p'@(px',py') = if inRange (bounds arr) p' && arr ! p' /= '0'
      then p'
      else p

setPad :: Pos -> [Move] -> (Pos,Char)
setPad c ms = let r = foldl (padCursorMove pad) c ms
           in (r, pad ! r)

seriesPad :: Pos -> [[Move]] -> String
seriesPad p = snd . mapAccumL setPad p

parseInput :: String -> [[Move]]
parseInput = map (map (read . pure)) . lines

gridCursorMove :: Grid -> Pos -> Move -> Pos
gridCursorMove arr p@(py,px) m = case m of
    U | inRange (bounds arr) (py-1,px) -> (py-1,px)
    R | inRange (bounds arr) (py,px+1) -> (py,px+1)
    D | inRange (bounds arr) (py+1,px) -> (py+1,px)
    L | inRange (bounds arr) (py,px-1) -> (py,px-1)
    _ -> p

set :: Pos -> [Move] -> (Pos,Int)
set c ms = let r = foldl (gridCursorMove grid) c ms
           in (r, grid ! r)

series :: Pos -> [[Move]] -> Int
series p = read . concatMap show . snd . mapAccumL set p

runFirst :: IO Int
runFirst = series startGrid . parseInput <$> readFile "./input/d2.txt"

runSecond :: IO String
runSecond = seriesPad startPad . parseInput <$> readFile "./input/d2.txt"

testInput = "ULL\nRRDDD\nLURDL\nUUUUD"
