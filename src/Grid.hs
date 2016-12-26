{-# LANGUAGE OverloadedStrings #-}

module Grid
( Grid
, grid
, empty
, squareGrid
, cycleRow
, cycleCol
, rect
, mapRow
, mapCol
, ppGrid
, printGrid
, gridToList
)
where

import qualified Data.Map as M
import Data.Map (Map(..))
import Data.Ix

import Data.List (unfoldr, intercalate, sortOn)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\b -> case splitAt n b of
  ([],_) -> Nothing
  x -> Just x)


-- row major, 0 indexed: rowcount, columncount, element map
type Rows = Int -- Height
type Cols = Int -- Width
data Grid a = Grid Rows Cols (Map Index a) deriving (Show)
type Index = (Int,Int)

squareGrid :: [a] -> Grid a
squareGrid ls = let s = truncate . sqrt . fromIntegral . length $ ls
                in Grid s s $ M.fromList (zip (range ((0,0),(s-1,s-1))) ls)

grid :: Int -> Int -> [a] -> Grid a
grid h w = Grid h w . M.fromList . zip (range ((0,0),(h-1,w-1)))

empty = Grid 0 0 M.empty

cycleRow :: Int -> Int -> Grid a -> Grid a
cycleRow r n g@(Grid _ w _) = cycleRun False ((r,0),(r,w)) n g

cycleCol :: Int -> Int -> Grid a -> Grid a
cycleCol c n g@(Grid h _ _) = cycleRun True ((0,c),(h,c)) n g

cycleRun :: Bool -> (Index, Index) -> Int -> Grid a -> Grid a
cycleRun lr ra n (Grid h w m) = Grid h w $ M.mapKeys doMap m
  where
    stepH n' = (n' + n) `mod` w
    stepV n' = (n' + n) `mod` h
    doMap k@(y,x) = if inRange ra k
                      then if lr then (stepV y, x) else (y, stepH x)
                      else k

-- fill a rect from the top left corner with v
rect :: Int -> Int -> a -> Grid a -> Grid a
rect y x v (Grid h w m) =
  let gridIndices = range ((0,0),(y-1,x-1))
  in Grid h w $ foldr (M.adjust (const v)) m gridIndices

mapRow :: Int -> (a -> a) -> Grid a -> Grid a
mapRow r f (Grid h w m) =
  let rowIndices = range ((r,0),(r,w))
  in Grid h w $ foldr (M.adjust f) m rowIndices

mapCol :: Int -> (a -> a) -> Grid a -> Grid a
mapCol c f (Grid h w m) =
  let colIndices = range ((0,c),(h,c))
  in Grid h w $ foldr (M.adjust f) m colIndices

gridToList :: Grid a -> [[a]]
gridToList (Grid h w m) = chunksOf w . map snd . M.assocs $ m

ppGrid :: (a -> String) -> Grid a -> String
ppGrid s (Grid h w m) = concat . intercalate ["\n"] . chunksOf w . map (s . snd) . M.assocs $ m

printGrid :: (a -> String) -> Grid a -> IO ()
printGrid f g = putStrLn $ ppGrid f g
