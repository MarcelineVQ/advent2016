module Day3 where

import Data.List

type Tri = [Int]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\b -> case splitAt n b of ([],[]) -> Nothing; r -> Just r)

parseInput :: String -> [Tri]
parseInput = map (map read . words) . lines

parseInputTwo :: String -> [Tri]
parseInputTwo = chunksOf 3 . concat . transpose . map (map read . words) . lines

validTri :: Tri -> Bool
validTri t@[_,_,_] = all (\(x:xs) -> x < sum xs) edges
  where
    edges = take 3 . map (take 3) $ iterate (drop 1) (cycle t)
validTri _ = False

runFirst :: IO Int
runFirst = length . filter validTri . parseInput <$> readFile "./input/d3.txt"

runSecond :: IO Int
runSecond = length . filter validTri . parseInputTwo <$> readFile "./input/d3.txt"

testInput = "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603"
