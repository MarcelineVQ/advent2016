{-# LANGUAGE RecordWildCards #-}

module Day4 where

import Data.List
import Data.Char
import Data.Ord (Down(..))

type Letters = String
type Sector = Int
type Checksum = String

data Room = Room { letters :: Letters, sector :: Sector, checksum :: Checksum }
  deriving (Eq, Ord, Show)

parseRoom :: String -> Room
parseRoom s = let (l,c) = span (/='[') s
                  (le, se) = span isLetter (filter (/='-') l)
                  ch = takeWhile (/=']') . drop 1 $ c
              in Room le (read se) ch

parseInput :: String -> [Room]
parseInput = map parseRoom . lines

validRoom :: Room -> Bool
validRoom r = and (zipWith (\(x:_) y -> x == y) (sortOn (Down . length)
            . group . sort $ letters r) (checksum r))

rotateRoom :: Room -> Room
rotateRoom Room{..} = Room { letters = map f letters, ..}
  where
    f '-' = ' '
    f s = chr (base + (ord s + sector - base)`mod`26)
    base = ord 'a'

runFirst :: IO Int
runFirst = sum . map sector . filter validRoom . parseInput <$> readFile "./input/d4.txt"

runSecond :: IO [Sector]
runSecond = map sector . filter (isInfixOf "north" . map toLower . letters)
          . map rotateRoom . filter validRoom . parseInput <$> readFile "./input/d4.txt"

testReal = "aaaaa-bbb-z-y-x-123[abxyz]"
testTie  = "a-b-c-d-e-f-g-h-987[abcde]"
testFake = "totally-real-room-200[decoy]"
