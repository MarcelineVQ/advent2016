module Day1 where

import Data.List (unfoldr, uncons, tails, find, groupBy)
import Data.Foldable (asum)
import Data.Maybe

data Facing = N | E | S | W deriving (Show)
data Turn = L | R deriving Show

type Steps = Int
type Action = (Turn, Steps)
type History = [Action]
type Distance = Int
type Spot = (Int, Int)

data Pos = Pos { facing :: Facing, spot :: Spot } deriving (Show)

samePosSpot :: Pos -> Pos -> Bool
samePosSpot (Pos _ x) (Pos _ y) = x == y


foo' :: (a -> Bool) -> [a] -> [[a]]
foo' p xs = case xs of
    (x:xs) | not (p x) -> [] : g xs
    xss -> g xss
  where
    g = groupBy (\x y -> p x == p y)


turn :: Turn -> Facing -> Facing
turn L f = case f of
  N -> W
  E -> N
  S -> E
  W -> S
turn R f = case f of -- clockwise
  N -> E
  E -> S
  S -> W
  W -> N

changePos :: Facing -> Int -> Pos -> Pos
changePos f s (Pos _ (y,x)) = case f of
  N -> Pos f (y-s,x)
  E -> Pos f (y,x+s)
  S -> Pos f (y+s,x)
  W -> Pos f (y,x-s)

runHistory :: History -> Spot
runHistory h = (\(Pos _ y) -> y) $ foldl doAction startingPos h
  where
    startingPos = Pos N (0,0)

runHistory' :: History -> [Pos]
runHistory' = scanl doAction startingPos
  where
    startingPos = Pos N (0,0)

foo = foldl (\xs x -> xs ++ doActionSteps (last xs) x) [startingPos]
  where
    startingPos = Pos N (0,0)

-- [Pos N (0,0),Pos E (0,8),Pos S (4,8),Pos W (4,4),Pos N (-4,4)]
runHistoryF :: History -> Maybe Pos
runHistoryF h = let ps = foo h
                in asum $ zipWith (\x ys -> find (x`samePosSpot`) ys) ps (tail (tails ps))

doActionSteps :: Pos -> Action -> [Pos]
doActionSteps p@(Pos f _) (t,s) = take s . drop 1 $ iterate (changePos (turn t f) 1) p

doAction :: Pos -> Action -> Pos
doAction p@(Pos f _) (t,s) = changePos (turn t f) s p

parseInput :: String -> History
parseInput = unfoldr (\b -> case pIn b of
    ([],_) -> Nothing
    (a,b) -> Just (pAction a,b) )
 where
     pIn = fmap (dropWhile (\x -> x == ',' || x == ' ')) . span (/=',')
     pAction (d:i) = case d of
       'L' -> (L, read i)
       'R' -> (R, read i)
       _ -> error "input formatting error"

distanceFromStart :: (Int, Int) -> Int
distanceFromStart (y,x) = abs y + abs x

testInput2 = "R3, R3, R3,R3,R3"

testInput = "R3, L5, R1,L5,,L0"
realInput = "R3, L5, R2, L2, R1, L3, R1, R3, L4, R3, L1, L1, R1, L3, R2, L3, L2, R1, R1, L1, R4, L1, L4, R3, L2, L2, R1, L1, R5, R4, R2, L5, L2, R5, R5, L2, R3, R1, R1, L3, R1, L4, L4, L190, L5, L2, R4, L5, R4, R5, L4, R1, R2, L5, R50, L2, R1, R73, R1, L2, R191, R2, L4, R1, L5, L5, R5, L3, L5, L4, R4, R5, L4, R4, R4, R5, L2, L5, R3, L4, L4, L5, R2, R2, R2, R4, L3, R4, R5, L3, R5, L2, R3, L1, R2, R2, L3, L1, R5, L3, L5, R2, R4, R1, L1, L5, R3, R2, L3, L4, L5, L1, R3, L5, L2, R2, L3, L4, L1, R1, R4, R2, R2, R4, R2, R2, L3, L3, L4, R4, L4, L4, R1, L4, L4, R1, L2, R5, R2, R3, R3, L2, L5, R3, L3, R5, L2, R3, R2, L4, L3, L1, R2, L2, L3, L5, R3, L1, L3, L4, L3"

runChallenge :: String -> Int
runChallenge = distanceFromStart . runHistory . parseInput

-- distanceTo :: History -> Action -> Int
