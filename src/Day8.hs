module Day8 where

-- unfinished, grids are lameassshit

import Grid
import Data.Char

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Map as M

-- screen = 50x6

data Pixel = Hash | Dot deriving (Show, Eq, Enum)

ppPixel :: Pixel -> String
ppPixel Hash = "#"
ppPixel Dot = "."

screen = grid 6 50 (repeat Dot)

testG = grid 3 7 (repeat Dot)
testA = parseSteps "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"
showTest = mapM_ (putStrLn . (++"\n") . ppGrid ppPixel) $ scanl doAction testG testA

type Wide = Int
type Tall = Int
type Index = Int

data Step = RotC Index Int | RotR Index Int | Rect Wide Tall deriving Show

parseSteps :: String -> [Step]
parseSteps s = either (const (error "bad parse")) id $ parse (some $ (rect' <|> rot) <* space) "" s

rect' :: Parser Step
rect' = do
  _ <- string "rect" <* space
  x <- read <$> some digitChar <* char 'x'
  y <- read <$> some digitChar
  return $ Rect x y
rot :: Parser Step
rot = do
  _ <- string "rotate" <* space
  con <- ((string "column" *> return RotC) <|> (string "row" *> return RotR)) <* space
  i <- read <$> (skipSome (satisfy (not . isDigit)) *> some digitChar)
  n <- read <$> (skipSome (satisfy (not . isDigit)) *> some digitChar)
  return $ con i n

doAction :: Grid Pixel -> Step -> Grid Pixel
doAction g (RotC i n) = cycleCol i n g
doAction g (RotR i n) = cycleRow i n g
doAction g (Rect x y) = rect y x Hash g

showActions :: Grid Pixel -> [Step] -> [Grid Pixel]
showActions = scanl doAction

doActions :: Grid Pixel -> [Step] -> Grid Pixel
doActions = foldl doAction

runFirst :: IO Int
runFirst = length . filter (==Hash) . concat . gridToList . doActions screen . parseSteps <$> readFile "./input/d8.txt"

runSecond :: IO ()
runSecond = doActions screen . parseSteps <$> readFile "./input/d8.txt" >>= printGrid ppPixel
