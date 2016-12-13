module Day7 where

import Data.List
import qualified Data.List.Split as LS

data IP = IP { normal :: [String], hyper :: [String] }
  deriving Show

parseIP :: String -> IP
parseIP s = let (normal,hyper) = foldr (\y ~(xs,ys) -> (y:ys,xs)) ([],[]) ss
                ss = LS.splitOneOf "[]" s
             in IP normal hyper

hasABBA :: String -> Bool
hasABBA = any match . fours
  where
    match [a,b,c,d] = a == d && b == c && a /= b
    fours = init . init . init . init . map (take 4) . tails

hasSSL :: IP -> Bool
hasSSL ip = any hasBAB abas
  where
    threes = init . init . init . map (take 3) . tails
    abas = filter (\[a,b,c] -> a == c) . concatMap threes . normal $ ip
    hasBAB [a,b,c] = any (\[x,y,z] -> x == b && y == a && z == b) (concatMap threes . hyper $ ip)

hasTLS :: IP -> Bool
hasTLS (IP normal hyper) = any hasABBA normal && all (not . hasABBA) hyper

run :: (IP -> Bool) -> IO Int
run f = length . filter (f . parseIP) . lines <$> readFile "./input/d7.txt"

runFirst = run hasTLS
runSecond = run hasSSL

testTLSGood1 = "abba[mnop]qrst"
testTLSBad1 = "abcd[bddb]xyyx"
testTLSBad2 = "aaaa[qwer]tyui"
testTLSGood2 = "ioxxoj[asdfgh]zxcvbn"

testSSLGood1 = "aba[bab]xyz"
testSSLBad1 = "xyx[xyx]xyx"
testSSLGood2 = "aaa[kek]eke"
testSSLGood3 = "zazbz[bzb]cdb"
