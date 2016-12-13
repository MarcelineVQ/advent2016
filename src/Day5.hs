module Day5 where

import Data.List
import Data.Ix
import qualified Data.ByteString.Char8 as BS8
import Crypto.Hash
import Data.Char (isDigit)
import qualified Data.Set as S

goodHash :: String -> Bool
goodHash = isPrefixOf "00000"

goodPos :: String -> Bool
goodPos s = isDigit h && (inRange (0,7) . read) [h]
  where
    h = hashHead s

md5 :: String -> String
md5 s = show (hash (BS8.pack s) :: Digest MD5)

posAndValue :: String -> (Char,Char)
posAndValue = (\(x:y:_) -> (x,y)) . drop 5

hashHead :: String -> Char
hashHead h = h !! 5

getHashes :: String -> [String]
getHashes s = filter goodHash . map snd . iterate (\(i,_) -> (i+1, md5 (s ++ show i))) $ (0,s)

runFirst = map hashHead . take 8 . getHashes

-- :(
runSecond = map snd . sortOn fst . take 8. concat . snd
          . mapAccumL (\a b -> (S.insert (fst b) a, if S.member (fst b) a then [] else [b]) ) S.empty
          . map posAndValue . filter goodPos . getHashes


input = "ffykfhsq"
