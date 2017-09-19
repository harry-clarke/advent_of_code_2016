module Puzzle_7 where

import System.IO
import Data.Maybe
import Data.List

isAbba :: String -> Bool
isAbba (a:b:c:d:_) = a==d && b==c && a/=b
isAbba _ = False

containsAbba :: String -> Bool
containsAbba [] = False
containsAbba s = isAbba s || containsAbba (drop 1 s)

type ABA = (Char , Char , Char)

getAbas :: String -> [ABA]
getAbas = mapMaybe toAba . tails

toAba :: String -> Maybe ABA
toAba (a:b:c:_) | a==c && a/=b = Just (a , b , c)
toAba _ = Nothing

reverseAba :: ABA -> ABA
reverseAba (a,b,c) | a==c = (b,a,b)

data IPv7 = IPv7 { strings :: [String] , hypernets :: [String] }
  deriving (Show , Eq)

parseIPv7 :: String -> IPv7
parseIPv7 = convertToIPv7 . parseStr ([],[])
  where
  parseStr :: ([String] , [String]) -> String ->  ([String] , [String])
  parseHyp :: ([String] , [String]) -> String ->  ([String] , [String])
  parseStr (strs , hyps) str =
    let (s,rest) = break (=='[') str in
    parseHyp (s:strs , hyps) rest
  parseHyp v "" = v
  parseHyp (strs , hyps) str =
    let (('[':h),rest) = break (==']') str in
    case rest of
      "" -> (h:strs , hyps) -- Edge case: No closing bracket found.
      (']':rest) -> parseStr (strs , h:hyps) rest
  convertToIPv7 :: ([String] , [String]) -> IPv7
  convertToIPv7 (strs , hyps) = IPv7 {strings = strs , hypernets = hyps}

supportsTLS :: IPv7 -> Bool
supportsTLS v = any containsAbba (strings v) && all (not . containsAbba) (hypernets v)

supportsSSL :: IPv7 -> Bool
supportsSSL v =
  let as = map reverseAba . concatMap getAbas $ strings v in
  let hs = concatMap getAbas $ hypernets v in
  not . null $ as `intersect` hs

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let l = lines contents
  let p = map parseIPv7 l
  let a1 = length $ filter supportsTLS p
  let a2 = length $ filter supportsSSL p
  print a1
  print a2
