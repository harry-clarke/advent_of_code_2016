module Puzzle_5 where

import Crypto.Hash.MD5
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

input = "ojvtpuvg"

hashAndHex :: String -> String
hashAndHex = foldr (++) "" . map toHex . B.unpack . hash . C.pack
  where toHex = C.unpack . L.toStrict . toLazyByteString . word8HexFixed

matchingHashes :: String -> [String]
matchingHashes s = map (drop 5) $ filter ("00000" `isPrefixOf`) [hashAndHex m | i<-[0,1..], let m = s ++ show i]

decrypt_1 :: String -> String
decrypt_1 = foldr ((:) . (!! 0)) "" . take 8 . matchingHashes

decrypt_2 :: String -> String
decrypt_2 = decrypt_2' (replicate 8 Nothing) . filter ((`elem` ['0'..'7']) . head) . matchingHashes
  where
    decrypt_2' :: [Maybe Char] -> [String] -> String
    decrypt_2' d hs
      | all isJust d = map fromJust d
      | ((i:d':_):hs) <- hs = decrypt_2' (swap d (read [i]) d') hs
    swap :: [Maybe Char] -> Int -> Char -> [Maybe Char]
    swap d i c =
      case d !! i of
        Just _ -> d
        Nothing -> let (x,(y:ys)) = splitAt i d in x ++ (Just c):ys

main :: IO ()
main = do
  let a1 = decrypt_1 input
  let a2 = decrypt_2 input
  --print a1
  print a2
