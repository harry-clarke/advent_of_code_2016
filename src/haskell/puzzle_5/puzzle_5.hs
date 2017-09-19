module Puzzle_5 where

import Crypto.Hash.MD5
import Data.List
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

main :: IO ()
main = do
  let a1 = decrypt_1 input
  print a1
