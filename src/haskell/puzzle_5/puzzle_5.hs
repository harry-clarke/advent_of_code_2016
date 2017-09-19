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

decrypt_1 :: String -> String
decrypt_1 = reverse . decrypt_1' 0 ""
  where
    decrypt_1' :: Int -> String -> String -> String
    decrypt_1' _ d _ | length d >= 8 = d
    decrypt_1' i d s =
      let hashStr = hashAndHex (s ++ show i) in
      let d' = if "00000" `isPrefixOf` hashStr then (hashStr !! 5):d else d in
      decrypt_1' (i+1) d' s

main :: IO ()
main = do
  let a1 = decrypt_1 input
  print a1
