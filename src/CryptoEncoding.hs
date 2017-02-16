{-# LANGUAGE OverloadedStrings #-}

module CryptoEncoding where

import Numeric (showHex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.Split (chunksOf)
import Data.Bits (Bits, (.|.),shiftR,shiftL,(.&.),shift)
import Data.Word8
import Data.List (unfoldr)
import Data.Maybe (fromJust)


type PlainText = BS.ByteString
newtype HexString = HexString BS.ByteString deriving (Show,Eq)
newtype B64String = B64String BS.ByteString deriving (Show,Eq)


class Encoded s where
  encode :: PlainText -> s
  decode :: s -> Maybe PlainText

instance Encoded HexString where
  encode = encode16
  decode = decode16


instance Encoded B64String where
  encode = encode64
  decode = decode64



encode16 :: PlainText -> HexString
encode16 bs = HexString $ BS.concat $ map (encodeInt16 . fromIntegral) $ BS.unpack bs
  where
    encodeInt16 i = BSC.pack $ (showHex i) ""

decode16 :: HexString -> Maybe PlainText
decode16 (HexString hs)
  | BS.length hs `rem` 2 == 0 =
        Just . BS.pack $ map (fromIntegral . decodeInt16) $ unfoldr (mSplitAt 2) hs
  | otherwise                 = decode16 $ HexString (BS.cons _0 hs)
  where
    decodeInt16 s = read $ "0x" ++ BSC.unpack s
    mSplitAt n "" = Nothing
    mSplitAt n bs = Just $ BS.splitAt n bs


b64List = [_A.._Z]++[_a.._z]++[_0.._9]++[_plus,_slash]
b64ListMap = zip [0..] b64List :: [(Word8, Word8)]
b64ListMapFlip = zip b64List [0..] :: [(Word8, Word8)]

encode64 :: PlainText -> B64String
encode64 s
    | r == 0    = B64String . encodeIntB64 $ joinBits 8 s'
    | otherwise = let t = BS.unpack $ encodeIntB64 $ joinBits 8 (s' ++ replicate (3-r) 0) 
                  in B64String . BS.pack $ take (length t - (3-r)) t ++ (replicate (3-r) _equal)
  where
    r = BS.length s `mod` 3
    s' = BS.unpack s
    encodeIntB64 :: Integer -> BS.ByteString
    encodeIntB64 n = BS.pack $ map (fromJust . flip lookup b64ListMap . fromIntegral) (splitBits 6 n)


decode64 :: B64String -> Maybe PlainText
decode64 (B64String b64s)
  | BS.length b64s `rem` 4 == 0 = parseChunkList <$> b64toWords b64s
  | otherwise                   = Nothing
  where
    parseChunkList = BS.pack . concatMap parseChunk . chunksOf 4
    b64toWords = sequence . map (flip lookup b64ListMapFlip) 
               . filter (/= _equal) . BS.unpack
    parseChunk [a,b,c,d] = [shiftL a 2 .|. shiftR b 4
                       , shiftL (b .&. 0x0f) 4 .|. shiftR c 2
                       , shiftL (c .&. 0x03) 6 .|. d]
    parseChunk [a,b,c]   = [shiftL a 2 .|. shiftR b 4
                       , shiftL (b .&. 0x0f) 4 .|. shiftR c 2]
    parseChunk [a,b]     = [shiftL a 2 .|. shiftR b 4]


-- Split Number into bit chunks of size bs
splitBits :: (Num a, Bits a) => Int -> a -> [a]
splitBits bs number = reverse $ splitBits' bs number
  where
    splitBits' _ 0        = []
    splitBits' bytesize n = 
        fromIntegral (2^bytesize-1) .&. n : splitBits' bytesize (shiftR n bytesize)

-- Join list of numbers into one number in bit chunks of byteshift
joinBits :: (Integral a, Num b, Bits b) => Int -> [a] -> b
joinBits byteshift wlist = foldr (.|.) 0 shifted
  where
    bitpos = [length wlist - 1,length wlist -2..0]
    shifted = zipWith (\p n -> shift (fromIntegral n) (byteshift*p)) bitpos wlist
