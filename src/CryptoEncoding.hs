{-# LANGUAGE OverloadedStrings #-}

module CryptoEncoding where

import Numeric (showHex)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.Split (chunksOf)
import Data.Bits (Bits, (.|.),shiftR,shiftL,(.&.),shift)
import Data.Word8
import Data.List (unfoldr,dropWhile)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Base64 as B64
import qualified Test.QuickCheck as QC
import Test.QuickCheck ((==>))

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

equals_index :: Word8
equals_index = 64

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


b64List = [_A.._Z]++[_a.._z]++[_0.._9]++[_plus,_slash,_equal]
b64ListMap = zip [0..] b64List :: [(Word8, Word8)]
b64ListMapFlip = zip b64List [0..] :: [(Word8, Word8)]


encode64 :: PlainText -> B64String
encode64 s = B64String . BS.pack
           . map (fromJust . flip lookup b64ListMap . fromIntegral)
           . concatMap parseChunk . chunksOf 3 . BS.unpack $ s
  where
    parseChunk [a,b,c] = [ shiftR a 2
                         , shiftR b 4 .|. (shiftL a 4 .&. 0x30)
                         , shiftR c 6 .|. (shiftL b 2 .&. 0x3c)
                         , c .&. 0x3f]
    parseChunk [a,b]   = [ shiftR a 2
                         , shiftR b 4 .|. (shiftL a 4 .&. 0x30)
                         , shiftL b 2 .&. 0x3c
                         , equals_index]
    parseChunk [a]     = [ shiftR a 2
                         , shiftL a 4 .&. 0x30
                         , equals_index
                         , equals_index]

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



-- QuickCheck Tests

instance QC.Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> QC.arbitrary


prop_myb16 :: BS.ByteString -> Bool
prop_myb16 bs = Just bs == (decode16 . encode $ bs)

prop_myb64 :: BS.ByteString -> Bool
prop_myb64 bs = Just bs == (decode64 . encode $ bs)

prop_b64 :: BS.ByteString -> Bool
prop_b64 bs = Right bs == (B64.decode . B64.encode $ bs)

prop_b64_comp :: BS.ByteString -> Bool
prop_b64_comp bs = b64str == B64.encode bs
  where
    B64String b64str = encode64 bs

