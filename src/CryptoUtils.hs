module CryptoUtils where

import qualified Data.ByteString as BS
import Data.Word8


pkcs7 :: Word8 -> BS.ByteString -> BS.ByteString
pkcs7 blocksize bs = BS.append bs . BS.replicate n . fromIntegral $ n
  where
    n = fromIntegral blocksize - (BS.length bs `mod` fromIntegral blocksize)

unpkcs7 :: BS.ByteString -> Maybe BS.ByteString
unpkcs7 bs = if (BS.all (==last') pad)
             then Just text
             else Nothing
  where
    len        = BS.length bs
    last'      = BS.last bs
    (text,pad) = BS.splitAt (len - (fromIntegral last')) bs
