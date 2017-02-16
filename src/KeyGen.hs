module KeyGen where

import System.Random
import Control.Monad.Random
import Control.Monad.Random.Class (getRandom,getRandoms)
import qualified Data.ByteString as BS

type Key = BS.ByteString

randKey :: RandomGen g => Int -> Rand g Key
randKey bs = do
   kstream <- getRandoms
   return . BS
