module Data.HTorrent.FileInfo (FileInfo (..)) where

import           Prelude      hiding (to)

import           Control.Lens
import           Data.BEncode

type Byte = Integer

data FileInfo = FileInfo
  { _filength :: Byte
  , _fipath   :: [ByteString] }
  deriving (Eq, Show)

instance FromBEncode FileInfo where

  decodeB dict = FileInfo
                <$> dict ^? _BDict . ix "length" . _BInteger
                <*> dict ^? _BDict . ix "path" . _BList . to getBStrings
