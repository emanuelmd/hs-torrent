module Data.Meta ( Info (..)
                 , MetaInfo (..)
                 , FileInfo (..)
                 , readMetaInfo
                 , ) where

import           Prelude         hiding (ByteString, to)

import           Control.Lens

import           Data.BEncoded
import           Data.ByteString as BL

type URL = Text
type Byte = Integer

data MetaInfo = MetaInfo
  { _mfannounce  :: URL
  , _mfcreatedBy :: ByteString
  , _mfinfo      :: Info }
  deriving (Eq, Show)

instance FromBencoded MetaInfo where

  decodeB dict = do

    ann       <- dict ^? _BDict . ix "announce"    . _BString . to toS
    createdBy <- dict ^? _BDict . ix "created by"  . _BString

    info      <- dict ^? _BDict . ix "info"        >>= decodeB

    return $ MetaInfo ann createdBy info

data Info = Info
  { _inname        :: Text
  , _inpieceLength :: Integer
  , _inpieces      :: ByteString
  , _inlength      :: Maybe Byte
  , _infiles       :: [FileInfo] }
  deriving (Eq, Show)

instance FromBencoded Info where

  decodeB dict = do

    dict' <- dict ^? _BDict

    nm       <- dict' ^? ix "name" . _BString . to toS
    pieceLen <- dict' ^? ix "piece length" . _BInteger
    pieceMap <- dict' ^? ix "pieces" . _BString

    let len = dict' ^? ix "length" . _BInteger

    files'   <- dict' ^? ix "files" . _BList
    files    <- traverse decodeB files'

    return $ Info nm pieceLen pieceMap len files

data FileInfo = FileInfo
  { _filength :: Byte
  , _fipath   :: [ByteString] }
  deriving (Eq, Show)

instance FromBencoded FileInfo where

  decodeB dict = FileInfo
                <$> dict ^? _BDict . ix "length" . _BInteger
                <*> dict ^? _BDict . ix "path" . _BList . to getBStrings

getBStrings :: [BEncoded] -> [ByteString]
getBStrings = mapMaybe (^? _BString)

-- Utils

readMetaInfo :: Text -> IO (Maybe MetaInfo)
readMetaInfo path = do

  contents <- BL.readFile (toS path)
  pure (decode contents >>= decodeB)

