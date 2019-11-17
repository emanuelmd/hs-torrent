module Data.HTorrent.Info (Info (..)) where

import           Prelude                hiding (to)

import           Control.Lens           (ix, to, (^?))

import           Data.BEncode
import           Data.HTorrent.FileInfo (FileInfo)

data Info = Info
  { _inname        :: Text
  , _inpieceLength :: Integer
  , _inpieces      :: ByteString
  , _inlength      :: Maybe Integer
  , _infiles       :: [FileInfo] }
  deriving (Eq, Show)

instance FromBEncode Info where

  decodeB dict = do

    dict' <- dict ^? _BDict

    nm       <- dict' ^? ix "name" . _BString . to toS
    pieceLen <- dict' ^? ix "piece length" . _BInteger
    pieceMap <- dict' ^? ix "pieces" . _BString

    let len = dict' ^? ix "length" . _BInteger

    files'   <- dict' ^? ix "files" . _BList
    files    <- traverse decodeB files'

    return $ Info nm pieceLen pieceMap len files
