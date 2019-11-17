module Data.HTorrent.MetaInfo ( MetaInfo (..)
                              , readMetaInfo
                              , readBFile
                              ) where

import           Prelude            hiding (to)

import           Control.Lens       (ix, (^?))

import           Data.BEncode
import           Data.ByteString    as BL
import           Data.HTorrent.Info (Info (..))

type URL  = ByteString

data MetaInfo = MetaInfo
  { _mfannounce  :: URL
  , _mfcreatedBy :: ByteString
  , _mfinfo      :: Info }
  deriving (Eq, Show)

instance FromBEncode MetaInfo where

  decodeB dict = do

    ann          <- dict ^? _BDict . ix "announce"    . _BString
    createdBy    <- dict ^? _BDict . ix "created by"  . _BString

    info      <- dict ^? _BDict . ix "info"        >>= decodeB

    return $ MetaInfo ann createdBy info

-- Utils

readBFile :: Text -> IO (Maybe BEncode)
readBFile path = do
  contents <- BL.readFile (toS path)
  pure $ decode contents

readMetaInfo :: Text -> IO (Maybe MetaInfo)
readMetaInfo path = do
  contents <- readBFile path
  pure (contents >>= decodeB)
