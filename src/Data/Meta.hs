{-# LANGUAGE TemplateHaskell #-}

module Data.Meta where

import           Prelude         hiding (ByteString)

import           Control.Lens

import           Data.BEncoded
import           Data.ByteString as BL
import qualified Data.Map        as Map
import           Data.Maybe      (catMaybes)

type URL = Text
type Byte = Integer

data MetaInfo = MetaInfo
  { _mfannounce  :: URL
  , _mfcreatedBy :: ByteString
  , _mfinfo      :: Info }
  deriving (Eq, Show)

instance FromBencoded MetaInfo where

  decodeB dict = do

    dict' <- dict ^? _BDict

    ann <- toS <$> (Map.lookup "announce" dict' >>= extract _BString)
    info <- Map.lookup "info" dict' >>= decodeB
    createdBy <- Map.lookup "created by" dict' >>= extract _BString

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

    nm <- fmap toS (Map.lookup "name" dict' >>= extract _BString)
    pieceLen <- Map.lookup "piece length" dict' >>= extract _BInteger
    pieceMap <- Map.lookup "pieces" dict' >>= extract _BString

    let len = Map.lookup "length" dict' >>= extract _BInteger

    files' <- Map.lookup "files" dict' >>= extract _BList
    files  <- (sequenceA $ fmap decodeB files') :: Maybe [FileInfo]

    return $ Info nm pieceLen pieceMap len files

data FileInfo = FileInfo
  { _filength :: Byte
  , _fipath   :: [ByteString] }
  deriving (Eq, Show)

instance FromBencoded FileInfo where
  decodeB dict = do
    dict' <- dict ^? _BDict

    len'   <- Map.lookup "length" dict' >>= extract _BInteger
    paths' <- Map.lookup "path" dict' >>= extract _BList

    return $ FileInfo len' (getBStrings paths')

-- Utils
getBStrings :: [BEncoded] -> [ByteString]
getBStrings = catMaybes . fmap (flip (^?) _BString)

extract :: Getting (First a) s a -> s -> Maybe a
extract = flip (^?)

readMetaInfo :: Text -> IO (Maybe MetaInfo)
readMetaInfo path = do

  contents <- BL.readFile (toS path)

  return $ case decode contents of
    Just benc -> decodeB benc
    Nothing   -> Nothing


