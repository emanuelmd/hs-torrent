{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Data.BEncode ( BEncode     (..)
                    , FromBEncode (..)
                    , ToBEncode   (..)
                    , _BString
                    , _BInteger
                    , _BList
                    , _BDict
                    , getBStrings
                    , decode
                    , encode
                    ) where

import           Prelude         hiding (many, optional, try, (<|>))

import           Control.Lens

import           Text.Parsec

import qualified Data.ByteString as B
import qualified Data.Map        as Map
import           Data.Maybe      (fromJust)

-- Data
data BEncode = BString ByteString
             | BInteger Integer
             | BList [BEncode]
             | BDict (Map.Map ByteString BEncode)
             deriving (Show, Eq)

makePrisms ''BEncode

-- Parsers

numerP :: Stream s m Char => ParsecT s u m Integer
numerP = fromJust . readMaybe <$> (negativeN <|> positiveN)
  where
    negativeN = (:) <$> char '-' <*> many1 digit
    positiveN = many digit

bstringP :: Stream s m Char => ParsecT s u m BEncode
bstringP = do
  n <- try numerP
  when (n < 1) $ parserFail "NumerP cannot be negative"
  rest <- char ':' *> count (fromIntegral n) anyChar
  return $ BString (toS rest)

bintegerP :: Stream s m Char => ParsecT s u m BEncode
bintegerP = BInteger <$> (skipMany1 (char 'i') *> numerP <* char 'e')

blistP :: Stream s m Char => ParsecT s u m BEncode
blistP = BList <$> (skipMany1 (char 'l') *> manyTill belementP (char 'e'))

bdictP :: Stream s m Char => ParsecT s u m BEncode
bdictP = BDict <$> (skipMany1 (char 'd') *> (Map.fromList <$> manyTill keyPair (char 'e')))
  where
    keyPair = do

      key <- belementP
      val <- belementP

      case key of
        (BString k) -> pure (k, val)
        _           -> parserFail "Dictionary key must be a string!"

belementP :: Stream s m Char => ParsecT s u m BEncode
belementP = bdictP <|> blistP <|> bintegerP <|> bstringP

-- Utility

encode :: BEncode -> ByteString
encode (BString s) = (toS . show $ B.length s) <> ":" <> toS s
encode (BInteger i)  = "i" <> (toS . show) i <> "e"
encode (BList items) = "l" <> mconcat (encode <$> items) <> "e"
encode (BDict items)     = "d" <> Map.foldMapWithKey encodePair items <> "e"
  where
    encodePair :: ByteString -> BEncode -> ByteString
    encodePair key val = (toS . show $ B.length key) <> ":" <> key <> toS (encode val)

-- Encoding

class FromBEncode a where
  decodeB :: BEncode -> Maybe a

class ToBEncode a where
  encodeB :: a -> BEncode

-- Utils

decode :: ByteString -> Maybe BEncode
decode input =
  case parse belementP "" input of
    Right r -> Just r
    Left _  -> Nothing

getBStrings :: [BEncode] -> [ByteString]
getBStrings = mapMaybe (^? _BString)
