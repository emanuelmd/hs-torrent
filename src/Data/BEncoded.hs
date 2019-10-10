{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Data.BEncoded where

import           Prelude             hiding (many, optional, try, (<|>))

import           Control.Lens

import           Text.Parsec

import qualified Data.ByteString     as B
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust)

-- Data
data BEncoded = BString ByteString
              | BInteger Integer
              | BList [BEncoded]
              | BDict (Map.Map ByteString BEncoded)
              deriving (Show, Eq)

-- Parsing
numerP :: Stream s m Char => ParsecT s u m Integer
numerP = (fromJust . readMaybe) <$> (negativeN <|> positiveN)
  where
    negativeN = (:) <$> char '-' <*> many1 digit
    positiveN = many digit

bstringP :: Stream s m Char => ParsecT s u m BEncoded
bstringP = do
  n <- try numerP
  when (n < 1) $ parserFail "NumerP cannot be negative"
  rest <- char ':' *> count (fromIntegral n) anyChar
  return $ BString (toS rest)

bintegerP :: Stream s m Char => ParsecT s u m BEncoded
bintegerP = BInteger <$> ((skipMany1 $ char 'i') *> numerP <* char 'e')

blistP :: Stream s m Char => ParsecT s u m BEncoded
blistP = BList <$> (skipMany1 (char 'l') *> manyTill belementP (char 'e'))

bdictP :: Stream s m Char => ParsecT s u m BEncoded
bdictP = BDict <$> (skipMany1 (char 'd') *> (Map.fromList <$> manyTill keyPair (char 'e')))
  where
    keyPair = do

      key <- belementP
      val <- belementP

      case key of
        (BString k) -> pure (k, val)
        _           -> parserFail "Dictionary key must be a string!"

belementP :: Stream s m Char => ParsecT s u m BEncoded
belementP = bdictP <|> blistP <|> bintegerP <|> bstringP

-- Utility
encode :: BEncoded -> ByteString
encode (BString s) = (show $ B.length s) <> ":" <> (toS s)
encode (BInteger i)  = "i" <> (show i) <> "e"
encode (BList items) = "l" <> (mconcat $ encode <$> items) <> "e"
encode (BDict items)     = "d" <> (Map.foldMapWithKey encodePair items) <> "e"
  where
    encodePair :: ByteString -> BEncoded -> ByteString
    encodePair key val = toS $ (show $ B.length key) <> ":" <> key <> (toS $ encode val)

decode :: ByteString -> Maybe BEncoded
decode input =
  case parse belementP "" input of
    Right r -> Just r
    Left _  -> Nothing

-- Prisms

makePrisms ''BEncoded

-- Encoding

class FromBencoded a where
  decodeB :: BEncoded -> Maybe a

