{-# LANGUAGE FlexibleContexts #-}

module Data.BEncoded where

import           Prelude         hiding (many, optional, (<|>), try)

import           Text.Parsec

import qualified Data.ByteString as B
import           Data.Maybe      (fromJust)

data BEncoded = BString ByteString
              | BInteger Integer
              | BList [BEncoded]
              | BDict [(ByteString, BEncoded)]
              deriving (Show, Eq)

number :: Stream s m Char => ParsecT s u m Integer
number = (fromJust . readMaybe) <$> (negativeN <|> positiveN)
  where
    negativeN = (:) <$> char '-' <*> many1 digit
    positiveN = many digit

bstring :: Stream s m Char => ParsecT s u m BEncoded
bstring = do
  n <- try number
  when (n < 1) $ parserFail "Number cannot be negative"
  rest <- char ':' *> count (fromIntegral n) anyChar
  return $ BString (toS rest)

binteger :: Stream s m Char => ParsecT s u m BEncoded
binteger = BInteger <$> ((skipMany1 $ char 'i') *> number <* char 'e')

blist :: Stream s m Char => ParsecT s u m BEncoded
blist = BList <$> (skipMany1 (char 'l') *> manyTill belement (char 'e'))

bdict :: Stream s m Char => ParsecT s u m BEncoded
bdict = BDict <$> (skipMany1 (char 'd') *> manyTill keyPair (char 'e'))
  where
    keyPair = do

      key <- belement
      val <- belement

      case key of
        (BString k) -> pure (k, val)
        _             -> parserFail "Dictionary key must be a string!"

belement :: Stream s m Char => ParsecT s u m BEncoded
belement = bdict <|> blist <|> binteger <|> bstring

encode :: BEncoded -> Text
encode (BString s) = (show $ B.length s) <> ":" <> (toS s)
encode (BInteger i)  = "i" <> (show i) <> "e"
encode (BList items) = "l" <> (mconcat $ encode <$> items) <> "e"
encode (BDict items)     = "d" <> (mconcat $ encodePair <$> items) <> "e"
  where
    encodePair :: (ByteString, BEncoded) -> Text
    encodePair (key, val) = toS $ (show $ B.length key) <> ":" <> key <> (toS $ encode val)

decode :: Text -> Maybe BEncoded
decode input =
  case parse belement "" input of
    Right r -> Just r
    Left _  -> Nothing

sampleDictionary :: BEncoded
sampleDictionary = BDict
  [("name", BString "Emanuel")
  ,("age", BInteger 24)
  ,("shopping_list", BList [BString "eggs", BString "ham", BInteger 20])]

sampleList :: BEncoded
sampleList = BDict [
  ("my_list", BList [BString "eggs", BString "ham", BInteger 3000])]

