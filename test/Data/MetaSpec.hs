module Data.MetaSpec where

import           Prelude

import           Test.Hspec

import           Data.BEncoded (BEncoded (..), decodeB)
import qualified Data.Map      as Map
import           Data.Meta     (FileInfo (..), Info (..), MetaInfo (..))

fileB :: BEncoded
fileB = BDict $ Map.fromList
  [("length", BInteger 200)
  ,("path", BList [BString "first", BString "second"])]

file' :: FileInfo
file' = FileInfo 200 ["first", "second"]

infoB :: BEncoded
infoB = BDict $ Map.fromList
  [("name", BString "name")
  ,("piece length", BInteger 200)
  ,("pieces", BString "pieces")
  ,("files", BList [fileB])]

info' :: Info
info' = Info "name" 200 "pieces" Nothing [file']

metaInfoB :: BEncoded
metaInfoB = BDict $ Map.fromList
  [("announce", BString "http://whatever")
  ,("created by", BString "me")
  ,("info", infoB)]

metaInfo' :: MetaInfo
metaInfo' = MetaInfo "http://whatever" "me" info'

spec :: Spec
spec =
  describe "Encoding/Decoding Meta structures" $ do

    it "should decode FileInfo" $ do
      let (Just decoded) = decodeB fileB :: Maybe FileInfo
      decoded `shouldBe` file'

    it "should decode Info" $ do
      let (Just decoded) = decodeB infoB :: Maybe Info
      decoded `shouldBe` info'

    it "should decode MetaInfo" $ do
      let (Just decoded) = decodeB metaInfoB :: Maybe MetaInfo
      decoded `shouldBe` metaInfo'

