module Data.BEncodedSpec where

import           Prelude

import           Test.Hspec

import           Data.BEncoded (BEncoded (..), decode, encode)

sampleDictionary :: BEncoded
sampleDictionary = BDict
  [("name", BString "Emanuel")
  ,("age", BInteger 24)
  ,("shopping_list", BList [BString "eggs", BString "ham", BInteger 20])]

sampleList :: BEncoded
sampleList = BDict [
  ("my_list", BList [BString "eggs", BString "ham", BInteger 3000])]

spec :: Spec
spec =
  describe "test bencoding on different data structures" $ do
    it "should roundtrip a dictionary" $ do
      let (Just decodedDictionary) = decode . encode $ sampleDictionary
      decodedDictionary `shouldBe` sampleDictionary

    it "should roundtrip a list" $ do
      let (Just decodedList) = decode . encode $ sampleList
      decodedList `shouldBe` sampleList

    it "should decode and encode a list" $ do
      let str = BString "awesome"
      let encoded = encode str
      encoded `shouldBe` "7:awesome"
      let (Just decoded) = decode encoded
      decoded `shouldBe` str


