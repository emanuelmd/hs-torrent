module Data.BEncodedSpec where

import           Prelude

import           Test.Hspec

import           Data.BEncode (BEncode (..), decode, encode)
import qualified Data.Map      as Map

sampleDictionary :: BEncode
sampleDictionary = BDict . Map.fromList $
  [("name", BString "Emanuel")
  ,("age", BInteger 24)
  ,("shopping_list", BList [BString "eggs", BString "ham", BInteger 20])]

sampleList :: BEncode
sampleList = BList [BString "eggs", BString "ham", BInteger 3000]

spec :: Spec
spec =
  describe "test bencoding on different data structures" $ do
    it "should roundtrip a dictionary" $ do
      let (Just decodedDictionary) = decode . encode $ sampleDictionary
      decodedDictionary `shouldBe` sampleDictionary

    it "should roundtrip a list" $ do
      let (Just decodedList) = decode . encode $ sampleList
      decodedList `shouldBe` sampleList

    it "should decode and encode a string" $ do
      let str = BString "awesome"
      let encoded = encode str
      encoded `shouldBe` "7:awesome"
      let (Just decoded) = decode encoded
      decoded `shouldBe` str

    it "should decode strings" $ do
      let (Just (BString decoded)) = decode "4:spam"
      decoded `shouldBe` "spam"

    it "should decode numbers" $ do
      let (Just (BInteger decoded)) = decode "i32e"
      decoded `shouldBe` 32

    it "should decode lists" $ do
      let (Just (BList [BInteger integ, BString str, BList l])) = decode "li20e3:heylee"
      integ `shouldBe` 20
      str `shouldBe` "hey"
      l `shouldBe` []
