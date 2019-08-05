{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import PyF
import Data.Text
import Data.ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Test formatting with different types" $ do
    it "String" $ do
      [fmt|hello {10:d}|] `shouldBe` ("hello 10" :: String)
    it "Text" $ do
      [fmt|hello {10:d}|] `shouldBe` ("hello 10" :: Text)
    it "ByteString" $ do
      [fmt|hello {10:d}|] `shouldBe` ("hello 10" :: ByteString)
