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
      [f|hello {10:d}|] `shouldBe` ("hello 10" :: String)
    it "Text" $ do
      [f|hello {10:d}|] `shouldBe` ("hello 10" :: Text)
    it "ByteString" $ do
      [f|hello {10:d}|] `shouldBe` ("hello 10" :: ByteString)