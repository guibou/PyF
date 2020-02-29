{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.ByteString
import Data.Text
import PyF
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let ten = 10 :: Int
  describe "Test formatting with different types" $ do
    it "String" $
      [fmt|hello {ten:d}|] `shouldBe` ("hello 10" :: String)
    it "Text" $
      [fmt|hello {ten:d}|] `shouldBe` ("hello 10" :: Text)
    it "ByteString" $
      [fmt|hello {ten:d}|] `shouldBe` ("hello 10" :: ByteString)
