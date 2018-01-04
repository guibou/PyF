{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import PyF

import Test.Hspec
import Data.Text.Lazy as Text

main :: IO ()
main = hspec spec

anInt :: Int
anInt = 123

aString :: String
aString = "hello"

aFloat :: Double
aFloat = 0.234

spec :: Spec
spec = do
  describe "types" $ do
    describe "default" $ do
      it "int" $ [f|{anInt}|] `shouldBe` "123"
      it "float" $ [f|{aFloat}|] `shouldBe` "0.234"
      it "string" $ [f|{aString}|] `shouldBe` "hello"
      it "float precision" $ [f|{aFloat:.1}|] `shouldBe` "0.2"
      it "string precision" $ [f|{aString:.1}|] `shouldBe` "h"
    describe "binary" $ do
      it "simple" $ [f|{anInt:b}|] `shouldBe` "1111011"
      it "alt" $ [f|{anInt:#b}|] `shouldBe` "0b1111011"
    describe "character" $ do
      it "simple" $ [f|{anInt:c}|] `shouldBe` "{"
    describe "decimal" $ do
      it "simple" $ [f|{anInt:d}|] `shouldBe` "123"
    describe "exponentiel" $ do
      it "simple" $ [f|{aFloat:e}|] `shouldBe` "2.340000e-1"
      it "precision" $ [f|{aFloat:.1e}|] `shouldBe` "2.3e-1"
    describe "exponentiel caps" $ do
      it "simple" $ [f|{aFloat:E}|] `shouldBe` "2.340000E-1"
      it "precision" $ [f|{aFloat:.1E}|] `shouldBe` "2.3E-1"
    describe "general" $ do
      let smallF = 123.02 :: Double
          bigF = 1234567890.23 :: Double
      it "simple small" $ [f|{smallF:g}|] `shouldBe` "123.020000"
      it "precision small" $ [f|{smallF:.1g}|] `shouldBe` "123.0"
      it "simple big" $ [f|{bigF:g}|] `shouldBe` "1.234568e9"
      it "precision big" $ [f|{bigF:.1g}|] `shouldBe` "1.2e9"
    describe "general caps" $ do
      let smallF = 123.02 :: Double
          bigF = 1234567890.23 :: Double
      it "simple small" $ [f|{smallF:G}|] `shouldBe` "123.020000"
      it "precision small" $ [f|{smallF:.1G}|] `shouldBe` "123.0"
      it "simple big" $ [f|{bigF:G}|] `shouldBe` "1.234568E9"
      it "precision big" $ [f|{bigF:.1G}|] `shouldBe` "1.2E9"
    describe "fixed" $ do
      it "simple" $ [f|{aFloat:f}|] `shouldBe` "0.234000"
      it "precision" $ [f|{aFloat:.1f}|] `shouldBe` "0.2"
    describe "fixed caps" $ do
      it "simple" $ [f|{aFloat:F}|] `shouldBe` "0.234000"
      it "precision" $ [f|{aFloat:.1F}|] `shouldBe` "0.2"
    describe "octal" $ do
      it "simple" $ [f|{anInt:o}|] `shouldBe` "173"
      it "alt" $ [f|{anInt:#o}|] `shouldBe` "0o173"
    describe "string" $ do
      it "string" $ [f|{aString:s}|] `shouldBe` "hello"
      it "precision" $ [f|{aString:.2s}|] `shouldBe` "he"
    describe "hex" $ do
      it "simple" $ [f|{anInt:x}|] `shouldBe` "7b"
      it "alt" $ [f|{anInt:#x}|] `shouldBe` "0x7b"
    describe "hex caps" $ do
      it "simple" $ [f|{anInt:X}|] `shouldBe` "7B"
      it "alt" $ [f|{anInt:#X}|] `shouldBe` "0X7B"
    describe "percent" $ do
      it "simple" $ [f|{aFloat:%}|] `shouldBe` "23.400000%"
      it "precision" $ [f|{aFloat:.2%}|] `shouldBe` "23.40%"
    describe "padding" $ do
      describe "default char" $ do
        it "left" $ [f|{aString:<10}|] `shouldBe` "hello     "
        it "right" $ [f|{aString:>10}|] `shouldBe` "     hello"
        it "center" $ [f|{aString:^10}|] `shouldBe` "   hello  "
      describe "a char" $ do
        it "left" $ [f|{aString:-<10}|] `shouldBe` "hello-----"
        it "right" $ [f|{aString:->10}|] `shouldBe` "-----hello"
        it "center" $ [f|{aString:-^10}|] `shouldBe` "---hello--"

  describe "complex" $ do
    it "works with many things at once" $
      let
        name = "Guillaume" :: String
        age = 31 :: Int
        euroToFrancs = 6.55957 :: Double
      in
        [f|hello {name} you are {age} years old and the conversion rate of euro is {euroToFrancs:.2}|] `shouldBe` ("hello Guillaume you are 31 years old and the conversion rate of euro is 6.56")


  describe "error reporting" $ do
    pure () -- TODO: find a way to test error reporting
