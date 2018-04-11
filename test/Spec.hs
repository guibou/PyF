{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import PyF

import Test.Hspec

main :: IO ()
main = hspec spec

anInt :: Int
anInt = 123

anIntNeg :: Int
anIntNeg = -123

aString :: String
aString = "hello"

aFloat :: Double
aFloat = 0.234

spec :: Spec
spec = do
  describe "types" $ do
    describe "default" $ do
      it "int" $ [fString|{anInt}|] `shouldBe` "123"
      it "float" $ [fString|{aFloat}|] `shouldBe` "0.234"
      it "string" $ [fString|{aString}|] `shouldBe` "hello"
      it "float precision" $ [fString|{aFloat:.1}|] `shouldBe` "0.2"
      it "string precision" $ [fString|{aString:.1}|] `shouldBe` "h"
      it "sign +" $ [fString|{aFloat:+}|] `shouldBe` "+0.234"
      it "sign space" $ [fString|{aFloat: }|] `shouldBe` " 0.234"
      it "sign neg" $ [fString|{anIntNeg:+}|] `shouldBe` "-123"
    describe "binary" $ do
      it "simple" $ [fString|{anInt:b}|] `shouldBe` "1111011"
      it "alt" $ [fString|{anInt:#b}|] `shouldBe` "0b1111011"
      it "sign" $ [fString|{anInt:+#b}|] `shouldBe` "+0b1111011"
    describe "character" $ do
      it "simple" $ [fString|{anInt:c}|] `shouldBe` "{"
    describe "decimal" $ do
      it "simple" $ [fString|{anInt:d}|] `shouldBe` "123"
      it "sign" $ [fString|{anInt:+d}|] `shouldBe` "+123"
    describe "exponentiel" $ do
      it "simple" $ [fString|{aFloat:e}|] `shouldBe` "2.340000e-1"
      it "precision" $ [fString|{aFloat:.1e}|] `shouldBe` "2.3e-1"
    describe "exponentiel caps" $ do
      it "simple" $ [fString|{aFloat:E}|] `shouldBe` "2.340000E-1"
      it "precision" $ [fString|{aFloat:.1E}|] `shouldBe` "2.3E-1"
    describe "general" $ do
      let smallF = 123.02
          bigF = 1234567890.23
      it "simple small" $ [fString|{smallF:g}|] `shouldBe` "123.020000"
      it "precision small" $ [fString|{smallF:.1g}|] `shouldBe` "123.0"
      it "simple big" $ [fString|{bigF:g}|] `shouldBe` "1.234568e9"
      it "precision big" $ [fString|{bigF:.1g}|] `shouldBe` "1.2e9"
    describe "general caps" $ do
      let smallF = 123.02
          bigF = 1234567890.23
      it "simple small" $ [fString|{smallF:G}|] `shouldBe` "123.020000"
      it "precision small" $ [fString|{smallF:.1G}|] `shouldBe` "123.0"
      it "simple big" $ [fString|{bigF:G}|] `shouldBe` "1.234568E9"
      it "precision big" $ [fString|{bigF:.1G}|] `shouldBe` "1.2E9"
    describe "fixed" $ do
      it "simple" $ [fString|{aFloat:f}|] `shouldBe` "0.234000"
      it "precision" $ [fString|{aFloat:.1f}|] `shouldBe` "0.2"
    describe "fixed caps" $ do
      it "simple" $ [fString|{aFloat:F}|] `shouldBe` "0.234000"
      it "precision" $ [fString|{aFloat:.1F}|] `shouldBe` "0.2"
    describe "octal" $ do
      it "simple" $ [fString|{anInt:o}|] `shouldBe` "173"
      it "alt" $ [fString|{anInt:#o}|] `shouldBe` "0o173"
    describe "string" $ do
      it "string" $ [fString|{aString:s}|] `shouldBe` "hello"
      it "precision" $ [fString|{aString:.2s}|] `shouldBe` "he"
    describe "hex" $ do
      it "simple" $ [fString|{anInt:x}|] `shouldBe` "7b"
      it "alt" $ [fString|{anInt:#x}|] `shouldBe` "0x7b"
    describe "hex caps" $ do
      it "simple" $ [fString|{anInt:X}|] `shouldBe` "7B"
      it "alt" $ [fString|{anInt:#X}|] `shouldBe` "0X7B"
    describe "percent" $ do
      it "simple" $ [fString|{aFloat:%}|] `shouldBe` "23.400000%"
      it "precision" $ [fString|{aFloat:.2%}|] `shouldBe` "23.40%"
    describe "padding" $ do
      describe "default char" $ do
        it "left" $ [fString|{aString:<10}|] `shouldBe` "hello     "
        it "right" $ [fString|{aString:>10}|] `shouldBe` "     hello"
        it "center" $ [fString|{aString:^10}|] `shouldBe` "  hello   "
      describe "a char" $ do
        it "left" $ [fString|{aString:-<10}|] `shouldBe` "hello-----"
        it "right" $ [fString|{aString:->10}|] `shouldBe` "-----hello"
        it "center" $ [fString|{aString:-^10}|] `shouldBe` "--hello---"
      describe "inside" $ do
        it "inside" $ [fString|{anInt:=+10}|] `shouldBe` "+      123"
        it "inside" $ [fString|{anInt:=10}|] `shouldBe` "       123"
        it "inside" $ [fString|{- anInt:=10}|] `shouldBe` "-      123"
        it "inside" $ [fString|{- anInt:|= 10}|] `shouldBe` "-||||||123"
        it "inside" $ [fString|{anInt:|= 10}|] `shouldBe` " ||||||123"
    describe "NaN" $ do
        it "nan" $ [fString|{0/0}|] `shouldBe` "nan"
        it "nan f" $ [fString|{0/0:f}|] `shouldBe` "nan"
        it "nan e" $ [fString|{0/0:e}|] `shouldBe` "nan"
        it "nan g" $ [fString|{0/0:g}|] `shouldBe` "nan"
        it "nan F" $ [fString|{0/0:F}|] `shouldBe` "NAN"
        it "nan G" $ [fString|{0/0:G}|] `shouldBe` "NAN"
        it "nan E" $ [fString|{0/0:E}|] `shouldBe` "NAN"
    describe "Infinite" $ do
        it "infinite" $ [fString|{1/0}|] `shouldBe` "inf"
        it "infinite f" $ [fString|{1/0:f}|] `shouldBe` "inf"
        it "infinite e" $ [fString|{1/0:e}|] `shouldBe` "inf"
        it "infinite g" $ [fString|{1/0:g}|] `shouldBe` "inf"
        it "infinite F" $ [fString|{1/0:F}|] `shouldBe` "INF"
        it "infinite G" $ [fString|{1/0:G}|] `shouldBe` "INF"
        it "infinite E" $ [fString|{1/0:E}|] `shouldBe` "INF"

  describe "complex" $ do
    it "works with many things at once" $
      let
        name = "Guillaume"
        age = 31
        euroToFrancs = 6.55957
      in
        [fString|hello {name} you are {age} years old and the conversion rate of euro is {euroToFrancs:.2}|] `shouldBe` ("hello Guillaume you are 31 years old and the conversion rate of euro is 6.56")


  describe "error reporting" $ do
    pure () -- TODO: find a way to test error reporting

  describe "sub expressions" $ do
    it "works" $ do
      [fString|2pi = {2 * pi:.2}|] `shouldBe` "2pi = 6.28"

  describe "escape strings" $ do
    it "works" $ do
      [fString|hello \n\b|] `shouldBe` "hello \n\b"
