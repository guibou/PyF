{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This warning is disabled because any expression with literal leads to it.
{-# OPTIONS -Wno-type-defaults #-}

import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import qualified Data.Ratio
import Data.String
import qualified Data.Text
import qualified Data.Text as Text
import qualified Data.Text.Lazy
import qualified Data.Time
import GHC.OverloadedLabels
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import PyF
import SpecCustomDelimiters
import Test.Hspec

main :: IO ()
main = hspec $ parallel spec

newtype FooFloating t = FooFloating t
  deriving newtype (Show, RealFloat, RealFrac, Floating, Fractional, Real, Enum, Num, Ord, Eq, PyfFormatFractional)

newtype FooIntegral = FooIntegral Integer
  deriving newtype (Show, Integral, Real, Enum, Num, Ord, Eq)

data Foo = Foo

data FooDefault = FooDefault
  deriving (Show)

instance PyFToString Foo where
  pyfToString Foo = "I'm a Foo"

type instance PyFClassify Foo = 'PyFString

type instance PyFClassify (FooFloating t) = 'PyFFractional

type instance PyFClassify FooIntegral = 'PyFIntegral

type instance PyFClassify FooDefault = 'PyFString

-- Data type to test overloaded labels
data V (a :: Symbol) = V

instance (KnownSymbol a) => Show (V a) where
  show V = "V=" ++ symbolVal (Proxy @a)

instance (a ~ a') => IsLabel a (V a') where
  fromLabel = V

showV :: (KnownSymbol a) => V a -> String
showV = show

globalName :: String
globalName = "Valérian"

spec :: Spec
spec = do
  describe "simple with external variable" $ do
    let anInt = 123
        aFloat = 0.234
        aString = "hello"
    it "int" $ [fmt|{anInt}|] `shouldBe` "123"
    it "float" $ [fmt|{aFloat}|] `shouldBe` "0.234"
    it "string" $ [fmt|{aString}|] `shouldBe` "hello"
  describe "only expression" $ do
    describe "default" $ do
      it "int" $ [fmt|{123}|] `shouldBe` "123"
      it "float" $ [fmt|{0.234}|] `shouldBe` "0.234"
      it "string" $ [fmt|{"hello"}|] `shouldBe` "hello"
      it "float precision" $ [fmt|{0.234:.1}|] `shouldBe` "0.2"
      it "string precision" $ [fmt|{"hello":.1}|] `shouldBe` "h"
      it "sign +" $ [fmt|{0.234:+}|] `shouldBe` "+0.234"
      it "sign space" $ [fmt|{0.234: }|] `shouldBe` " 0.234"
      it "sign neg" $ [fmt|{-123:+}|] `shouldBe` "-123"
    describe "binary" $ do
      it "simple" $ [fmt|{123:b}|] `shouldBe` "1111011"
      it "alt" $ [fmt|{123:#b}|] `shouldBe` "0b1111011"
      it "sign" $ [fmt|{123:+#b}|] `shouldBe` "+0b1111011"
    describe "character" $
      it "simple" $
        [fmt|{123:c}|] `shouldBe` "{"
    describe "decimal" $ do
      it "simple" $ [fmt|{123:d}|] `shouldBe` "123"
      it "sign" $ [fmt|{123:+d}|] `shouldBe` "+123"
    describe "exponentiel" $ do
      it "simple > 1" $ [fmt|{234.0:e}|] `shouldBe` "2.340000e+02"
      it "precision > 1" $ [fmt|{234.0:.1e}|] `shouldBe` "2.3e+02"
      it "simple < 1" $ [fmt|{0.234:e}|] `shouldBe` "2.340000e-01"
      it "precision < 1 " $ [fmt|{0.234:.1e}|] `shouldBe` "2.3e-01"
    describe "exponentiel caps" $ do
      it "simple > 1" $ [fmt|{234.0:E}|] `shouldBe` "2.340000E+02"
      it "precision > 1" $ [fmt|{234.0:.1E}|] `shouldBe` "2.3E+02"
      it "simple < 1" $ [fmt|{0.234:E}|] `shouldBe` "2.340000E-01"
      it "precision < 1 " $ [fmt|{0.234:.1E}|] `shouldBe` "2.3E-01"
    describe "general" $ do
      it "simple small" $ [fmt|{123.02:g}|] `shouldBe` "123.020000"
      it "precision small" $ [fmt|{123.02:.1g}|] `shouldBe` "123.0"
      it "simple big" $ [fmt|{1234567890.23:g}|] `shouldBe` "1.234568e+09"
      it "precision big" $ [fmt|{1234567890.23:.1g}|] `shouldBe` "1.2e+09"
    describe "general caps" $ do
      it "simple small" $ [fmt|{123.02:G}|] `shouldBe` "123.020000"
      it "precision small" $ [fmt|{123.02:.1G}|] `shouldBe` "123.0"
      it "simple big" $ [fmt|{1234567890.23:G}|] `shouldBe` "1.234568E+09"
      it "precision big" $ [fmt|{1234567890.23:.1G}|] `shouldBe` "1.2E+09"
    describe "fixed" $ do
      it "simple" $ [fmt|{0.234:f}|] `shouldBe` "0.234000"
      it "precision" $ [fmt|{0.234:.1f}|] `shouldBe` "0.2"
    describe "fixed caps" $ do
      it "simple" $ [fmt|{0.234:F}|] `shouldBe` "0.234000"
      it "precision" $ [fmt|{0.234:.1F}|] `shouldBe` "0.2"
    describe "octal" $ do
      it "simple" $ [fmt|{123:o}|] `shouldBe` "173"
      it "alt" $ [fmt|{123:#o}|] `shouldBe` "0o173"
    describe "string" $ do
      it "string" $ [fmt|{"hello":s}|] `shouldBe` "hello"
      it "precision" $ [fmt|{"hello":.2s}|] `shouldBe` "he"
    describe "hex" $ do
      it "simple" $ [fmt|{123:x}|] `shouldBe` "7b"
      it "alt" $ [fmt|{123:#x}|] `shouldBe` "0x7b"
    describe "hex caps" $ do
      it "simple" $ [fmt|{123:X}|] `shouldBe` "7B"
      it "alt" $ [fmt|{123:#X}|] `shouldBe` "0X7B"
    describe "percent" $ do
      it "simple" $ [fmt|{0.234:%}|] `shouldBe` "23.400000%"
      it "precision" $ [fmt|{0.234:.2%}|] `shouldBe` "23.40%"
    describe "string truncating" $
      it "works" $
        [fmt|{"hello":.3}|] `shouldBe` "hel"
    describe "padding" $ do
      describe "default char" $ do
        it "left" $ [fmt|{"hello":<10}|] `shouldBe` "hello     "
        it "right" $ [fmt|{"hello":>10}|] `shouldBe` "     hello"
        it "center" $ [fmt|{"hello":^10}|] `shouldBe` "  hello   "
      describe "a char" $ do
        it "left" $ [fmt|{"hello":-<10}|] `shouldBe` "hello-----"
        it "right" $ [fmt|{"hello":->10}|] `shouldBe` "-----hello"
        it "center" $ [fmt|{"hello":-^10}|] `shouldBe` "--hello---"
      describe "inside" $ do
        it "inside" $ [fmt|{123:=+10}|] `shouldBe` "+      123"
        it "inside" $ [fmt|{123:=10}|] `shouldBe` "       123"
        it "inside" $ [fmt|{- 123:=10}|] `shouldBe` "-      123"
        it "inside" $ [fmt|{- 123:|= 10}|] `shouldBe` "-||||||123"
        it "inside" $ [fmt|{123:|= 10}|] `shouldBe` " ||||||123"
      describe "default padding" $ do
        it "floating" $ [fmt|{1:10f}|] `shouldBe` "  1.000000"
        it "integral" $ [fmt|{1:10d}|] `shouldBe` "         1"
        it "string" $ [fmt|{"h":10s}|] `shouldBe` "h         "
        it "default" $ [fmt|{1:10}|] `shouldBe` "         1"
        it "default" $ [fmt|{1.0:10}|] `shouldBe` "       1.0"
        it "default" $ [fmt|{"h":10}|] `shouldBe` "h         "
    describe "NaN" $ do
      describe "float" $ do
        let nan = 0.0 / 0 :: Float
        it "nan" $ [fmt|{nan}|] `shouldBe` "nan"
        it "nan f" $ [fmt|{nan:f}|] `shouldBe` "nan"
        it "nan e" $ [fmt|{nan:e}|] `shouldBe` "nan"
        it "nan g" $ [fmt|{nan:g}|] `shouldBe` "nan"
        it "nan F" $ [fmt|{nan:F}|] `shouldBe` "NAN"
        it "nan G" $ [fmt|{nan:G}|] `shouldBe` "NAN"
        it "nan E" $ [fmt|{nan:E}|] `shouldBe` "NAN"
      describe "double" $ do
        let nan = 0.0 / 0 :: Double
        it "nan" $ [fmt|{nan}|] `shouldBe` "nan"
        it "nan f" $ [fmt|{nan:f}|] `shouldBe` "nan"
        it "nan e" $ [fmt|{nan:e}|] `shouldBe` "nan"
        it "nan g" $ [fmt|{nan:g}|] `shouldBe` "nan"
        it "nan F" $ [fmt|{nan:F}|] `shouldBe` "NAN"
        it "nan G" $ [fmt|{nan:G}|] `shouldBe` "NAN"
        it "nan E" $ [fmt|{nan:E}|] `shouldBe` "NAN"
    describe "Infinite" $ do
      describe "float" $ do
        let inf = 1.0 / 0 :: Float
        it "infinite" $ [fmt|{inf}|] `shouldBe` "inf"
        it "infinite f" $ [fmt|{inf:f}|] `shouldBe` "inf"
        it "infinite e" $ [fmt|{inf:e}|] `shouldBe` "inf"
        it "infinite g" $ [fmt|{inf:g}|] `shouldBe` "inf"
        it "infinite F" $ [fmt|{inf:F}|] `shouldBe` "INF"
        it "infinite G" $ [fmt|{inf:G}|] `shouldBe` "INF"
        it "infinite E" $ [fmt|{inf:E}|] `shouldBe` "INF"
      describe "double" $ do
        let inf = 1.0 / 0 :: Double
        it "infinite" $ [fmt|{inf}|] `shouldBe` "inf"
        it "infinite f" $ [fmt|{inf:f}|] `shouldBe` "inf"
        it "infinite e" $ [fmt|{inf:e}|] `shouldBe` "inf"
        it "infinite g" $ [fmt|{inf:g}|] `shouldBe` "inf"
        it "infinite F" $ [fmt|{inf:F}|] `shouldBe` "INF"
        it "infinite G" $ [fmt|{inf:G}|] `shouldBe` "INF"
        it "infinite E" $ [fmt|{inf:E}|] `shouldBe` "INF"
    describe "Grouping" $ do
      it "groups int" $ [fmt|{123456789:,d}|] `shouldBe` "123,456,789"
      it "groups int with _" $ [fmt|{123456789:_d}|] `shouldBe` "123_456_789"
      it "groups float" $ [fmt|{123456789.234:,f}|] `shouldBe` "123,456,789.234000"
      it "groups bin" $ [fmt|{123456789:_b}|] `shouldBe` "111_0101_1011_1100_1101_0001_0101"
      it "groups hex" $ [fmt|{123456789:_x}|] `shouldBe` "75b_cd15"
      it "groups oct" $ [fmt|{123456789:_o}|] `shouldBe` "7_2674_6425"
    describe "negative zero" $ do
      it "f" $ [fmt|{-0.0:f}|] `shouldBe` "-0.000000"
      it "e" $ [fmt|{-0.0:e}|] `shouldBe` "-0.000000e+00"
      it "g" $ [fmt|{-0.0:g}|] `shouldBe` "-0.000000"
      it "F" $ [fmt|{-0.0:F}|] `shouldBe` "-0.000000"
      it "G" $ [fmt|{-0.0:G}|] `shouldBe` "-0.000000"
      it "E" $ [fmt|{-0.0:E}|] `shouldBe` "-0.000000E+00"
    describe "0" $ do
      it "works" $ [fmt|{123:010}|] `shouldBe` "0000000123"
      it "works with sign" $ [fmt|{-123:010}|] `shouldBe` "-000000123"
      it "accept mode override" $ [fmt|{-123:<010}|] `shouldBe` "-123000000"
      it "accept mode and char override" $ [fmt|{-123:.<010}|] `shouldBe` "-123......"
    describe "no digit no dot" $ do
      it "f" $ [fmt|{1.0:.0f}|] `shouldBe` "1"
      it "e" $ [fmt|{1.0:.0e}|] `shouldBe` "1e+00"
      it "g" $ [fmt|{1.0:.0g}|] `shouldBe` "1"
      it "E" $ [fmt|{1.0:.0E}|] `shouldBe` "1E+00"
      it "G" $ [fmt|{1.0:.0G}|] `shouldBe` "1"
      it "percent" $ [fmt|{1.0:.0%}|] `shouldBe` "100%"
    describe "no digit alt -> dot" $ do
      it "f" $ [fmt|{1.0:#.0f}|] `shouldBe` "1."
      it "e" $ [fmt|{1.0:#.0e}|] `shouldBe` "1.e+00"
      it "g" $ [fmt|{1.0:#.0g}|] `shouldBe` "1."
      it "E" $ [fmt|{1.0:#.0E}|] `shouldBe` "1.E+00"
      it "G" $ [fmt|{1.0:#.0G}|] `shouldBe` "1."
      it "percent" $ [fmt|{1.0:#.0%}|] `shouldBe` "100.%"
  describe "complex" $
    it "works with many things at once" $
      let name = "Guillaume"
          age = 31
          euroToFrancs = 6.55957
       in [fmt|hello {name} you are {age} years old and the conversion rate of euro is {euroToFrancs:.2}|] `shouldBe` "hello Guillaume you are 31 years old and the conversion rate of euro is 6.56"
  describe "error reporting" $
    pure () -- TODO: find a way to test error reporting
  describe "sub expressions" $
    it "works" $
      [fmt|2pi = {2 * pi:.2}|] `shouldBe` "2pi = 6.28"
  describe "escape strings" $
    it "works" $
      [fmt|hello \n\b|] `shouldBe` "hello \n\b"
  describe "variable precision" $
    it "works" $
      do
        let n = 3 :: Int
        [fmt|{pi:.{n}}|] `shouldBe` "3.142"
  describe "variable padding" $
    it "works" $
      do
        let n = 5 :: Integer
        [fmt|Bonjour {'a':>{n}}|] `shouldBe` "Bonjour     a"
  it "escape chars" $
    [fmt|}}{{}}{{|] `shouldBe` "}{}{"
  describe "custom delimiters" $ do
    it "works" $
      [myCustomFormatter|2 * pi = @2*pi:.2f!|] `shouldBe` "2 * pi = 6.28"
    it "escape chars" $
      [myCustomFormatter|@@!!@@!!|] `shouldBe` "@!@!"
    it "works for custom precision" $
      [myCustomFormatter|@pi:.@2!!|] `shouldBe` "3.14"
  describe "empty line" $
    it "works" $
      [fmt||] `shouldBe` ""
  describe "multi line escape" $ do
    it "works" $
      [fmt|\
- a
- b
\
|]
        `shouldBe` "- a\n- b\n"
    it "escapes in middle of line" $
      [fmt|Example goes \
here!|]
        `shouldBe` "Example goes here!"
    it "escapes a lot of things" $
      [fmt|\
I'm a line with \n and \\ and a correct line
ending, but that one is escaped\
And I'm escaping before and after: \\{pi:.3f}\\
yeah\
|]
        `shouldBe` "I'm a line with \n and \\ and a correct line\nending, but that one is escapedAnd I'm escaping before and after: \\3.142\\\nyeah"
    it "escapes" $
      [fmt|\\
- a
- b
\
|]
        `shouldBe` "\\\n- a\n- b\n"
  describe "empty trailing value" $
    it "String" $
      ( [fmt|\
{pi:.0}
|] ::
          String
      )
        `shouldBe` "3\n"
  describe "language extensions" $ do
    it "parses @Int" $
      [fmt|hello {show @Int 10}|] `shouldBe` "hello 10"
    it "parses @_" $
      [fmt|hello {show @_ 10}|] `shouldBe` "hello 10"
    it "parses BinaryLiterals" $
      [fmt|hello {0b1111}|] `shouldBe` "hello 15"
    it "OverloadedLabels works" $
      [fmt|{showV #abc}|] `shouldBe` "V=abc"
  describe "custom types" $ do
    it "works with integral" $
      [fmt|{FooIntegral 10:d}|] `shouldBe` "10"
    it "works with floating" $
      [fmt|{FooFloating 25.123:f}|] `shouldBe` "25.123000"
    it "works with string" $ do
      [fmt|{Foo:s}|] `shouldBe` "I'm a Foo"
      [fmt|{FooDefault:s}|] `shouldBe` "FooDefault"
    it "works with classify" $ do
      [fmt|{Foo}|] `shouldBe` "I'm a Foo"
      [fmt|{FooIntegral 100}|] `shouldBe` "100"
      let fooDouble = FooFloating (100.123 :: Double) in [fmt|{fooDouble}|] `shouldBe` "100.123"
      let fooFloat = FooFloating (100.123 :: Float) in [fmt|{fooFloat}|] `shouldBe` "100.123"
      [fmt|{FooDefault}|] `shouldBe` "FooDefault"
  describe "Special syntax " $ do
    it "[] is correct expression" $ do
      [fmt|{[] @Char}|] `shouldBe` ""
    it "() is correct expression" $ do
      [fmt|{const "STRING" ()}|] `shouldBe` "STRING"

  describe "instances" $ do
    describe "default" $ do
      it "bytestring" $ do
        let x = "hello" :: Data.ByteString.ByteString in [fmt|{x}|] `shouldBe` "hello"
      it "bytestring lazy" $ do
        let x = "hello" :: Data.ByteString.Lazy.ByteString in [fmt|{x}|] `shouldBe` "hello"
      it "bytestring char 8 lazy" $ do
        let x = "hello" :: Data.ByteString.Lazy.Char8.ByteString in [fmt|{x}|] `shouldBe` "hello"
      it "bytestring char 8" $ do
        let x = "hello" :: Data.ByteString.Char8.ByteString in [fmt|{x}|] `shouldBe` "hello"
      it "text" $ do
        let x = "hello" :: Data.Text.Text in [fmt|{x}|] `shouldBe` "hello"
      it "lazy text" $ do
        let x = "hello" :: Data.Text.Lazy.Text in [fmt|{x}|] `shouldBe` "hello"
      it "DiffTime" $ do
        let x = 3 :: Data.Time.DiffTime in [fmt|{x}|] `shouldBe` "3.0"
      it "NominalDiffTime" $ do
        let x = 3 :: Data.Time.NominalDiffTime in [fmt|{x}|] `shouldBe` "3.0"
      it "Ratio" $ do
        let x = 3 :: Data.Ratio.Ratio Int in [fmt|{x}|] `shouldBe` "3.0"
      it "Int" $ do
        let x = 3 :: Int in [fmt|{x}|] `shouldBe` "3"
      it "Char" $ do
        let x = 'a' in [fmt|{x}|] `shouldBe` "a"
    describe "forced" $ do
      it "bytestring" $ do
        let x = "hello" :: Data.ByteString.ByteString in [fmt|{x:s}|] `shouldBe` "hello"
      it "bytestring lazy" $ do
        let x = "hello" :: Data.ByteString.Lazy.ByteString in [fmt|{x:s}|] `shouldBe` "hello"
      it "bytestring char 8 lazy" $ do
        let x = "hello" :: Data.ByteString.Lazy.Char8.ByteString in [fmt|{x:s}|] `shouldBe` "hello"
      it "bytestring char 8" $ do
        let x = "hello" :: Data.ByteString.Char8.ByteString in [fmt|{x:s}|] `shouldBe` "hello"
      it "text" $ do
        let x = "hello" :: Data.Text.Text in [fmt|{x:s}|] `shouldBe` "hello"
      it "lazy text" $ do
        let x = "hello" :: Data.Text.Text in [fmt|{x:s}|] `shouldBe` "hello"
      it "DiffTime" $ do
        let x = 3 :: Data.Time.DiffTime in [fmt|{x:.2f}|] `shouldBe` "3.00"
      it "NominalDiffTime" $ do
        let x = 3 :: Data.Time.NominalDiffTime in [fmt|{x:.2f}|] `shouldBe` "3.00"
      it "Ratio" $ do
        let x = 3 :: Data.Ratio.Ratio Int in [fmt|{x:.2f}|] `shouldBe` "3.00"
      describe "int" $ do
        it "decimal" $ do
          let x = 3 :: Int in [fmt|{x:d}|] `shouldBe` "3"
        it "fractional" $ do
          let x = 3 :: Int in [fmt|{x:.2f}|] `shouldBe` "3.00"
      describe "Char" $ do
        it "string" $ do
          let x = 'a' in [fmt|{x:s}|] `shouldBe` "a"
        it "int" $ do
          let x = 'a' in [fmt|{x:d}|] `shouldBe` "97"

  describe "syntax" $ do
    describe "name" $ do
      it "qualified" $ do
        [fmt|{List.sort [1,2,3]:s}|] `shouldBe` "[1,2,3]"
    describe "literals" $ do
      it "list" $ do
        [fmt|{[1,2,3]:s}|] `shouldBe` "[1,2,3]"
      describe "arith seq" $ do
        it "from" $ [fmt|{take 3 [1..]:s}|] `shouldBe` "[1,2,3]"
        it "fromthen" $ [fmt|{take 3 [1, 3..]:s}|] `shouldBe` "[1,3,5]"
        it "fromto" $ [fmt|{[1..3]:s}|] `shouldBe` "[1,2,3]"
        it "fromthento" $ [fmt|{[1,3..5]:s}|] `shouldBe` "[1,3,5]"

    it "lambda" $ do
      [fmt|{(\x -> 2 * x) 10}|] `shouldBe` "20"
    it "section" $ do
      [fmt|{(100/) 10:.0f}|] `shouldBe` "10"
      [fmt|{(/2) 10:.0f}|] `shouldBe` "5"
    it "tuples" $ do
      [fmt|{fst (1, 2)}|] `shouldBe` "1"

  -- Disabled because it does not build with GHC < 8.10
  -- xit "tuples section" $ do
  -- [fmt|{fst ((,2) 1)}|] `shouldBe` "1"

  describe "multiline trimming" $ do
    it "works with overloading" $ do
      [fmtTrim|hello|] `shouldBe` ("hello" :: Data.Text.Text)
    it "overloading in overloading" $ do
      let foo = [fmtTrim|hello {10}|]
      -- foo `shouldBe` "hello 10"
      [fmtTrim|biz {foo} goodbye|] `shouldBe` ("biz hello 10 goodbye" :: Data.Text.Text)
    it "do not fail on trailing ignore line return" $ do
      [fmtTrim|
      hello\

      |]
        `shouldBe` "hello\n"
    it "do not take too much indent in account" $ do
      [fmtTrim|
      hello
      - a
        - b
      - c
      |]
        `shouldBe` "hello\n- a\n  - b\n- c\n"
    it "works with empty lines" $ do
      [fmtTrim|
      hello


      |]
        `shouldBe` "hello\n\n\n"
    it "works with empty last lines" $ do
      [fmtTrim|
      hello


|]
        `shouldBe` "hello\n\n\n"
    it "works" $ do
      [fmtTrim|
                  hello
                  - a
                   - b

                  - c
      |]
        `shouldBe` "hello\n- a\n - b\n\n- c\n"
    it "works with replacement" $ do
      [fmtTrim|
                  hello
                  - a
                   - {pi:.2}|]
        `shouldBe` "hello\n- a\n - 3.14"
    it "Do not ignore not indented lines" $ do
      [fmtTrim|  hello
- a
 - {pi:.2}|]
        `shouldBe` "  hello\n- a\n - 3.14"

    it "works with multiline" $ do
      [fmtTrim|
                  hello
                  - a
                   - {
                      2 + 2
                   :d}|]
        `shouldBe` "hello\n- a\n - 4"

    it "Do not touch single lines" $ do
      [fmtTrim|  hello|] `shouldBe` "  hello"
    it "trim when there is a linebreak" $ do
      -- https://github.com/guibou/PyF/issues/141
      [fmtTrim|
          Cannot convert formula 2.0 * exponent(unit=s, value=1.0) which has unit dimensionless to\
           unit dimensionless for they have different dimensions|]
        `shouldBe` "Cannot convert formula 2.0 * exponent(unit=s, value=1.0) which has unit dimensionless to unit dimensionless for they have different dimensions"
  describe "raw" $ do
    it "does not escape anything" $
      [raw|hello
  - a \n {\
  - b }
  |]
        `shouldBe` "hello\n  - a \\n {\\\n  - b }\n  "
  describe "str" $ do
    it "basic escaping but no indentation neither formatting" $
      [str|hello
  - a \n {\
  - b {pi}
  |]
        `shouldBe` "hello\n  - a \n {  - b {pi}\n  "
  describe "strTrim" $ do
    it "basic escaping neither formatting" $
      [strTrim|
  - a \b {
  - b {pi}
  |]
        `shouldBe` "- a \b {\n- b {pi}\n"

  describe "handle ::" $ do
    it "works in simple context" $ do
      [fmt|{-10 :: Int:d}|] `shouldBe` "-10"
    it "works in a padding = context" $ do
      [fmt|{-10 :: Int::=10d}|] `shouldBe` "-:::::::10"
    it "works in a padding < context" $ do
      [fmt|{-10 :: Int::<10d}|] `shouldBe` "-10:::::::"
    it "works in a padding > context" $ do
      [fmt|{-10 :: Int::>10d}|] `shouldBe` ":::::::-10"
    it "works in a padding ^ context" $ do
      [fmt|{-10 :: Int::^10d}|] `shouldBe` ":::-10::::"

  describe "variables" $ do
    it "local" $ do
      let var = "Guillaume" :: String
      [fmt|{var}|] `shouldBe` "Guillaume"
    it "global" $ do
      [fmt|{globalName}|] `shouldBe` "Valérian"
    it "multiple expressions with variables" $ do
      let padding = 10
      let precision = 3
      [fmt|{globalName:{padding}} and pi = {pi:.{precision}}|] `shouldBe` "Valérian   and pi = 3.142"
    it "an expression with multiples variables" $ do
      let padding = 10
      let precision = 3
      [fmt|pi = {pi:{padding}.{precision}}|] `shouldBe` "pi =      3.142"
    it "an expression with module.variable" $ do
      [fmt|{Text.intercalate " " ["a" :: Text.Text, "b" :: Text.Text]}|] `shouldBe` "a b"
