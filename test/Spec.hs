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
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Time
import GHC.OverloadedLabels
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import PyF
import SpecCustomDelimiters
import SpecUtils
import Test.Hspec
import qualified Data.Text as Text

{-
   - Normal tests are done using the recommanded API: [fmt|.....|]
   - Test with $(checkExample formatString result) are checked against the python reference implementation. Result is provided as documentation.
   - Test with $(checkExampleDiff formatString result) are not checked against the python reference implementation. This is known (and documented) differences.
   - Test with $(check formatString) are only tested against the python reference implementation.
-}

main :: IO ()
main = hspec spec

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

instance KnownSymbol a => Show (V a) where
  show V = "V=" ++ symbolVal (Proxy @a)

instance (a ~ a') => IsLabel a (V a') where
  fromLabel = V

showV :: KnownSymbol a => V a -> String
showV = show

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
      it "int" $(checkExample "{123}" "123")
      it "float" $(checkExample "{0.234}" "0.234")
      it "string" $(checkExample "{\"hello\"}" "hello")
      it "float precision" $(checkExample "{0.234:.1}" "0.2")
      it "string precision" $(checkExample "{\"hello\":.1}" "h")
      it "sign +" $(checkExample "{0.234:+}" "+0.234")
      it "sign space" $(checkExample "{0.234: }" " 0.234")
      it "sign neg" $(checkExample "{-123:+}" "-123")
    describe "binary" $ do
      it "simple" $(checkExample "{123:b}" "1111011")
      it "alt" $(checkExample "{123:#b}" "0b1111011")
      it "sign" $(checkExample "{123:+#b}" "+0b1111011")
    describe "character" $
      it "simple" $(checkExample "{123:c}" "{")
    describe "decimal" $ do
      it "simple" $(checkExample "{123:d}" "123")
      it "sign" $(checkExample "{123:+d}" "+123")
    describe "exponentiel" $ do
      it "simple > 1" $(checkExample "{234.0:e}" "2.340000e+02")
      it "precision > 1" $(checkExample "{234.0:.1e}" "2.3e+02")
      it "simple < 1" $(checkExample "{0.234:e}" "2.340000e-01")
      it "precision < 1 " $(checkExample "{0.234:.1e}" "2.3e-01")
    describe "exponentiel caps" $ do
      it "simple > 1" $(checkExample "{234.0:E}" "2.340000E+02")
      it "precision > 1" $(checkExample "{234.0:.1E}" "2.3E+02")
      it "simple < 1" $(checkExample "{0.234:E}" "2.340000E-01")
      it "precision < 1 " $(checkExample "{0.234:.1E}" "2.3E-01")
    describe "general" $ do
      it "simple small" $(checkExampleDiff "{123.02:g}" "123.020000")
      it "precision small" $(checkExampleDiff "{123.02:.1g}" "123.0")
      it "simple big" $(checkExampleDiff "{1234567890.23:g}" "1.234568e+09")
      it "precision big" $(checkExampleDiff "{1234567890.23:.1g}" "1.2e+09")
    describe "general caps" $ do
      it "simple small" $(checkExampleDiff "{123.02:G}" "123.020000")
      it "precision small" $(checkExampleDiff "{123.02:.1G}" "123.0")
      it "simple big" $(checkExampleDiff "{1234567890.23:G}" "1.234568E+09")
      it "precision big" $(checkExampleDiff "{1234567890.23:.1G}" "1.2E+09")
    describe "fixed" $ do
      it "simple" $(checkExample "{0.234:f}" "0.234000")
      it "precision" $(checkExample "{0.234:.1f}" "0.2")
    describe "fixed caps" $ do
      it "simple" $(checkExample "{0.234:F}" "0.234000")
      it "precision" $(checkExample "{0.234:.1F}" "0.2")
    describe "octal" $ do
      it "simple" $(checkExample "{123:o}" "173")
      it "alt" $(checkExample "{123:#o}" "0o173")
    describe "string" $ do
      it "string" $(checkExample "{\"hello\":s}" "hello")
      it "precision" $(checkExample "{\"hello\":.2s}" "he")
    describe "hex" $ do
      it "simple" $(checkExample "{123:x}" "7b")
      it "alt" $(checkExample "{123:#x}" "0x7b")
    describe "hex caps" $ do
      it "simple" $(checkExample "{123:X}" "7B")
      it "alt" $(checkExample "{123:#X}" "0X7B")
    describe "percent" $ do
      it "simple" $(checkExample "{0.234:%}" "23.400000%")
      it "precision" $(checkExample "{0.234:.2%}" "23.40%")
    describe "string truncating" $
      it "works" $(checkExample "{\"hello\":.3}" "hel")
    describe "padding" $ do
      describe "default char" $ do
        it "left" $(checkExample "{\"hello\":<10}" "hello     ")
        it "right" $(checkExample "{\"hello\":>10}" "     hello")
        it "center" $(checkExample "{\"hello\":^10}" "  hello   ")
      describe "a char" $ do
        it "left" $(checkExample "{\"hello\":-<10}" "hello-----")
        it "right" $(checkExample "{\"hello\":->10}" "-----hello")
        it "center" $(checkExample "{\"hello\":-^10}" "--hello---")
      describe "inside" $ do
        it "inside" $(checkExample "{123:=+10}" "+      123")
        it "inside" $(checkExample "{123:=10}" "       123")
        it "inside" $(checkExample "{- 123:=10}" "-      123")
        it "inside" $(checkExample "{- 123:|= 10}" "-||||||123")
        it "inside" $(checkExample "{123:|= 10}" " ||||||123")
      describe "default padding" $ do
        it "floating" $(checkExample "{1:10f}" "  1.000000")
        it "integral" $(checkExample "{1:10d}" "         1")
        it "string" $(checkExample "{\"h\":10s}" "h         ")
        it "default" $(checkExample "{1:10}" "         1")
        it "default" $(checkExample "{1.0:10}" "       1.0")
        it "default" $(checkExample "{\"h\":10}" "h         ")
    describe "NaN" $ do
      describe "float" $ do
        let nan = 0.0 / 0 :: Float
        it "nan" $(checkExample "{nan}" "nan")
        it "nan f" $(checkExample "{nan:f}" "nan")
        it "nan e" $(checkExample "{nan:e}" "nan")
        it "nan g" $(checkExample "{nan:g}" "nan")
        it "nan F" $(checkExample "{nan:F}" "NAN")
        it "nan G" $(checkExample "{nan:G}" "NAN")
        it "nan E" $(checkExample "{nan:E}" "NAN")
      describe "double" $ do
        let nan = 0.0 / 0 :: Double
        it "nan" $(checkExample "{nan}" "nan")
        it "nan f" $(checkExample "{nan:f}" "nan")
        it "nan e" $(checkExample "{nan:e}" "nan")
        it "nan g" $(checkExample "{nan:g}" "nan")
        it "nan F" $(checkExample "{nan:F}" "NAN")
        it "nan G" $(checkExample "{nan:G}" "NAN")
        it "nan E" $(checkExample "{nan:E}" "NAN")
    describe "Infinite" $ do
      describe "float" $ do
        let inf = 1.0 / 0 :: Float
        it "infinite" $(checkExample "{inf}" "inf")
        it "infinite f" $(checkExample "{inf:f}" "inf")
        it "infinite e" $(checkExample "{inf:e}" "inf")
        it "infinite g" $(checkExample "{inf:g}" "inf")
        it "infinite F" $(checkExample "{inf:F}" "INF")
        it "infinite G" $(checkExample "{inf:G}" "INF")
        it "infinite E" $(checkExample "{inf:E}" "INF")
      describe "double" $ do
        let inf = 1.0 / 0 :: Double
        it "infinite" $(checkExample "{inf}" "inf")
        it "infinite f" $(checkExample "{inf:f}" "inf")
        it "infinite e" $(checkExample "{inf:e}" "inf")
        it "infinite g" $(checkExample "{inf:g}" "inf")
        it "infinite F" $(checkExample "{inf:F}" "INF")
        it "infinite G" $(checkExample "{inf:G}" "INF")
        it "infinite E" $(checkExample "{inf:E}" "INF")
    describe "Grouping" $ do
      it "groups int" $(checkExample "{123456789:,d}" "123,456,789")
      it "groups int with _" $(checkExample "{123456789:_d}" "123_456_789")
      it "groups float" $(checkExample "{123456789.234:,f}" "123,456,789.234000")
      it "groups bin" $(checkExample "{123456789:_b}" "111_0101_1011_1100_1101_0001_0101")
      it "groups hex" $(checkExample "{123456789:_x}" "75b_cd15")
      it "groups oct" $(checkExample "{123456789:_o}" "7_2674_6425")
    describe "negative zero" $ do
      it "f" $(checkExample "{-0.0:f}" "-0.000000")
      it "e" $(checkExample "{-0.0:e}" "-0.000000e+00")
      it "g" $(checkExampleDiff "{-0.0:g}" "-0.000000")
      it "F" $(checkExample "{-0.0:F}" "-0.000000")
      it "G" $(checkExampleDiff "{-0.0:G}" "-0.000000")
      it "E" $(checkExample "{-0.0:E}" "-0.000000E+00")
    describe "0" $ do
      it "works" $(checkExample "{123:010}" "0000000123")
      it "works with sign" $(checkExample "{-123:010}" "-000000123")
      it "accept mode override" $(checkExample "{-123:<010}" "-123000000")
      it "accept mode and char override" $(checkExample "{-123:.<010}" "-123......")
    describe "no digit no dot" $ do
      it "f" $(checkExample "{1.0:.0f}" "1")
      it "e" $(checkExample "{1.0:.0e}" "1e+00")
      it "g" $(checkExample "{1.0:.0g}" "1")
      it "E" $(checkExample "{1.0:.0E}" "1E+00")
      it "G" $(checkExample "{1.0:.0G}" "1")
      it "percent" $(checkExample "{1.0:.0%}" "100%")
    describe "no digit alt -> dot" $ do
      it "f" $(checkExample "{1.0:#.0f}" "1.")
      it "e" $(checkExample "{1.0:#.0e}" "1.e+00")
      it "g" $(checkExample "{1.0:#.0g}" "1.")
      it "E" $(checkExample "{1.0:#.0E}" "1.E+00")
      it "G" $(checkExample "{1.0:#.0G}" "1.")
      it "percent" $(checkExample "{1.0:#.0%}" "100.%")
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

    it "behaves well with escaped first line" $ do
      [fmtTrim|\
                  - a
                  - b
                  |]
        `shouldBe` "- a\n- b\n"
    it "Do not touch single lines" $ do
      [fmtTrim|  hello|] `shouldBe` "  hello"
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
