{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies #-}

import Test.Hspec

import PyF
import SpecUtils
import SpecCustomDelimiters

{-
   - Normal tests are done using the recommanded API: [f|.....|]
   - Test with $(checkExample formatString result) are checked against the python reference implementation. Result is provided as documentation.
   - Test with $(checkExampleDiff formatString result) are not checked against the python reference implementation. This is known (and documented) differences.
   - Test with $(check formatString) are only tested against the python reference implementation.
-}

main :: IO ()
main = hspec spec

newtype FooFloating = FooFloating Float
  deriving newtype (Show, RealFloat, RealFrac, Floating, Fractional, Real, Enum, Num, Ord, Eq)

newtype FooIntegral = FooIntegral Integer
  deriving newtype (Show, Integral, Real, Enum, Num, Ord, Eq)

data Foo = Foo

data FooDefault = FooDefault
  deriving (Show)

instance PyFToString FooDefault

instance PyFToString Foo where
  toString Foo = "I'm a Foo"

type instance PyFClassify Foo = 'PyFString
type instance PyFClassify FooFloating = 'PyFFractional
type instance PyFClassify FooIntegral = 'PyFIntegral
type instance PyFClassify FooDefault = 'PyFString

spec :: Spec
spec = do
  describe "simple with external variable" $ do
    let
      anInt = 123
      aFloat = 0.234
      aString = "hello"
    it "int" $ [f|{anInt}|] `shouldBe` "123"
    it "float" $ [f|{aFloat}|] `shouldBe` "0.234"
    it "string" $ [f|{aString}|] `shouldBe` "hello"
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
    describe "character" $ do
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

    describe "string truncating" $ do
      it "works" $ $(checkExample "{\"hello\":.3}" "hel")

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
        let nan = 0.0 / 0
        it "nan" $(checkExample "{nan}" "nan")
        it "nan f" $(checkExample "{nan:f}" "nan")
        it "nan e" $(checkExample "{nan:e}" "nan")
        it "nan g" $(checkExample "{nan:g}" "nan")
        it "nan F" $(checkExample "{nan:F}" "NAN")
        it "nan G" $(checkExample "{nan:G}" "NAN")
        it "nan E" $(checkExample "{nan:E}" "NAN")
    describe "Infinite" $ do
        let inf = 1.0 / 0
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

  describe "complex" $ do
    it "works with many things at once" $
      let
        name = "Guillaume"
        age = 31
        euroToFrancs = 6.55957
      in
        [f|hello {name} you are {age} years old and the conversion rate of euro is {euroToFrancs:.2}|] `shouldBe` ("hello Guillaume you are 31 years old and the conversion rate of euro is 6.56")


  describe "error reporting" $ do
    pure () -- TODO: find a way to test error reporting

  describe "sub expressions" $ do
    it "works" $ do
      [f|2pi = {2 * pi:.2}|] `shouldBe` "2pi = 6.28"

  describe "escape strings" $ do
    it "works" $ do
      [f|hello \n\b|] `shouldBe` "hello \n\b"

  it "escape chars" $ do
     [f|}}{{}}{{|] `shouldBe` "}{}{"

  describe "custom delimiters" $ do
    it "works" $ do
      [myCustomFormatter|2 * pi = @2*pi:.2f!|] `shouldBe` "2 * pi = 6.28"
    it "escape chars" $ do
       [myCustomFormatter|@@!!@@!!|] `shouldBe` "@!@!"

  describe "empty line" $ do
    it "works" $ do
      [f||] `shouldBe` ""

  describe "multi line escape" $ do
    it "works" $ do
      [f|\
- a
- b
\
|] `shouldBe` "- a\n- b\n"

    it "escapes in middle of line" $ do
      [f|Example goes \
here!|] `shouldBe` "Example goes here!"

    it "escapes a lot of things" $ do
      [f|\
I'm a line with \n and \\ and a correct line
ending, but that one is escaped\
And I'm escaping before and after: \\{pi:.3f}\\
yeah\
|] `shouldBe` "I'm a line with \n and \\ and a correct line\nending, but that one is escapedAnd I'm escaping before and after: \\3.142\\\nyeah"

    it "escapes" $ do
      [f|\\
- a
- b
\
|] `shouldBe` "\\\n- a\n- b\n"

  describe "language extensions" $ do
     it "parses @Int" $ do
       [f|hello {show @Int 10}|] `shouldBe` "hello 10"
     it "parses BinaryLiterals" $ do
       [f|hello {0b1111}|] `shouldBe` "hello 15"


  describe "custom types" $ do
      it "works with integral" $ do
        [f|{FooIntegral 10:d}|] `shouldBe` "10"
      it "works with floating" $ do
        [f|{FooFloating 25.123:f}|] `shouldBe` "25.123000"
      it "works with string" $ do
        [f|{Foo:s}|] `shouldBe` "I'm a Foo"
        [f|{FooDefault:s}|] `shouldBe` "FooDefault"
      it "works with classify" $ do
        [f|{Foo}|] `shouldBe` "I'm a Foo"
        [f|{FooIntegral 100}|] `shouldBe` "100"
        [f|{FooFloating 100.123}|] `shouldBe` "100.123"
        [f|{FooDefault}|] `shouldBe` "FooDefault"