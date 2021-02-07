{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}

-- |
--
-- Formatters for integral / fractional and strings.
--
-- Is support:
--
-- For all types:
--
--   * Grouping of the integral part (i.e: adding a custom char to separate groups of digits)
--   * Padding (left, right, around, and between the sign and the number)
--   * Sign handling (i.e: display the positive sign or not)
--
-- For floating:
--
--   * Precision
--   * Fixed / Exponential / Generic formatting
--
-- For integrals:
--
--    * Binary / Hexa / Octal / Character representation
module PyF.Formatters
  ( -- * Generic formating function
    formatString,
    formatIntegral,
    formatFractional,

    -- * Formatter details
    AltStatus (..),
    UpperStatus (..),
    FormatType (..),
    Format (..),
    SignMode (..),
    AnyAlign (..),

    -- * Internal usage only
    AlignMode (..),
    getAlignForString,
    AlignForString (..),
  )
where

import Data.Char (chr, toUpper)
import Data.List (intercalate)
import "template-haskell" Language.Haskell.TH.Syntax
import qualified Numeric

-- ADT for API

-- | Sign handling
data SignMode
  = -- | Display '-' sign and '+' sign
    Plus
  | -- | Only display '-' sign
    Minus
  | -- | Display '-' sign and a space for positive numbers
    Space
  deriving (Show)

data AlignForString = AlignAll | AlignNumber
  deriving (Show)

-- | Alignement
data AlignMode (k :: AlignForString) where
  -- | Left padding
  AlignLeft :: AlignMode 'AlignAll
  -- | Right padding
  AlignRight :: AlignMode 'AlignAll
  -- | Padding will be added between the sign and the number
  AlignInside :: AlignMode 'AlignNumber
  -- | Padding will be added around the valueber
  AlignCenter :: AlignMode 'AlignAll

deriving instance Show (AlignMode k)

-- The generic version

-- | Existential version of 'AlignMode'
data AnyAlign where
  AnyAlign :: AlignMode (k :: AlignForString) -> AnyAlign

deriving instance Show AnyAlign

deriving instance Lift AnyAlign

-- I hate how a must list all cases, any solution ?
-- o = Just o does not work
getAlignForString :: AlignMode k -> Maybe (AlignMode 'AlignAll)
getAlignForString AlignInside = Nothing
getAlignForString AlignRight = Just AlignRight
getAlignForString AlignCenter = Just AlignCenter
getAlignForString AlignLeft = Just AlignLeft

-- | This formatter support alternate version
data AltStatus = CanAlt | NoAlt

-- | This formatter support Upper case version
data UpperStatus = CanUpper | NoUpper

-- | This formatter formats an integral or a fractional
data FormatType = Fractional | Integral

-- | All the Formatters
data Format (k :: AltStatus) (k' :: UpperStatus) (k'' :: FormatType) where
  -- Integrals
  Decimal :: Format 'NoAlt 'NoUpper 'Integral
  Character :: Format 'NoAlt 'NoUpper 'Integral
  Binary :: Format 'CanAlt 'NoUpper 'Integral
  Hexa :: Format 'CanAlt 'CanUpper 'Integral
  Octal :: Format 'CanAlt 'NoUpper 'Integral
  -- Fractionals
  Fixed :: Format 'CanAlt 'CanUpper 'Fractional
  Exponent :: Format 'CanAlt 'CanUpper 'Fractional
  Generic :: Format 'CanAlt 'CanUpper 'Fractional
  Percent :: Format 'CanAlt 'NoUpper 'Fractional
  -- Meta formats
  Alternate :: Format 'CanAlt u f -> Format 'NoAlt u f
  -- Upper should come AFTER Alt, so this disallow any future alt
  Upper :: Format alt 'CanUpper f -> Format 'NoAlt 'NoUpper f

-- Internal Integral
-- Todo: remove the Show constraint ?
-- Needed for debug in Numeric function, this is painful
reprIntegral :: (Show i, Integral i) => Format t t' 'Integral -> i -> Repr
reprIntegral fmt i = IntegralRepr sign $ format fmt
  where
    format :: Format t t' 'Integral -> String
    format = \case
      Decimal -> Numeric.showInt iAbs ""
      Octal -> Numeric.showOct iAbs ""
      Binary -> Numeric.showIntAtBase 2 (\digit -> if digit == 0 then '0' else '1') iAbs ""
      Hexa -> Numeric.showHex iAbs ""
      Upper fmt' -> map toUpper $ format fmt'
      Character -> [chr (fromIntegral i)]
      Alternate fmt' -> format fmt'
    (sign, iAbs) = splitSign i

prefixIntegral :: Format t t' 'Integral -> String
prefixIntegral (Alternate Octal) = "0o"
prefixIntegral (Alternate Binary) = "0b"
prefixIntegral (Alternate Hexa) = "0x"
prefixIntegral (Upper f) = toUpper <$> prefixIntegral f
prefixIntegral _ = ""

splitSign :: (Num b, Ord b) => b -> (Sign, b)
splitSign v = (if v < 0 then Negative else Positive, abs v)

-- Internal Fractional
reprFractional :: (RealFloat f) => Format t t' 'Fractional -> Maybe Int -> f -> Repr
reprFractional fmt precision f
  | isInfinite f = Infinite sign (upperIt "inf")
  | isNaN f = NaN (upperIt "nan")
  | isNegativeZero f =
    let (FractionalRepr Positive aa bb cc) = reprFractional fmt precision (abs f)
     in FractionalRepr Negative aa bb cc
  | otherwise = FractionalRepr sign decimalPart fractionalPart suffixPart
  where
    upperIt s = case fmt of
      Upper _ -> toUpper <$> s
      _ -> s
    (sign, iAbs) = splitSign f
    (decimalPart, fractionalPart, suffixPart) = format fmt
    format :: Format t t' 'Fractional -> (String, String, String)
    format = \case
      Fixed -> splitFractional (Numeric.showFFloatAlt precision iAbs "")
      Exponent -> overrideExponent precision $ splitFractionalExp (Numeric.showEFloat precision iAbs "")
      Generic -> splitFractionalExp (Numeric.showGFloatAlt precision iAbs "")
      Percent -> let (a, b, "") = splitFractional (Numeric.showFFloatAlt precision (iAbs * 100) "") in (a, b, "%")
      Alternate fmt' -> format fmt'
      Upper fmt' ->
        let (a, b, c) = format fmt'
         in (a, b, map toUpper c)
    splitFractional :: String -> (String, String, String)
    splitFractional s =
      let (a, b) = break (== '.') s
       in (a, drop 1 b, "")

overrideExponent :: Maybe Int -> (String, String, String) -> (String, String, String)
overrideExponent (Just 0) (a, "0", c) = (a, "", c)
overrideExponent _ o = o

splitFractionalExp :: String -> (String, String, String)
splitFractionalExp s =
  let (a, b') = break (\c -> c == '.' || c == 'e') s
      b = drop 1 b'
      (fpart, e) = case b' of
        'e' : _ -> ("", b')
        _ -> break (== 'e') b
   in ( a,
        fpart,
        case e of
          'e' : '-' : n -> "e-" ++ pad n
          'e' : n -> "e+" ++ pad n
          leftover -> leftover
      )
  where
    pad n@[_] = '0' : n
    pad n = n

-- Cases Integral / Fractional

group :: Repr -> Maybe (Int, Char) -> Repr
group (IntegralRepr s str) (Just (size, c)) = IntegralRepr s (groupIntercalate c size str)
group (FractionalRepr s a b d) (Just (size, c)) = FractionalRepr s (groupIntercalate c size a) b d
group i _ = i

padAndSign :: Format t t' t'' -> String -> SignMode -> Maybe (Int, AlignMode k, Char) -> Repr -> String
padAndSign format prefix sign padding repr = leftAlignMode <> prefixStr <> middleAlignMode <> content <> rightAlignMode
  where
    (signStr, content) = case repr of
      IntegralRepr s str -> (formatSign s sign, str)
      FractionalRepr s a b c -> (formatSign s sign, joinPoint format a b <> c)
      Infinite s str -> (formatSign s sign, str)
      NaN str -> ("", str)
    prefixStr = signStr <> prefix
    len = length prefixStr + length content
    (leftAlignMode, rightAlignMode, middleAlignMode) = case padding of
      Nothing -> ("", "", "")
      Just (pad, padMode, padC) ->
        let padNeeded = max 0 (pad - len)
         in case padMode of
              AlignLeft -> ("", replicate padNeeded padC, "")
              AlignRight -> (replicate padNeeded padC, "", "")
              AlignCenter -> (replicate (padNeeded `div` 2) padC, replicate (padNeeded - padNeeded `div` 2) padC, "")
              AlignInside -> ("", "", replicate padNeeded padC)

joinPoint :: Format t t' t'' -> String -> String -> String
joinPoint (Upper f) a b = joinPoint f a b
joinPoint (Alternate _) a b = a <> "." <> b
joinPoint _ a "" = a
joinPoint _ a b = a <> "." <> b

-- Generic
data Repr
  = IntegralRepr Sign String
  | FractionalRepr Sign String String String
  | Infinite Sign String
  | NaN String
  deriving (Show)

data Sign = Negative | Positive
  deriving (Show)

formatSign :: Sign -> SignMode -> String
formatSign Positive Plus = "+"
formatSign Positive Minus = ""
formatSign Positive Space = " "
formatSign Negative _ = "-"

groupIntercalate :: Char -> Int -> String -> String
groupIntercalate c i s = intercalate [c] (reverse (pack (reverse s)))
  where
    pack "" = []
    pack l = reverse (take i l) : pack (drop i l)

-- Final formatters

-- | Format an integral number
formatIntegral ::
  (Show i, Integral i) =>
  Format t t' 'Integral ->
  SignMode ->
  -- | Padding
  Maybe (Int, AlignMode k, Char) ->
  -- | Grouping
  Maybe (Int, Char) ->
  i ->
  String
formatIntegral f sign padding grouping i = padAndSign f (prefixIntegral f) sign padding (group (reprIntegral f i) grouping)

-- | Format a fractional number
formatFractional ::
  (RealFloat f) =>
  Format t t' 'Fractional ->
  SignMode ->
  -- | Padding
  Maybe (Int, AlignMode k, Char) ->
  -- | Grouping
  Maybe (Int, Char) ->
  -- | Precision
  Maybe Int ->
  f ->
  String
formatFractional f sign padding grouping precision i = padAndSign f "" sign padding (group (reprFractional f precision i) grouping)

-- | Format a string
formatString ::
  -- | Padding
  Maybe (Int, AlignMode 'AlignAll, Char) ->
  -- | Precision (will truncate before padding)
  Maybe Int ->
  String ->
  String
formatString Nothing Nothing s = s
formatString Nothing (Just i) s = take i s
formatString (Just (padSize, padMode, padC)) size s = padLeft <> str <> padRight
  where
    str = formatString Nothing size s
    paddingLength = max 0 (padSize - length str)
    (padLeft, padRight) = case padMode of
      AlignLeft -> ("", replicate paddingLength padC)
      AlignRight -> (replicate paddingLength padC, "")
      AlignCenter -> (replicate (paddingLength `div` 2) padC, replicate (paddingLength - paddingLength `div` 2) padC)

-- TODO
{-
the .
-}

deriving instance Lift (AlignMode k)

deriving instance Lift SignMode

deriving instance Lift (Format k k' k'')
