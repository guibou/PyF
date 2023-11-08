{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | You want to add formatting support for your custom type. This is the right module.
--
-- In PyF, formatters are in three categories:
--
-- - Integral numbers, which are numbers without fractional part
-- - Fractional numbers, which are numbers with a fractional part
-- - String, which represents text.
--
-- The formatting can be either explicit or implicit. For example:
--
-- >>> let x = 10 in [fmt|{x}|]
-- 10
--
-- Is an implicit formatting to number, but:
--
-- >>> let x = 10 in [fmt|{x:d}|]
--
-- Is an explicit formatting to Integral numbers, using @d@.
--
-- Implicit formatting will only format to either Integral, Fractional or text,
-- and this choice is done by the (open) type family `PyFCategory'.
--
-- This modules also provides 3 type class for formatting.
--
-- - 'PyfFormatFractional' and 'PyfFormatIntegral' are responsible for
-- formatting integral and fractional numbers. Default instances are provided
-- respectively for 'Real' and 'Integral'. 'PyFToString' is in charge of
-- formatting text.
module PyF.Class where

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.Char (ord)
import Data.Int
import qualified Data.Ratio
import qualified Data.Text as SText
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as LText
import qualified Data.Time
import Data.Word
import Numeric.Natural
import PyF.Formatters

-- * Default formatting classification

-- | The three categories of formatting in 'PyF'
data PyFCategory
  = -- | Format as an integral, no fractional part, precise value
    PyFIntegral
  | -- | Format as a fractional, approximate value with a fractional part
    PyFFractional
  | -- | Format as a string
    PyFString

-- | Classify a type to a 'PyFCategory'
--   This classification will be used to decide which formatting to
--   use when no type specifier in provided.
type family PyFClassify t :: PyFCategory

type instance PyFClassify Integer = 'PyFIntegral

type instance PyFClassify Int = 'PyFIntegral

type instance PyFClassify Int8 = 'PyFIntegral

type instance PyFClassify Int16 = 'PyFIntegral

type instance PyFClassify Int32 = 'PyFIntegral

type instance PyFClassify Int64 = 'PyFIntegral

type instance PyFClassify Natural = 'PyFIntegral

type instance PyFClassify Word = 'PyFIntegral

type instance PyFClassify Word8 = 'PyFIntegral

type instance PyFClassify Word16 = 'PyFIntegral

type instance PyFClassify Word32 = 'PyFIntegral

type instance PyFClassify Word64 = 'PyFIntegral

-- Float numbers

type instance PyFClassify Float = 'PyFFractional

type instance PyFClassify Double = 'PyFFractional

type instance PyFClassify Data.Time.DiffTime = 'PyFFractional

type instance PyFClassify Data.Time.NominalDiffTime = 'PyFFractional

type instance PyFClassify (Data.Ratio.Ratio i) = 'PyFFractional

-- String

type instance PyFClassify String = 'PyFString

type instance PyFClassify LText.Text = 'PyFString

type instance PyFClassify SText.Text = 'PyFString

type instance PyFClassify Data.ByteString.ByteString = 'PyFString

type instance PyFClassify Data.ByteString.Lazy.ByteString = 'PyFString

type instance PyFClassify Char = 'PyFString

-- * String formatting

-- | Convert a type to string
-- This is used for the string formatting.
class PyFToString t where
  pyfToString :: t -> String

instance PyFToString String where pyfToString = id

instance PyFToString LText.Text where pyfToString = LText.unpack

instance PyFToString SText.Text where pyfToString = SText.unpack

instance PyFToString Data.ByteString.ByteString where pyfToString = SText.unpack . E.decodeUtf8

instance PyFToString Data.ByteString.Lazy.ByteString where pyfToString = pyfToString . Data.ByteString.Lazy.toStrict

instance PyFToString Char where pyfToString c = [c]

-- | Default instance. Convert any type with a 'Show instance.
instance {-# OVERLAPPABLE #-} Show t => PyFToString t where pyfToString = show

-- * Real formatting (with optional fractional part)

-- | Apply a fractional formatting to any type.
--
-- A default instance for any 'Real' is provided which internally converts to
-- 'Double', which may not be efficient or results in rounding errors.
--
-- You can provide your own instance and internally use 'formatFractional'
-- which does have the same signatures as 'pyfFormatFractional' but with a
-- 'RealFrac' constraint.
class PyfFormatFractional a where
  pyfFormatFractional ::
    (Integral paddingWidth, Integral precision) =>
    Format t t' 'Fractional ->
    -- | Sign formatting
    SignMode ->
    -- | Padding
    Maybe (paddingWidth, AlignMode k, Char) ->
    -- | Grouping
    Maybe (Int, Char) ->
    -- | Precision
    Maybe precision ->
    a ->
    String

-- | Default instance working for any 'Real'. Internally it converts the type to 'Double'.
instance {-# OVERLAPPABLE #-} Real t => PyfFormatFractional t where
  pyfFormatFractional f s p g prec v = formatFractional f s p g prec (realToFrac @t @Double v)

-- | This instance does not do any conversion.
instance PyfFormatFractional Double where pyfFormatFractional = formatFractional

-- | This instance does not do any conversion.
instance PyfFormatFractional Float where pyfFormatFractional = formatFractional

-- * Integral formatting

-- | Apply an integral formatting to any type.
--
-- A default instance for any 'Integral' is provided.
--
-- You can provide your own instance and internally use 'formatIntegral'
-- which does have the same signatures as 'pyfFormatIntegral' but with an
-- 'Integral' constraint.
class PyfFormatIntegral i where
  pyfFormatIntegral ::
    Integral paddingWidth =>
    Format t t' 'Integral ->
    -- | Sign formatting
    SignMode ->
    -- | Padding
    Maybe (paddingWidth, AlignMode k, Char) ->
    -- | Grouping
    Maybe (Int, Char) ->
    i ->
    String

-- | Default instance for any 'Integral'.
instance {-# OVERLAPPABLE #-} Integral t => PyfFormatIntegral t where
  pyfFormatIntegral f s p g v = formatIntegral f s p g v

-- | Returns the numerical value of a 'Char'
-- >>> [fmt|{'a':d}|]
-- 97
instance PyfFormatIntegral Char where
  pyfFormatIntegral f s p g v = formatIntegral f s p g (ord v)
