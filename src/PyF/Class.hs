{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PyF.Class where

import Data.Int
import qualified Data.Text as SText
import qualified Data.Text.Lazy as LText
import Data.Word
import Numeric.Natural

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

type instance PyFClassify Float = 'PyFFractional

type instance PyFClassify Double = 'PyFFractional

type instance PyFClassify String = 'PyFString

type instance PyFClassify LText.Text = 'PyFString

type instance PyFClassify SText.Text = 'PyFString

-- | Convert a type to string
--   The default implementation uses `Show`
class PyFToString t where
  pyfToString :: t -> String
  default pyfToString :: Show t => t -> String
  pyfToString = show

instance PyFToString String where pyfToString = id

instance PyFToString LText.Text where pyfToString = LText.unpack

instance PyFToString SText.Text where pyfToString = SText.unpack

instance {-# OVERLAPPABLE #-} Show t => PyFToString t where pyfToString = show
