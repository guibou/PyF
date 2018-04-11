{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module PyF.Internal.QQ (
  toExp)
where

import Text.Megaparsec

import qualified Formatting as F

import           Language.Haskell.TH

import Data.Maybe (fromMaybe)

import qualified Data.Text.Lazy.Builder as Builder

import qualified Data.Text.Lazy as LText
import qualified Data.Text as SText
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Word as Word
import qualified Data.Int as Int
import Numeric.Natural

import Language.Haskell.Meta.Parse (parseExp)

import PyF.Internal.PythonSyntax
import qualified PyF.Formatters as Formatters

-- Be Careful: empty format string
toExp:: String -> Q Exp
toExp s = do
  filename <- loc_filename <$> location
  (line, col) <- loc_start <$> location

  let change_log "<interactive>" currentState = currentState
      change_log _ currentState = let
        (SourcePos sName _ _) NonEmpty.:| xs = statePos currentState
        in currentState {statePos = (SourcePos sName (mkPos line) (mkPos col)) NonEmpty.:| xs}

  case parse (updateParserState (change_log filename) >> result) filename s of
    Left err -> do

      if filename == "<interactive>"
        then do
          fail (parseErrorPretty' s err)
        else do
          fileContent <- runIO (readFile filename)
          fail (parseErrorPretty' fileContent err)
    Right items -> goFormat items

goFormat :: [Item] -> Q Exp
goFormat items = foldl1 fofo <$> (mapM toFormat items)

fofo :: Exp -> Exp -> Exp
fofo s0 s1 = InfixE (Just s0) (VarE '(F.%)) (Just s1)

-- Real formatting is here

toFormat :: Item -> Q Exp
toFormat (Raw x) = [| F.now (Builder.fromString x) |]
toFormat (Replacement x y) = do
  formatExpr <- padAndFormat (fromMaybe DefaultFormatMode y)

  case parseExp x of
    Right expr -> pure (AppE (VarE 'F.now) (VarE 'Builder.fromString `AppE` (formatExpr `AppE` expr)))
    Left err -> fail err

changePrec :: Precision -> Maybe Int
changePrec PrecisionDefault = Just 6
changePrec (Precision n) = Just (fromIntegral n)

changePrec' :: Precision -> Maybe Int
changePrec' PrecisionDefault = Nothing
changePrec' (Precision n) = Just (fromIntegral n)

toGrp Nothing _ = Nothing
toGrp (Just c) i = Just (i, c)

-- Todo: Alternates for floating
padAndFormat :: FormatMode -> Q Exp
padAndFormat (FormatMode padding tf grouping) = case tf of
  -- Integrals
  BinaryF AlternateForm s -> [| Formatters.formatIntegral (Formatters.Alternate Formatters.Binary) s newPadding (toGrp grouping 4) |]
  BinaryF NormalForm s -> [| Formatters.formatIntegral Formatters.Binary s newPadding (toGrp grouping 4) |]
  CharacterF -> [| Formatters.formatIntegral Formatters.Character Formatters.Minus newPadding Nothing |]
  DecimalF s -> [| Formatters.formatIntegral Formatters.Decimal s newPadding (toGrp grouping 3) |]
  HexF AlternateForm s -> [| Formatters.formatIntegral (Formatters.Alternate Formatters.Hexa) s newPadding (toGrp grouping 4) |]
  HexF NormalForm s -> [| Formatters.formatIntegral Formatters.Hexa s newPadding (toGrp grouping 4) |]
  OctalF AlternateForm s -> [| Formatters.formatIntegral (Formatters.Alternate Formatters.Octal) s newPadding (toGrp grouping 4) |]
  OctalF NormalForm s -> [| Formatters.formatIntegral Formatters.Octal s newPadding (toGrp grouping 4) |]
  HexCapsF AlternateForm s -> [| Formatters.formatIntegral (Formatters.Upper (Formatters.Alternate Formatters.Hexa)) s newPadding (toGrp grouping 4) |]
  HexCapsF NormalForm s -> [| Formatters.formatIntegral (Formatters.Upper (Formatters.Hexa)) s newPadding (toGrp grouping 4) |]

  -- Floating
  ExponentialF prec s -> [| Formatters.formatFractional (Formatters.Exponent) s newPadding (toGrp grouping 3) (changePrec prec) |]
  ExponentialCapsF prec s -> [| Formatters.formatFractional (Formatters.Upper (Formatters.Exponent)) s newPadding (toGrp grouping 3) (changePrec prec) |]
  GeneralF prec s -> [| Formatters.formatFractional (Formatters.Generic) s newPadding (toGrp grouping 3) (changePrec prec) |]
  GeneralCapsF prec s -> [| Formatters.formatFractional (Formatters.Upper (Formatters.Generic)) s newPadding (toGrp grouping 3) (changePrec prec) |]
  FixedF prec s -> [| Formatters.formatFractional (Formatters.Fixed) s newPadding (toGrp grouping 3) (changePrec prec) |]
  FixedCapsF prec s -> [| Formatters.formatFractional (Formatters.Upper (Formatters.Fixed)) s newPadding (toGrp grouping 3) (changePrec prec) |]
  PercentF prec s -> [| Formatters.formatFractional (Formatters.Percent) s newPadding (toGrp grouping 3) (changePrec prec) |]

  -- Default / String
  DefaultF prec s -> [| \v ->
      case categorise v of
        Integral i -> Formatters.formatIntegral Formatters.Decimal s newPadding (toGrp grouping 3) i
        Fractional f -> Formatters.formatFractional Formatters.Generic s newPadding (toGrp grouping 3) (changePrec' prec) f
        StringType f -> Formatters.formatString newPadding (changePrec' prec) f
                         |]
  StringF prec -> [| Formatters.formatString newPadding (changePrec' prec) |]
  where
    newPadding = case padding of
      PaddingDefault -> Nothing
      (Padding i alignMode alignChar) -> Just (i, alignMode, alignChar)

data FormattingType where
  StringType :: String -> FormattingType
  Fractional :: RealFloat t => t -> FormattingType
  Integral :: (Show t, Integral t) => t -> FormattingType

class Categorise t where
  categorise :: t -> FormattingType

instance Categorise Integer where categorise i = Integral i
instance Categorise Int where categorise i = Integral i
instance Categorise Int.Int8 where categorise i = Integral i
instance Categorise Int.Int16 where categorise i = Integral i
instance Categorise Int.Int32 where categorise i = Integral i
instance Categorise Int.Int64 where categorise i = Integral i

instance Categorise Natural where categorise i = Integral i
instance Categorise Word where categorise i = Integral i
instance Categorise Word.Word8 where categorise i = Integral i
instance Categorise Word.Word16 where categorise i = Integral i
instance Categorise Word.Word32 where categorise i = Integral i
instance Categorise Word.Word64 where categorise i = Integral i

instance Categorise Float where categorise f = Fractional f
instance Categorise Double where categorise f = Fractional f

instance Categorise LText.Text where categorise t = StringType (LText.unpack t)
instance Categorise SText.Text where categorise t = StringType (SText.unpack t)
instance Categorise String where categorise t = StringType t
