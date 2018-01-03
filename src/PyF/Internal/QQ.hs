{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PyF.Internal.QQ where

import Text.Megaparsec

import qualified Formatting as F
import qualified Data.Scientific as Scientific

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.TH

import Data.Maybe (fromMaybe)
import Control.Monad

import qualified Data.Text.Lazy.Builder as Builder

import qualified Data.Text.Lazy as LText
import qualified Data.Text as SText
import qualified Data.Char
import qualified Data.List.NonEmpty as NonEmpty

import Data.Monoid ((<>))

import PyF.Internal.PythonSyntax

f :: QuasiQuoter
f = QuasiQuoter {
    quoteExp = toExp
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("Data.String.Interpolate.i: This QuasiQuoter can not be used as a " ++ name ++ "!")

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
    Right items -> do
      fmtItems <- goFormat items
      foldM goArgs (AppE (VarE 'F.format) fmtItems) items

goArgs :: Exp -> Item -> Q Exp
goArgs currentApp (Raw _) = pure currentApp
goArgs currentApp (Replacement x _) = pure $ AppE currentApp (VarE (mkName x))

goFormat :: [Item] -> Q Exp
goFormat items = foldl1 fofo <$> (mapM toFormat items)

fofo :: Exp -> Exp -> Exp
fofo s0 s1 = InfixE (Just s0) (VarE '(F.%)) (Just s1)

-- Real formatting is here

toFormat :: Item -> Q Exp
toFormat (Raw x) = [| F.now (Builder.fromString x) |]
toFormat (Replacement _ y) = padAndFormat (fromMaybe DefaultFormatMode y)

padAndFormat :: FormatMode -> Q Exp
padAndFormat DefaultFormatMode = [| F.build |]
padAndFormat (FormatMode pad t) = applyPadding pad =<< format t

format :: TypeFormat -> Q Exp

-- Default cases
format (DefaultF prec) = case prec of
  PrecisionDefault -> [| F.build |]
  Precision i -> [| genericPrecision i |] -- Precision is clipping
-- String types
format (StringF prec) = case prec of
  PrecisionDefault -> [| F.later defaultString |]
  Precision i -> [| laterTake i |] -- Precision is clipping
-- Integer types
format (BinaryF alt) = [| $(ifAlternate alt "0b") F.% F.bin |]
format (DecimalF) = [| F.build |] -- TODO: check that it is a number
format (OctalF alt) = [| $(ifAlternate alt "0o") F.% F.oct |]
format (HexF alt) = [| $(ifAlternate alt "0x") F.% F.hex |]
format (HexCapsF alt) = [| $(ifAlternate alt "0x") F.% toUpper F.%. F.hex |]
format (CharacterF) = [| laterChar |]

-- Floating point types
-- TODO: NaN and Inf are bugged
format (ExponentialF prec alt) = [| F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format (ExponentialCapsF prec alt) = [| toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format (FixedF prec alt) = [| F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format (FixedCapsF prec alt) = [| toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format (PercentF prec alt) = [| F.mapf (*100) (F.scifmt Scientific.Fixed $(precToMaybe prec)) F.% "%" |]

ifAlternate :: AlternateForm -> String -> Q Exp
ifAlternate NormalForm _ = [| F.now (Builder.fromString "") |]
ifAlternate AlternateForm s = [| F.now (Builder.fromString s) |]

opLater :: (LText.Text -> LText.Text) -> F.Format r (Builder.Builder -> r)
opLater op = F.later (Builder.fromLazyText . op . Builder.toLazyText)

laterChar :: F.Format r (Int -> r)
laterChar = F.later (Builder.fromLazyText . LText.singleton . Data.Char.chr)

laterTake :: Integral i => i -> F.Format r (Builder.Builder -> r)
laterTake i = opLater (LText.take (fromIntegral i))

toUpper :: F.Format r (Builder.Builder -> r)
toUpper = opLater LText.toUpper

alternateFloat :: AlternateForm -> F.Format r (Builder.Builder -> r)
alternateFloat NormalForm = opLater id
alternateFloat AlternateForm = opLater f
  where f t = case LText.find (=='.') t of
          Nothing -> t <> "."
          Just _ -> t

precToMaybe :: Precision -> Q Exp
precToMaybe p = [| Just $(precToInt p) |] -- Default precision from python

precToInt :: Precision -> Q Exp
precToInt PrecisionDefault = [| 6 |] -- Default precision from python
precToInt (Precision i) = [| i |]

applyPadding :: Padding -> Exp -> Q Exp
applyPadding PaddingDefault e = pure e
applyPadding (Padding i am ac) e = [| $(padFunc) i padChar F.%. $(pure e) |]
  where
    padFunc = pure $ VarE $ case am of
          AlignDefault -> 'F.left
          AlignLeft -> 'F.left
          AlignRight -> 'F.right
          AlignCenter -> 'F.center

    padChar = case ac of
          AlignCharDefault -> ' '
          AlignChar c -> c

-- To Scientific
class ToScientific t where
  toScientific :: t -> Scientific.Scientific

instance ToScientific Float where toScientific = Scientific.fromFloatDigits
instance ToScientific Double where toScientific = Scientific.fromFloatDigits
instance ToScientific Scientific.Scientific where toScientific = id

-- Generic Precision
class GenericPrecision t where
  genericPrecision :: Int -> F.Format r (t -> r)

instance GenericPrecision Float where
  genericPrecision i = F.fixed i

instance GenericPrecision Double where
  genericPrecision i = F.fixed i

instance GenericPrecision [Char] where
  genericPrecision i = laterTake i F.%. F.build

instance GenericPrecision LText.Text where
  genericPrecision i = laterTake i F.%. F.build

instance GenericPrecision SText.Text where
  genericPrecision i = laterTake i F.%. F.build

class DefaultString t where
  defaultString :: t -> Builder.Builder

instance DefaultString [Char] where
  defaultString = Builder.fromString

instance DefaultString SText.Text where
  defaultString = Builder.fromText

instance DefaultString LText.Text where
  defaultString = Builder.fromLazyText
