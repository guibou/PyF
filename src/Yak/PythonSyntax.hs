{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeInType, StandaloneDeriving #-}

module Yak.PythonSyntax where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

import Data.Void (Void)

import qualified Formatting as F
import qualified Data.Scientific as Scientific

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.TH

import Data.Maybe (fromMaybe)
import Control.Monad

import qualified Data.Text.Lazy.Builder as Builder

import qualified Data.Text.Lazy as LText
import qualified Data.Text as SText

import Data.Monoid ((<>))
import qualified Data.Char
import qualified Data.Set as Set -- For fancyFailure
import qualified Data.List.NonEmpty as NonEmpty

type Parser t = Parsec Void String t

{-
-- TODO:
- Better parsing of integer
- Recursive replacement field, so "{string:.{precision}} can be parsed
- f_expression / conversion
- Not (Yet) implemented:
     - fields: sign / grouping_option / 0
     - types: n / g / G
     - alignement: =
     - #: for floating points
     - floating point rendering of NaN and Inf are bugged
-}


{-
f_string          ::=  (literal_char | "{{" | "}}" | replacement_field)*
replacement_field ::=  "{" f_expression ["!" conversion] [":" format_spec] "}"
f_expression      ::=  (conditional_expression | "*" or_expr)
                         ("," conditional_expression | "," "*" or_expr)* [","]
                       | yield_expression
conversion        ::=  "s" | "r" | "a"
format_spec       ::=  (literal_char | NULL | replacement_field)*
literal_char      ::=  <any code point except "{", "}" or NULL>
-}

data Item = Raw String
           | Replacement String (Maybe FormatMode)
           deriving (Show)

result :: Parser [Item]
result = many (rawString <|> escapedParenthesis <|> replacementField)

rawString :: Parser Item
rawString = Raw <$> some (noneOf ("{}" :: [Char]))

escapedParenthesis :: Parser Item
escapedParenthesis = Raw <$> (string "{{" <|> string "}}")

replacementField :: Parser Item
replacementField = do
  _ <- char '{'
  expr <- many (noneOf ("}:" :: [Char]))
  fmt <- optional $ do
    _ <- char ':'
    format_spec
  _ <- char '}'

  pure (Replacement expr fmt)

pattern DefaultFormatMode :: FormatMode
pattern DefaultFormatMode = FormatMode PaddingDefault (DefaultF PrecisionDefault)

data FormatMode = FormatMode Padding TypeFormat
                deriving (Show)

data AlignMode = AlignLeft
               | AlignRight
              -- | AlignInside -- Not handled (Yet ?)
               | AlignCenter
               | AlignDefault
               deriving (Show)

data Padding = PaddingDefault
             | Padding Integer AlignMode AlignChar
             deriving (Show)

data AlignChar = AlignCharDefault
               | AlignChar Char
               deriving (Show)

data Precision = PrecisionDefault
               | Precision Integer
               deriving (Show)
{-

Python format mini language

format_spec     ::=  [[fill]align][sign][#][0][width][grouping_option][.precision][type]
fill            ::=  <any character>
align           ::=  "<" | ">" | "=" | "^"
sign            ::=  "+" | "-" | " "
width           ::=  integer
grouping_option ::=  "_" | ","
precision       ::=  integer
type            ::=  "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "n" | "o" | "s" | "x" | "X" | "%"
-}

data TypeFlag = Flagb | Flagc | Flagd | Flage | FlagE | Flagf | FlagF | Flagg | FlagG | Flagn | Flago | Flags | Flagx | FlagX | FlagPercent
  deriving (Show)

data TypeFormat =
    DefaultF Precision -- Default
  | BinaryF AlternateForm -- Binary
  | CharacterF -- Character
  | DecimalF -- Decimal
  | ExponentialF Precision AlternateForm -- exponential notation
  | ExponentialCapsF Precision AlternateForm -- exponentiel notation CAPS
  | FixedF Precision AlternateForm -- fixed point
  | FixedCapsF Precision AlternateForm -- fixed point CAPS
  -- | GeneralF Precision AlternateForm -- General (Not Yet handled)
  -- | GeneralOrExponentialCapsF Precision AlternateForm -- General Switch to E (Not yet handled)
  -- | NumberF Precision AlternateForm -- Number (Not Yet handled)
  | OctalF AlternateForm -- octal
  | StringF Precision -- string
  | HexF AlternateForm -- small hex
  | HexCapsF AlternateForm -- big hex
  | PercentF Precision AlternateForm -- percent
  deriving (Show)

data AlternateForm = AlternateForm | NormalForm
  deriving (Show)

unhandled :: Parser t -> String -> Parser ()
unhandled p err = do
  isP <- optional p

  case isP of
    Nothing -> pure ()
    Just _ -> lastCharFailed err

lastCharFailed :: String -> Parser t
lastCharFailed err = do
  (SourcePos name line col) <- getPosition

  -- This is right as long as there is not line break in the string
  setPosition (SourcePos name line (mkPos (unPos col - 1)))
  fancyFailure (Set.singleton (ErrorFail err))

format_spec :: Parser FormatMode
format_spec = do
  al <- option (Right (AlignCharDefault, AlignDefault)) alignment

  (ac, am) <- case al of
    Left err -> lastCharFailed err
    Right v -> pure v

  unhandled sign "'Sign field' is not handled for now. Please remove it."
  alternateForm <- option NormalForm (AlternateForm <$ char '#')
  unhandled (char '0') "'0' is not handled for now. Please remove it."
  w <- optional width

  let padding = case w of
        Just p -> Padding p am ac
        Nothing -> PaddingDefault

  unhandled grouping_option "'Grouping option' field is not handled for now. Please remove it."
  prec <- option PrecisionDefault (char '.' *> (Precision <$> precision))
  t <- optional type_

  case t of
    Nothing -> pure (FormatMode padding (DefaultF prec))
    Just flag -> case evalFlag flag prec alternateForm of
      Right fmt -> pure (FormatMode padding fmt)
      Left typeError -> do
        lastCharFailed typeError

evalFlag :: TypeFlag -> Precision -> AlternateForm -> Either String TypeFormat
evalFlag Flagb prec alt = failIfPrec prec =<< failIfAlt alt (BinaryF alt)
evalFlag Flagc prec alt = failIfPrec prec =<< failIfAlt alt CharacterF
evalFlag Flagd prec alt = failIfPrec prec =<< failIfAlt alt DecimalF
evalFlag Flage prec alt = Right (ExponentialF prec alt)
evalFlag FlagE prec alt = Right (ExponentialCapsF prec alt)
evalFlag Flagf prec alt = Right (FixedF prec alt)
evalFlag FlagF prec alt = Right (FixedCapsF prec alt)
evalFlag Flagg _prec _alt = Left ("Type 'g' not handled (yet). " <> errgGn)
evalFlag FlagG _prec _alt = Left ("Type 'G' not handled (yet). " <> errgGn)
evalFlag Flagn _prec _alt = Left ("Type 'n' not handled (yet). " <> errgGn)
evalFlag Flago prec alt = failIfPrec prec $ OctalF alt
evalFlag Flags prec alt = failIfAlt alt $ StringF prec
evalFlag Flagx prec alt = failIfPrec prec $ HexF alt
evalFlag FlagX prec alt = failIfPrec prec $ HexCapsF alt
evalFlag FlagPercent prec alt = Right (PercentF prec alt)

errgGn :: String
errgGn = "Use one of {'b', 'c', 'd', 'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 's', 'x', 'X', '%'}."

failIfPrec :: Precision -> TypeFormat -> Either String TypeFormat
failIfPrec PrecisionDefault i = Right i
failIfPrec (Precision i) _ = Left (F.formatToString ("Type incompatible with precision (." F.% F.int F.% "), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 's', '%'} or remove the precision field.") i)

failIfAlt :: AlternateForm -> TypeFormat -> Either String TypeFormat
failIfAlt NormalForm i = Right i
failIfAlt _ _ = Left "Type incompatible with alternative form (#), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the alternative field."

alignment :: Parser (Either String (AlignChar, AlignMode))
alignment = choice [
    try $ do
        c <- fill
        mode <- align
        pure ((\m -> (AlignChar c, m)) <$> mode)
    , do
        mode <- align
        pure ((\m -> (AlignCharDefault, m)) <$> mode)
    ]

fill :: Parser Char
fill = anyChar

align :: Parser (Either String AlignMode)
align = choice [
  Right AlignLeft <$ char '<',
  Right AlignRight <$ char '>',
  Right AlignCenter <$ char '^',
  Left "Align mode '=' is not handled yet" <$ char '='
  ]

sign :: Parser Char
sign = oneOf ("+- " :: [Char])

width :: Parser Integer
width = integer

integer :: Parser Integer
integer = L.decimal -- incomplete: see: https://docs.python.org/3/reference/lexical_analysis.html#grammar-token-integer

grouping_option :: Parser Char
grouping_option = oneOf ("_," :: [Char])

precision :: Parser Integer
precision = integer

type_ :: Parser TypeFlag
type_ = choice [
  Flagb <$ char 'b',
  Flagc <$ char 'c',
  Flagd <$ char 'd',
  Flage <$ char 'e',
  FlagE <$ char 'E',
  Flagf <$ char 'f',
  FlagF <$ char 'F',
  Flagg <$ char 'g',
  FlagG <$ char 'G',
  Flagn <$ char 'n',
  Flago <$ char 'o',
  Flags <$ char 's',
  Flagx <$ char 'x',
  FlagX <$ char 'X',
  FlagPercent <$ char '%'
  ]

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
