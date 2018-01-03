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
import qualified Data.Text.Lazy as Text

import Data.Monoid ((<>))

type Parser t = Parsec Void String t


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
pattern DefaultFormatMode = FormatMode PaddingDefault PrecisionDefault (TNormal TypeDefault)

data FormatMode = FormatMode Padding Precision TQualifiedFormat
                deriving (Show)

data AlignMode = AlignLeft | AlignRight | AlignInside | AlignCenter | AlignDefault
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

data HasAlt = HasAlt | NoAlt

data TypeFormat (haltAlt :: HasAlt) where
  TypeDefault :: TypeFormat 'NoAlt
  Typeb :: TypeFormat 'HasAlt -- Binary
  Typec :: TypeFormat 'NoAlt -- Character
  Typed :: TypeFormat 'NoAlt -- Decimal
  Typee :: TypeFormat 'HasAlt -- exponential notation
  TypeE :: TypeFormat 'HasAlt -- Exp notation
  Typef :: TypeFormat 'HasAlt -- fixed point
  TypeF :: TypeFormat 'HasAlt -- fixed point (Caps NAN and INF)
  Typeg :: TypeFormat 'HasAlt -- General ?
  TypeG :: TypeFormat 'HasAlt -- General
  Typen :: TypeFormat 'HasAlt -- Number ?
  Typeo :: TypeFormat 'HasAlt -- octal
  Types :: TypeFormat 'NoAlt -- STRING
  Typex :: TypeFormat 'HasAlt -- small hex
  TypeX :: TypeFormat 'HasAlt -- big hex
  TypePercent :: TypeFormat 'HasAlt -- percent

deriving instance Show (TypeFormat t)

data AlternateForm = AlternateForm | NormalForm
  deriving (Show)

data UnqualifiedTypeFormat = UnqualifiedHasAlt (TypeFormat 'HasAlt)
                           | UnqualifiedNoAlt (TypeFormat 'NoAlt)
                           deriving (Show)

data TQualifiedFormat = TNormal (TypeFormat 'NoAlt)
                      | TAlternate (TypeFormat 'HasAlt) AlternateForm
                      deriving (Show)

qualifyFormat :: AlternateForm -> UnqualifiedTypeFormat -> Maybe TQualifiedFormat
qualifyFormat form (UnqualifiedHasAlt c) = Just $ TAlternate c form
qualifyFormat NormalForm (UnqualifiedNoAlt c) = Just $ TNormal c
qualifyFormat AlternateForm (UnqualifiedNoAlt _) = Nothing

upgrade :: UnqualifiedTypeFormat -> TQualifiedFormat
upgrade (UnqualifiedHasAlt c) = TAlternate c NormalForm
upgrade (UnqualifiedNoAlt c) = TNormal c

upgradeAlt :: UnqualifiedTypeFormat -> Maybe TQualifiedFormat
upgradeAlt (UnqualifiedHasAlt c) = Just (TAlternate c AlternateForm)
upgradeAlt (UnqualifiedNoAlt _) = Nothing

-- Ignored for now: sign / grouping_option / 0 / sharp

format_spec :: Parser FormatMode
format_spec = do
  (ac, am) <- option (AlignCharDefault, AlignDefault) alignment
  _s <- optional sign
  alternateForm <- option NormalForm (AlternateForm <$ char '#')
  _zero <- optional (char '0')
  w <- optional width

  let padding = case w of
        Just p -> Padding p am ac
        Nothing -> PaddingDefault

  _go <- optional grouping_option
  prec <- option PrecisionDefault (char '.' *> (Precision <$> precision))
  t <- option (UnqualifiedNoAlt TypeDefault) type_

  let t' = qualifyFormat alternateForm t

  case t' of
    Just t'' -> pure (FormatMode padding prec t'')
    Nothing -> fail "format qualifier broken with that format"

alignment :: Parser (AlignChar, AlignMode)
alignment = choice [
    try $ do
        c <- fill
        mode <- align
        pure (AlignChar c, mode)
    , do
        mode <- align
        pure (AlignCharDefault, mode)
    ]

fill :: Parser Char
fill = anyChar

align :: Parser AlignMode
align = choice [
  AlignLeft <$ char '<',
  AlignRight <$ char '>',
  AlignInside <$ char '=',
  AlignCenter <$ char '^'
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

type_ :: Parser UnqualifiedTypeFormat
type_ = choice [
  UnqualifiedHasAlt Typeb <$ char 'b',
  UnqualifiedNoAlt Typec <$ char 'c',
  UnqualifiedNoAlt Typed <$ char 'd',
  UnqualifiedHasAlt Typee <$ char 'e',
  UnqualifiedHasAlt TypeE <$ char 'E',
  UnqualifiedHasAlt Typef <$ char 'f',
  UnqualifiedHasAlt TypeF <$ char 'F',
  UnqualifiedHasAlt Typeg <$ char 'g',
  UnqualifiedHasAlt TypeG <$ char 'G',
  UnqualifiedHasAlt Typen <$ char 'n',
  UnqualifiedHasAlt Typeo <$ char 'o',
  UnqualifiedNoAlt Types <$ char 's',
  UnqualifiedHasAlt Typex <$ char 'x',
  UnqualifiedHasAlt TypeX <$ char 'X',
  UnqualifiedHasAlt TypePercent <$ char '%'
  ]

{-
-- TODO:
- Better parsing of integer
- Beautiful type (if possible, with GADTs
- Recursive replacement field, so "{string:.{precision}} can be parsed
- f_expression / conversion
-}

{-
*Yak.PythonSyntax> name = "Guillaume"
*Yak.PythonSyntax> cash = 100
*Yak.PythonSyntax> [yak|"hello {name} your cash is {cash:.3}"]
"hello Guillaume your cash is 100.000"
-}

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
toExp s = case parseMaybe result s of
    Nothing -> fail "yo, des rabins"
    Just items -> do
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
padAndFormat (FormatMode pad prec t) = applyPadding pad =<< format prec t

format :: Precision -> TQualifiedFormat -> Q Exp

-- default case type, precision must depends on the different sub types (string, int, float)
-- TODO: handle it.
format (Precision _TODOi) (TNormal TypeDefault) = [| F.build |]
format PrecisionDefault (TNormal TypeDefault) = [| F.build |]

-- String types. TODO: handled clipping with precision
format (Precision _TODOi) (TNormal Types) = [| F.build |]
format PrecisionDefault (TNormal Types) = [| F.build |]

-- integer types, precision should not exists
format PrecisionDefault (TAlternate Typeb alt) = [| $(ifAlternate alt "0b") F.% F.bin |]
format PrecisionDefault (TNormal Typed) = [| F.build |] -- TODO: check that it is a number
format PrecisionDefault (TAlternate Typeo alt) = [| $(ifAlternate alt "0o") F.% F.oct |]
format PrecisionDefault (TAlternate Typex alt) = [| $(ifAlternate alt "0x") F.% F.hex |]
format PrecisionDefault (TAlternate TypeX alt) = [| $(ifAlternate alt "0x") F.% toUpper F.%. F.hex |]
format PrecisionDefault (TNormal Typec) = error "Type Char is not handled"

-- There is number for integer and float, find a way to discriminate them TODO.
format PrecisionDefault (TAlternate Typen _TODOalt) = error "Type n is not handled"

-- Floating point types
-- TODO: NaN and Inf are bugged
format prec (TAlternate Typee _TODOalt) = [| F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format prec (TAlternate TypeE _TODOalt) = [| toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format prec (TAlternate Typef _TODOalt) = [| F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format prec (TAlternate TypeF _TODOalt) = [| toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format _prec (TAlternate Typeg _TODOalt) = error "type g is not handled"
format _prec (TAlternate TypeG _TODOalt) = error "type G is not handled"
format _prec (TAlternate Typen _TODOalt) = error "type n is not handled"
format prec (TAlternate TypePercent _TODOalt) = [| F.mapf (*100) (F.scifmt Scientific.Fixed $(precToMaybe prec)) F.% "%" |]
format a b = fail (show (a, b))

ifAlternate :: AlternateForm -> String -> Q Exp
ifAlternate NormalForm _ = [| F.now (Builder.fromString "") |]
ifAlternate AlternateForm s = [| F.now (Builder.fromString s) |]

opLater :: (Text.Text -> Text.Text) -> F.Format r (Builder.Builder -> r)
opLater op = F.later (Builder.fromLazyText . op . Builder.toLazyText)

toUpper :: F.Format r (Builder.Builder -> r)
toUpper = opLater Text.toUpper

alternateFloat :: AlternateForm -> F.Format r (Builder.Builder -> r)
alternateFloat NormalForm = opLater id
alternateFloat AlternateForm = opLater f
  where f t = case Text.find (=='.') t of
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
          AlignInside -> undefined

    padChar = case ac of
          AlignCharDefault -> ' '
          AlignChar c -> c

-- To Scientific
class ToScientific t where
  toScientific :: t -> Scientific.Scientific

instance {-# OVERLAPS #-} RealFloat t => ToScientific t where
  toScientific = Scientific.fromFloatDigits

instance ToScientific Scientific.Scientific where
  toScientific = id
