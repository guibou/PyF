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
pattern DefaultFormatMode = FormatMode PaddingDefault PrecisionDefault TypeDefault NormalForm

data FormatMode = FormatMode Padding Precision TypeFormat AlternateForm
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

data TypeFormat = TypeDefault
                | Typeb -- Binary
                | Typec -- Character
                | Typed -- Decimal
                | Typee -- exponential notation
                | TypeE -- Exp notation
                | Typef -- fixed point
                | TypeF -- fixed point (Caps NAN and INF)
                | Typeg -- General ?
                | TypeG -- General
                | Typen -- Number ?
                | Typeo -- octal
                | Types -- STRING
                | Typex -- small hex
                | TypeX -- big hex
                | TypePercent
                deriving (Show)

data AlternateForm = AlternateForm | NormalForm
  deriving (Show)

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
  t <- option TypeDefault type_

  pure (FormatMode padding prec t alternateForm)

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

type_ :: Parser TypeFormat
type_ = choice [
  Typeb <$ char 'b',
  Typec <$ char 'c',
  Typed <$ char 'd',
  Typee <$ char 'e',
  TypeE <$ char 'E',
  Typef <$ char 'f',
  TypeF <$ char 'F',
  Typeg <$ char 'g',
  TypeG <$ char 'G',
  Typen <$ char 'n',
  Typeo <$ char 'o',
  Types <$ char 's',
  Typex <$ char 'x',
  TypeX <$ char 'X',
  TypePercent <$ char '%'
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
padAndFormat (FormatMode pad prec t alt) = applyPadding pad =<< format prec t alt

format :: Precision -> TypeFormat -> AlternateForm -> Q Exp

-- default case type, precision must depends on the different sub types (string, int, float)
-- TODO: handle it.
format (Precision i) TypeDefault alt = assertNF alt $ [| F.build |]
format PrecisionDefault TypeDefault alt = assertNF alt $ [| F.build |]

-- String types. TODO: handled clipping with precision
format (Precision i) Types alt = assertNF alt $ [| F.build |]
format PrecisionDefault Types alt = assertNF alt $ [| F.build |]

-- integer types, precision should not exists
format PrecisionDefault Typeb alt = [| $(ifAlternate alt "0b") F.% F.bin |]
format PrecisionDefault Typed alt = assertNF alt $ [| F.build |] -- TODO: check that it is a number
format PrecisionDefault Typeo alt = [| $(ifAlternate alt "0o") F.% F.oct |]
format PrecisionDefault Typex alt = [| $(ifAlternate alt "0x") F.% F.hex |]
format PrecisionDefault TypeX alt = [| $(ifAlternate alt "0x") F.% toUpper F.%. F.hex |]
format PrecisionDefault Typec _alt = error "Type Char is not handled"

-- There is number for integer and float, find a way to discriminate them TODO.
format PrecisionDefault Typen _alt = error "Type n is not handled"

-- Floating point types
-- TODO: NaN and Inf are bugged
format prec Typee alt = assertNFYet alt $ [| F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format prec TypeE alt = assertNFYet alt $ [| toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format prec Typef alt = assertNFYet alt $ [| F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format prec TypeF alt = assertNFYet alt $ [| toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format _prec Typeg _alt = error "type g is not handled"
format _prec TypeG _alt = error "type G is not handled"
format _prec Typen _alt = error "type n is not handled"
format prec TypePercent alt = assertNFYet alt $ [| F.mapf (*100) (F.scifmt Scientific.Fixed $(precToMaybe prec)) F.% "%" |]
format a b c = fail (show (a, b, c))

ifAlternate :: AlternateForm -> String -> Q Exp
ifAlternate NormalForm _ = [| F.now (Builder.fromString "") |]
ifAlternate AlternateForm s = [| F.now (Builder.fromString s) |]

-- TODO: remove it
assertNF :: AlternateForm -> t -> t
assertNF NormalForm b = b
assertNF AlternateForm _ = error "this formater does not support alternate form"

-- TODO: handle them
assertNFYet :: AlternateForm -> t -> t
assertNFYet NormalForm b = b
assertNFYet AlternateForm _ = error "this formater does not support alternate form (Yet)"

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
