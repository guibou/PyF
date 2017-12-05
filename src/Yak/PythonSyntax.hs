{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
pattern DefaultFormatMode = FormatMode PaddingDefault PrecisionDefault TypeDefault

data FormatMode = FormatMode Padding Precision TypeFormat
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

-- Ignored for now: sign / grouping_option / 0 / sharp

format_spec :: Parser FormatMode
format_spec = do
  (ac, am) <- option (AlignCharDefault, AlignDefault) alignment
  _s <- optional sign
  _sharp <- optional (char '#')
  _zero <- optional (char '0')
  w <- optional width

  let padding = case w of
        Just p -> Padding p am ac
        Nothing -> PaddingDefault

  _go <- optional grouping_option
  prec <- option PrecisionDefault (char '.' *> (Precision <$> precision))
  t <- option TypeDefault type_

  pure (FormatMode padding prec t)

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
toFormat (Replacement _ y) = format (fromMaybe DefaultFormatMode y)

format :: FormatMode -> Q Exp
format DefaultFormatMode = [| F.build |]

-- default case type, padding is generic, but precision depends on the different sub types (string, int, float)
-- TODO: handle it.
format (FormatMode pad (Precision i) TypeDefault) = [| $(padnow pad)|]
format (FormatMode pad PrecisionDefault TypeDefault) = [| $(padnow pad)|]

-- String types. TODO: handled clipping with precision
format (FormatMode pad (Precision i) Types) = [| $(padnow pad)|]
format (FormatMode pad PrecisionDefault Types) = [| $(padnow pad) |]

-- integer types, precision should not exists
format (FormatMode pad PrecisionDefault Typeb) = [| $(padnow pad) F.%. F.bin |]
format (FormatMode pad PrecisionDefault Typed) = [| $(padnow pad) |]
format (FormatMode pad PrecisionDefault Typeo) = [| $(padnow pad) F.%. F.oct |]
format (FormatMode pad PrecisionDefault Typex) = [| $(padnow pad) F.%. F.hex |]
format (FormatMode pad PrecisionDefault TypeX) = [| $(padnow pad) F.%. toUpper F.%. F.hex |]
format (FormatMode pad PrecisionDefault Typec) = undefined
format (FormatMode pad PrecisionDefault Typen) = undefined

-- Floating point types
-- TODO: NaN and Inf are bugged
format (FormatMode pad prec Typee) = [| $(padnow pad) F.%. F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format (FormatMode pad prec TypeE) = [| $(padnow pad) F.%. toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Exponent $(precToMaybe prec)) |]
format (FormatMode pad prec Typef) = [| $(padnow pad) F.%. F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format (FormatMode pad prec TypeF) = [| $(padnow pad) F.%. toUpper F.%. F.mapf toScientific (F.scifmt Scientific.Fixed $(precToMaybe prec)) |]
format (FormatMode pad prec Typeg) = undefined
format (FormatMode pad prec TypeG) = undefined
format (FormatMode pad prec Typen) = undefined
format (FormatMode pad prec TypePercent) = [| ($(padnow pad) F.%. F.mapf (*100) (F.scifmt Scientific.Fixed $(precToMaybe prec))) F.% "%" |]
format f = fail (show f)

toUpper :: F.Format r (Builder.Builder -> r)
toUpper = F.later (Builder.fromLazyText . Text.toUpper . Builder.toLazyText)

precToMaybe :: Precision -> Q Exp
precToMaybe p = [| Just $(precToInt p) |] -- Default precision from python

precToInt :: Precision -> Q Exp
precToInt PrecisionDefault = [| 6 |] -- Default precision from python
precToInt (Precision i) = [| i |]

padnow :: Padding -> Q Exp
padnow PaddingDefault = [| F.left 0 '0' |] -- HACK: no padding is a 0 padding ?
padnow (Padding i am ac) = [| $(padFunc) i padChar |]
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
