{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{- |
This module provides a parser for <https://docs.python.org/3.4/library/string.html#formatspec python format string mini language>.
-}
module PyF.Internal.PythonSyntax
  ( parsePythonFormatString
  , Item(..)
  , FormatMode(..)
  , Padding(..)
  , Precision(..)
  , TypeFormat(..)
  , AlternateForm(..)
  , pattern DefaultFormatMode
  )
where

import Language.Haskell.TH.Syntax

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Data.Void (Void)

import qualified Data.Char

import Data.Maybe (fromMaybe)

import qualified Data.Set as Set -- For fancyFailure
import qualified Language.Haskell.Meta.Syntax.Translate as SyntaxTranslate
import qualified Language.Haskell.Exts.Parser as ParseExp
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import PyF.Formatters

type Parser t = Parsec Void String t

{-
-- TODO:
- Better parsing of integer
- Recursive replacement field, so "{string:.{precision}} can be parsed
- f_expression / conversion
- Not (Yet) implemented:
     - types: n
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

-- | A format string is composed of many chunks of raw string or replacement
data Item = Raw String -- ^ A raw string
           | Replacement Exp (Maybe FormatMode) -- ^ A replacement string, composed of an arbitrary Haskell expression followed by an optional formatter
           deriving (Show)

{- |
Parse a string, returns a list of raw string or replacement fields
-}
parsePythonFormatString :: Parser [Item]
parsePythonFormatString = many (rawString <|> escapedParenthesis <|> replacementField)

rawString :: Parser Item
rawString = Raw . escapeChars <$> some (noneOf ("{}" :: [Char]))

escapedParenthesis :: Parser Item
escapedParenthesis = Raw <$> ("{{" <|> "}}")

{- | Replace escape chars with their value
>>> escapeChars "hello \\n"
"hello \n"
-}
escapeChars :: String -> String
escapeChars "" = ""
escapeChars s = case Data.Char.readLitChar s of
                  [] -> ""
                  ((c, xs):_) -> c : escapeChars xs

replacementField :: Parser Item
replacementField = do
  _ <- char '{'
  expr <- evalExpr (many (noneOf ("}:" :: [Char])))
  fmt <- optional $ do
    _ <- char ':'
    format_spec
  _ <- char '}'

  pure (Replacement expr fmt)

-- | Default formating mode, no padding, default precision, no grouping, no sign handling
pattern DefaultFormatMode :: FormatMode
pattern DefaultFormatMode = FormatMode PaddingDefault (DefaultF PrecisionDefault Minus) Nothing

-- | A Formatter, listing padding, format and and grouping char
data FormatMode = FormatMode Padding TypeFormat (Maybe Char)
                deriving (Show)

-- | Padding, containing the padding width, the padding char and the alignement mode
data Padding = PaddingDefault
             | Padding Integer (Maybe (Maybe Char, AnyAlign))
             deriving (Show)

-- | Floating point precision
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

-- Note : 'n' is not handled

data TypeFlag = Flagb | Flagc | Flagd | Flage | FlagE | Flagf | FlagF | Flagg | FlagG | Flago | Flags | Flagx | FlagX | FlagPercent
  deriving (Show)

-- TODO: DefaultF and precision
-- | All formating type
data TypeFormat where
  TypeFormatIntegral :: Format k k' 'Integral -> SignMode -> TypeFormat
  TypeFormatFractional :: Format k k' 'Fractional -> SignMode -> Precision -> TypeFormat
  TypeFormatString :: Precision -> TypeFormat
  -- ^ Simple string
  DefaultF :: Precision -> SignMode -> TypeFormat
  -- ^ Default, depends on the infered type of the expression

deriving instance Show TypeFormat

-- | If the formatter use its alternate form
data AlternateForm = AlternateForm | NormalForm
  deriving (Show)

lastCharFailed :: String -> Parser t
lastCharFailed err = do
  (SourcePos name line col) <- getPosition

  -- This is right as long as there is not line break in the string
  setPosition (SourcePos name line (mkPos (unPos col - 1)))
  fancyFailure (Set.singleton (ErrorFail err))

evalExpr :: Parser String -> Parser Exp
evalExpr exprParser = do
  (SourcePos name line col) <- getPosition
  s <- exprParser
  case SyntaxTranslate.toExp <$> ParseExp.parseExp s of
    ParseExp.ParseOk expr -> pure expr
    ParseExp.ParseFailed (SrcLoc.SrcLoc _name' line' col') err -> do
      let realLine = mkPos (line' + unPos line - 1)
          realCol = if line' == 1
                    then mkPos (col' + unPos col - 1)
                    else mkPos col'
      setPosition (SourcePos name realLine realCol)
      fancyFailure (Set.singleton (ErrorFail err))

overrideAlignmentIfZero :: Bool -> Maybe (Maybe Char, AnyAlign) -> Maybe (Maybe Char, AnyAlign)
overrideAlignmentIfZero True Nothing = Just (Just '0', AnyAlign AlignInside)
overrideAlignmentIfZero True (Just (Nothing, al)) = Just (Just '0', al)
overrideAlignmentIfZero _ v = v

format_spec :: Parser FormatMode
format_spec = do
  al' <- optional alignment
  s <- optional sign
  alternateForm <- option NormalForm (AlternateForm <$ char '#')

  hasZero <- option False (True <$ char '0')

  let al = overrideAlignmentIfZero hasZero al'

  w <- optional width

  grouping <- optional grouping_option

  prec <- option PrecisionDefault (char '.' *> (Precision <$> precision))
  t <- optional type_

  let padding = case w of
        Just p -> Padding p al
        Nothing -> PaddingDefault

  case t of
    Nothing -> pure (FormatMode padding (DefaultF prec (fromMaybe Minus s)) grouping)
    Just flag -> case evalFlag flag prec alternateForm s of
      Right fmt -> pure (FormatMode padding fmt grouping)
      Left typeError -> do
        lastCharFailed typeError

data FormatTag (k :: FormatType) where
  TagIntegral :: FormatTag 'Integral
  TagFractional :: FormatTag 'Fractional
  TagString :: FormatTag 'StringType

data AnyFormat where
  AnyFormat :: Format k k' k'' -> FormatTag k'' -> AnyFormat

altIf :: AlternateForm -> Format 'CanAlt k k' -> FormatTag k' -> AnyFormat
altIf NormalForm t tag = AnyFormat t tag
altIf AlternateForm t tag = AnyFormat (Alternate t) tag

altUpperIf :: AlternateForm -> Format 'CanAlt 'CanUpper k -> FormatTag k -> AnyFormat
altUpperIf NormalForm t tag = AnyFormat (Upper t) tag
altUpperIf AlternateForm t tag = AnyFormat (Upper (Alternate t)) tag

typeFlagToFormat :: TypeFlag -> AlternateForm -> Either String AnyFormat
typeFlagToFormat Flagb alt = Right $ altIf alt Binary TagIntegral
typeFlagToFormat Flagc NormalForm = Right $ AnyFormat Character TagIntegral
typeFlagToFormat Flagd NormalForm = Right $ AnyFormat Decimal TagIntegral
typeFlagToFormat Flage alt = Right $ altIf alt Exponent TagFractional
typeFlagToFormat FlagE alt = Right $ altUpperIf alt Exponent TagFractional
typeFlagToFormat Flagf alt = Right $ altIf alt Fixed TagFractional
typeFlagToFormat FlagF alt = Right $ altUpperIf alt Fixed TagFractional
typeFlagToFormat Flagg alt = Right $ altIf alt Generic TagFractional
typeFlagToFormat FlagG alt = Right $ altUpperIf alt Generic TagFractional
typeFlagToFormat Flago alt = Right $ altIf alt Octal TagIntegral
typeFlagToFormat Flags NormalForm = Right $ AnyFormat StringF TagString
typeFlagToFormat Flagx alt = Right $ altIf alt Hexa TagIntegral
typeFlagToFormat FlagX alt = Right $ altUpperIf alt Hexa TagIntegral
typeFlagToFormat FlagPercent alt = Right $ altIf alt Percent TagFractional
typeFlagToFormat f AlternateForm = Left $ "Type " ++ show f ++ "incompatible with alternative form (#), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the alternative field."

evalFlag :: TypeFlag -> Precision -> AlternateForm -> Maybe SignMode -> Either String TypeFormat
evalFlag typeFlag pre alt s = do
  AnyFormat format tag <- typeFlagToFormat typeFlag alt

  case tag of
    TagIntegral -> failIfPrec pre (TypeFormatIntegral format (defSign s))
    TagFractional -> Right $ TypeFormatFractional format (defSign s) pre
    TagString -> failIfS s (TypeFormatString pre) -- check no sign and pre

defSign :: Maybe SignMode -> SignMode
defSign Nothing = Minus
defSign (Just s) = s

failIfPrec :: Precision -> TypeFormat -> Either String TypeFormat
failIfPrec PrecisionDefault i = Right i
failIfPrec (Precision i) _ = Left ("Type incompatible with precision (." ++ show i ++ "), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 's', '%'} or remove the precision field.")

failIfS :: Maybe SignMode -> TypeFormat -> Either String TypeFormat
failIfS Nothing i = Right i
failIfS (Just s) _ = Left ("Type incompatible with sign field (" ++ [toSignMode s] ++ "), use any of {'b', 'd', 'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the sign field.")

toSignMode :: SignMode -> Char
toSignMode Plus = '+'
toSignMode Minus = '-'
toSignMode Space = ' '

alignment :: Parser (Maybe Char, AnyAlign)
alignment = choice [
    try $ do
        c <- fill
        mode <- align
        pure (Just c, mode)
    , do
        mode <- align
        pure (Nothing, mode)
    ]

fill :: Parser Char
fill = anyChar

align :: Parser AnyAlign
align = choice [
  AnyAlign AlignLeft <$ char '<',
  AnyAlign AlignRight <$ char '>',
  AnyAlign AlignCenter <$ char '^',
  AnyAlign AlignInside <$ char '='
  ]

sign :: Parser SignMode
sign = choice
  [Plus <$ char '+',
   Minus <$ char '-',
   Space <$ char ' '
  ]

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
  Flago <$ char 'o',
  Flags <$ char 's',
  Flagx <$ char 'x',
  FlagX <$ char 'X',
  FlagPercent <$ char '%'
  ]


  -- TODO: remove !
deriving instance Lift Precision
deriving instance Lift Padding
