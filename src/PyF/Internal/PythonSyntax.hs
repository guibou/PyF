{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- This module provides a parser for <https://docs.python.org/3.4/library/string.html#formatspec python format string mini language>.
module PyF.Internal.PythonSyntax
  ( parseGenericFormatString,
    Item (..),
    FormatMode (..),
    Padding (..),
    Precision (..),
    TypeFormat (..),
    AlternateForm (..),
    pattern DefaultFormatMode,
    Parser,
    ParsingContext (..),
    ExprOrValue (..),
  )
where

import Control.Applicative (some)
import Control.Monad.Reader
import qualified Data.Char
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH.LanguageExtensions as ParseExtension
import Language.Haskell.TH.Syntax (Exp)
import PyF.Formatters
import PyF.Internal.Meta
import qualified PyF.Internal.Parser as ParseExp
import PyF.Internal.ParserEx (applyFixities, baseFixities, preludeFixities)
import Text.Parsec

type Parser t = ParsecT String () (Reader ParsingContext) t

data ParsingContext = ParsingContext
  { delimiters :: (Char, Char),
    enabledExtensions :: [ParseExtension.Extension]
  }
  deriving (Show)

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
data Item
  = -- | A raw string
    Raw String
  | -- | A replacement string, composed of an arbitrary Haskell expression followed by an optional formatter
    Replacement Exp (Maybe FormatMode)
  deriving (Show)

-- |
-- Parse a string, returns a list of raw string or replacement fields
--
-- >>> import Text.Megaparsec
-- >>> parse parsePythonFormatString "" "hello {1+1:>10.2f}"
-- Right [
--        Raw "hello ",
--        Replacement "1+1"
--        (
--        Just (FormatMode
--                       (Padding 10 (Just (Nothing,AnyAlign AlignRight)))
--                       (FixedF (Precision 2) NormalForm Minus)
--                        Nothing))]
parseGenericFormatString :: Parser [Item]
parseGenericFormatString =
  many (rawString <|> escapedParenthesis <|> replacementField) <* eof

rawString :: Parser Item
rawString = do
  (openingChar, closingChar) <- asks delimiters

  -- lookahead
  let p = some (noneOf [openingChar, closingChar])
  chars <- lookAhead p

  case escapeChars chars of
    Left remaining -> do
      -- Consume up to the error location
      void $ count (length chars - length remaining) anyChar
      fail "Lexical error in literal section"
    Right escaped -> do
      -- Consumne everything
      void p
      return (Raw escaped)

escapedParenthesis :: Parser Item
escapedParenthesis = do
  (openingChar, closingChar) <- asks delimiters
  Raw <$> (parseRaw openingChar <|> parseRaw closingChar)
  where
    parseRaw c = [c] <$ try (string (replicate 2 c))

-- | Replace escape chars with their value. Results in a Left with the
-- remainder of the string on encountering a lexical error (such as a bad escape
-- sequence).
-- >>> escapeChars "hello \\n"
-- Right "hello \n"
-- >>> escapeChars "hello \\x"
-- Left "\\x"
escapeChars :: String -> Either String String
escapeChars "" = Right ""
escapeChars ('\\' : '\n' : xs) = escapeChars xs
escapeChars ('\\' : '\\' : xs) = ('\\' :) <$> escapeChars xs
escapeChars s = case Data.Char.readLitChar s of
  ((c, xs) : _) -> (c :) <$> escapeChars xs
  _ -> Left s

replacementField :: Parser Item
replacementField = do
  exts <- asks enabledExtensions
  (charOpening, charClosing) <- asks delimiters
  _ <- char charOpening
  expr <- evalExpr exts (some (noneOf (charClosing : ":" :: String)) <?> "an haskell expression")
  fmt <- optionMaybe $ do
    _ <- char ':'
    formatSpec
  _ <- char charClosing
  pure (Replacement expr fmt)

-- | Default formating mode, no padding, default precision, no grouping, no sign handling
pattern DefaultFormatMode :: FormatMode
pattern DefaultFormatMode = FormatMode PaddingDefault (DefaultF PrecisionDefault Minus) Nothing

-- | A Formatter, listing padding, format and and grouping char
data FormatMode = FormatMode Padding TypeFormat (Maybe Char)
  deriving (Show)

-- | Padding, containing the padding width, the padding char and the alignement mode
data Padding
  = PaddingDefault
  | Padding Integer (Maybe (Maybe Char, AnyAlign))
  deriving (Show)

-- | Represents a value of type @t@ or an Haskell expression supposed to represents that value
data ExprOrValue t
  = Value t
  | HaskellExpr Exp
  deriving (Show)

-- | Floating point precision
data Precision
  = PrecisionDefault
  | Precision (ExprOrValue Integer)
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

-- | All formating type
data TypeFormat
  = -- | Default, depends on the infered type of the expression
    DefaultF Precision SignMode
  | -- | Binary, such as `0b0121`
    BinaryF AlternateForm SignMode
  | -- | Character, will convert an integer to its character representation
    CharacterF
  | -- | Decimal, base 10 integer formatting
    DecimalF SignMode
  | -- | Exponential notation for floatting points
    ExponentialF Precision AlternateForm SignMode
  | -- | Exponential notation with capitalised @e@
    ExponentialCapsF Precision AlternateForm SignMode
  | -- | Fixed number of digits floating point
    FixedF Precision AlternateForm SignMode
  | -- | Capitalized version of the previous
    FixedCapsF Precision AlternateForm SignMode
  | -- | General formatting: `FixedF` or `ExponentialF` depending on the number magnitude
    GeneralF Precision AlternateForm SignMode
  | -- | Same as `GeneralF` but with upper case @E@ and infinite / NaN
    GeneralCapsF Precision AlternateForm SignMode
  | -- | Octal, such as 00245
    OctalF AlternateForm SignMode
  | -- | Simple string
    StringF Precision
  | -- | Hexadecimal, such as 0xaf3e
    HexF AlternateForm SignMode
  | -- | Hexadecimal with capitalized letters, such as 0XAF3E
    HexCapsF AlternateForm SignMode
  | -- | Percent representation
    PercentF Precision AlternateForm SignMode
  deriving (Show)

-- | If the formatter use its alternate form
data AlternateForm = AlternateForm | NormalForm
  deriving (Show)

evalExpr :: [ParseExtension.Extension] -> Parser String -> Parser Exp
evalExpr exts exprParser = do
  s <- lookAhead exprParser
  -- Setup the dyn flags using the provided list of extensions
  let exts' = fmap translateTHtoGHCExt exts
  let dynFlags = baseDynFlags exts'
  case ParseExp.parseExpression s dynFlags of
    Right expr -> do
      -- Consumne the expression
      void exprParser
      pure (toExp dynFlags (applyFixities (preludeFixities ++ baseFixities) expr))
    Left (lineError, colError, err) -> do
      -- Skip lines
      replicateM_ (lineError - 1) (manyTill anyChar newline)
      -- Skip columns
      void $ count (colError - 2) anyChar

      fail $ err <> " in haskell expression"

overrideAlignmentIfZero :: Bool -> Maybe (Maybe Char, AnyAlign) -> Maybe (Maybe Char, AnyAlign)
overrideAlignmentIfZero True Nothing = Just (Just '0', AnyAlign AlignInside)
overrideAlignmentIfZero True (Just (Nothing, al)) = Just (Just '0', al)
overrideAlignmentIfZero _ v = v

formatSpec :: Parser FormatMode
formatSpec = do
  al' <- optionMaybe alignment
  s <- optionMaybe sign
  alternateForm <- option NormalForm (AlternateForm <$ char '#')
  hasZero <- option False (True <$ char '0')
  let al = overrideAlignmentIfZero hasZero al'
  w <- optionMaybe width
  grouping <- optionMaybe groupingOption
  prec <- option PrecisionDefault parsePrecision

  t <- optionMaybe $ lookAhead type_
  let padding = case w of
        Just p -> Padding p al
        Nothing -> PaddingDefault
  case t of
    Nothing -> pure (FormatMode padding (DefaultF prec (fromMaybe Minus s)) grouping)
    Just flag -> case evalFlag flag padding grouping prec alternateForm s of
      Right fmt -> do
        -- Consumne the parser
        void type_
        pure (FormatMode padding fmt grouping)
      Left typeError ->
        fail typeError

parsePrecision :: Parser Precision
parsePrecision = do
  exts <- asks enabledExtensions
  (charOpening, charClosing) <- asks delimiters
  _ <- char '.'
  choice
    [ Precision . Value <$> precision,
      char charOpening *> (Precision . HaskellExpr <$> evalExpr exts (someTill (satisfy (/= charClosing)) (char charClosing) <?> "an haskell expression"))
    ]

-- | Similar to 'manyTill' but always parse one element.
-- Be careful, @someTill p e@ may parse @e@ as first element if @e@ is a subset of @p@.
someTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
someTill p e = (:) <$> p <*> manyTill p e

evalFlag :: TypeFlag -> Padding -> Maybe Char -> Precision -> AlternateForm -> Maybe SignMode -> Either String TypeFormat
evalFlag Flagb _pad _grouping prec alt s = failIfPrec prec (BinaryF alt (defSign s))
evalFlag Flagc _pad _grouping prec alt s = failIfS s =<< failIfPrec prec =<< failIfAlt alt CharacterF
evalFlag Flagd _pad _grouping prec alt s = failIfPrec prec =<< failIfAlt alt (DecimalF (defSign s))
evalFlag Flage _pad _grouping prec alt s = pure $ ExponentialF prec alt (defSign s)
evalFlag FlagE _pad _grouping prec alt s = pure $ ExponentialCapsF prec alt (defSign s)
evalFlag Flagf _pad _grouping prec alt s = pure $ FixedF prec alt (defSign s)
evalFlag FlagF _pad _grouping prec alt s = pure $ FixedCapsF prec alt (defSign s)
evalFlag Flagg _pad _grouping prec alt s = pure $ GeneralF prec alt (defSign s)
evalFlag FlagG _pad _grouping prec alt s = pure $ GeneralCapsF prec alt (defSign s)
evalFlag Flagn _pad _grouping _prec _alt _s = Left ("Type 'n' not handled (yet). " ++ errgGn)
evalFlag Flago _pad _grouping prec alt s = failIfPrec prec $ OctalF alt (defSign s)
evalFlag Flags pad grouping prec alt s = failIfGrouping grouping =<< failIfInsidePadding pad =<< failIfS s =<< failIfAlt alt (StringF prec)
evalFlag Flagx _pad _grouping prec alt s = failIfPrec prec $ HexF alt (defSign s)
evalFlag FlagX _pad _grouping prec alt s = failIfPrec prec $ HexCapsF alt (defSign s)
evalFlag FlagPercent _pad _grouping prec alt s = pure $ PercentF prec alt (defSign s)

defSign :: Maybe SignMode -> SignMode
defSign Nothing = Minus
defSign (Just s) = s

failIfGrouping :: Maybe Char -> TypeFormat -> Either String TypeFormat
failIfGrouping (Just _) _t = Left "String type is incompatible with grouping (_ or ,)."
failIfGrouping Nothing t = Right t

failIfInsidePadding :: Padding -> TypeFormat -> Either String TypeFormat
failIfInsidePadding (Padding _ (Just (_, AnyAlign AlignInside))) _t = Left "String type is incompatible with inside padding (=)."
failIfInsidePadding _ t = Right t

errgGn :: String
errgGn = "Use one of {'b', 'c', 'd', 'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 's', 'x', 'X', '%'}."

failIfPrec :: Precision -> TypeFormat -> Either String TypeFormat
failIfPrec PrecisionDefault i = Right i
failIfPrec (Precision e) _ = Left ("Type incompatible with precision (." ++ showExpr ++ "), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 's', '%'} or remove the precision field.")
  where
    showExpr = case e of
      Value v -> show v
      HaskellExpr expr -> show expr

failIfAlt :: AlternateForm -> TypeFormat -> Either String TypeFormat
failIfAlt NormalForm i = Right i
failIfAlt _ _ = Left "Type incompatible with alternative form (#), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the alternative field."

failIfS :: Maybe SignMode -> TypeFormat -> Either String TypeFormat
failIfS Nothing i = Right i
failIfS (Just s) _ = Left ("Type incompatible with sign field (" ++ [toSignMode s] ++ "), use any of {'b', 'd', 'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the sign field.")

toSignMode :: SignMode -> Char
toSignMode Plus = '+'
toSignMode Minus = '-'
toSignMode Space = ' '

alignment :: Parser (Maybe Char, AnyAlign)
alignment =
  choice
    [ try $ do
        c <- fill
        mode <- align
        pure (Just c, mode),
      do
        mode <- align
        pure (Nothing, mode)
    ]

fill :: Parser Char
fill = anyChar

align :: Parser AnyAlign
align =
  choice
    [ AnyAlign AlignLeft <$ char '<',
      AnyAlign AlignRight <$ char '>',
      AnyAlign AlignCenter <$ char '^',
      AnyAlign AlignInside <$ char '='
    ]

sign :: Parser SignMode
sign =
  choice
    [ Plus <$ char '+',
      Minus <$ char '-',
      Space <$ char ' '
    ]

width :: Parser Integer
width = integer

integer :: Parser Integer
integer = read <$> some (oneOf ['0' .. '9']) -- incomplete: see: https://docs.python.org/3/reference/lexical_analysis.html#grammar-token-integer

groupingOption :: Parser Char
groupingOption = oneOf ("_," :: String)

precision :: Parser Integer
precision = integer

type_ :: Parser TypeFlag
type_ =
  choice
    [ Flagb <$ char 'b',
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
