{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module PyF.Internal.PythonSyntax where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Data.Void (Void)

import qualified Data.Char

import Data.Maybe (fromMaybe)

import qualified Data.Set as Set -- For fancyFailure

type Parser t = Parsec Void String t

{-
-- TODO:
- Better parsing of integer
- Recursive replacement field, so "{string:.{precision}} can be parsed
- f_expression / conversion
- Not (Yet) implemented:
     - fields: grouping_option / 0
     - types: n
     - alignement: =
     - #: for floating points
     - floating point rendering of NaN and Inf are not well defined...
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
rawString = Raw . escapeChars <$> some (noneOf ("{}" :: [Char]))

escapedParenthesis :: Parser Item
escapedParenthesis = Raw <$> (string "{{" <|> string "}}")

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
  expr <- many (noneOf ("}:" :: [Char]))
  fmt <- optional $ do
    _ <- char ':'
    format_spec
  _ <- char '}'

  pure (Replacement expr fmt)

pattern DefaultFormatMode :: FormatMode
pattern DefaultFormatMode = FormatMode PaddingDefault (DefaultF PrecisionDefault Negative)

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
    DefaultF Precision SignField -- Default
  | BinaryF AlternateForm SignField -- Binary
  | CharacterF -- Character
  | DecimalF SignField -- Decimal
  | ExponentialF Precision {- AlternateForm -} SignField -- exponential notation (Alt not handled)
  | ExponentialCapsF Precision {- AlternateForm -} SignField -- exponentiel notation CAPS (Alt not handled)
  | FixedF Precision {- AlternateForm -} SignField -- fixed point (Alt not handled)
  | FixedCapsF Precision {- AlternateForm -} SignField -- fixed point CAPS (Alt not handled)
  | GeneralF Precision {- AlternateForm -} SignField -- General (Alt Not Yet handled)
  | GeneralCapsF Precision {- AlternateForm -} SignField -- General Switch to E (Alt Not yet handled)
  -- | NumberF Precision AlternateForm SignField -- Number (Not Yet handled)
  | OctalF AlternateForm SignField -- octal
  | StringF Precision -- string
  | HexF AlternateForm SignField -- small hex
  | HexCapsF AlternateForm SignField -- big hex
  | PercentF Precision {- AlternateForm -} SignField -- percent (Alt not handled)
  deriving (Show)

data AlternateForm = AlternateForm | NormalForm
  deriving (Show)

data SignField = Positive | Negative | Space
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

  s <- optional sign
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
    Nothing -> pure (FormatMode padding (DefaultF prec (fromMaybe Negative s)))
    Just flag -> case evalFlag flag prec alternateForm s of
      Right fmt -> pure (FormatMode padding fmt)
      Left typeError -> do
        lastCharFailed typeError

evalFlag :: TypeFlag -> Precision -> AlternateForm -> Maybe SignField -> Either String TypeFormat
evalFlag Flagb prec alt s = failIfPrec prec (BinaryF alt (defSign s))
evalFlag Flagc prec alt s = failIfS s =<< failIfPrec prec =<< failIfAlt alt CharacterF
evalFlag Flagd prec alt s = failIfPrec prec =<< failIfAlt alt (DecimalF (defSign s))
evalFlag Flage prec alt s = unhandledAlt alt (ExponentialF prec (defSign s))
evalFlag FlagE prec alt s = unhandledAlt alt (ExponentialCapsF prec (defSign s))
evalFlag Flagf prec alt s = unhandledAlt alt (FixedF prec (defSign s))
evalFlag FlagF prec alt s = unhandledAlt alt (FixedCapsF prec (defSign s))
evalFlag Flagg prec alt s = unhandledAlt alt (GeneralF prec (defSign s))
evalFlag FlagG prec alt s = unhandledAlt alt (GeneralCapsF prec (defSign s))
evalFlag Flagn _prec _alt _s = Left ("Type 'n' not handled (yet). " ++ errgGn)
evalFlag Flago prec alt s = failIfPrec prec $ OctalF alt (defSign s)
evalFlag Flags prec alt s = failIfS s =<< (failIfAlt alt $ StringF prec)
evalFlag Flagx prec alt s = failIfPrec prec $ HexF alt (defSign s)
evalFlag FlagX prec alt s = failIfPrec prec $ HexCapsF alt (defSign s)
evalFlag FlagPercent prec alt s = unhandledAlt alt (PercentF prec (defSign s))

defSign :: Maybe SignField -> SignField
defSign Nothing = Negative
defSign (Just s) = s


errgGn :: String
errgGn = "Use one of {'b', 'c', 'd', 'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 's', 'x', 'X', '%'}."

failIfPrec :: Precision -> TypeFormat -> Either String TypeFormat
failIfPrec PrecisionDefault i = Right i
failIfPrec (Precision i) _ = Left ("Type incompatible with precision (." ++ show i ++ "), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 's', '%'} or remove the precision field.")

failIfAlt :: AlternateForm -> TypeFormat -> Either String TypeFormat
failIfAlt NormalForm i = Right i
failIfAlt _ _ = Left "Type incompatible with alternative form (#), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the alternative field."

failIfS :: Maybe SignField -> TypeFormat -> Either String TypeFormat
failIfS Nothing i = Right i
failIfS (Just s) _ = Left ("Type incompatible with sign field (" ++ [toSignField s] ++ "), use any of {'b', 'd', 'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the sign field.")

toSignField :: SignField -> Char
toSignField Positive = '+'
toSignField Negative = '-'
toSignField Space = ' '


unhandledAlt :: AlternateForm -> TypeFormat -> Either String TypeFormat
unhandledAlt NormalForm i = Right i
unhandledAlt _ _ = Left "Type not yet compatible with alternative form (#), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 'o', 'x', 'X', '%'} or remove the alternative field."

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
  Right AlignRight <$ char '<',
  Right AlignLeft <$ char '>',
  Right AlignCenter <$ char '^',
  Left "Align mode '=' is not handled yet" <$ char '='
  ]

sign :: Parser SignField
sign = choice
  [Positive <$ char '+',
   Negative <$ char '-',
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
  Flagn <$ char 'n',
  Flago <$ char 'o',
  Flags <$ char 's',
  Flagx <$ char 'x',
  FlagX <$ char 'X',
  FlagPercent <$ char '%'
  ]

