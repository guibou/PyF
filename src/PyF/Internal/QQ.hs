{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module uses the python mini language detailed in
-- 'PyF.Internal.PythonSyntax' to build an template haskell expression
-- representing a formatted string.
module PyF.Internal.QQ
  ( toExp,
    toExpPython,
  )
where

import Control.Monad.Reader
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.String (fromString)
import GHC.TypeLits
import Language.Haskell.TH hiding (Type)
import PyF.Class
import PyF.Formatters (AnyAlign (..))
import qualified PyF.Formatters as Formatters
import PyF.Internal.PythonSyntax
import Text.Megaparsec

-- Be Careful: empty format string

-- | Parse a string and return a formatter for it
toExp :: (Char, Char) -> String -> Q Exp
toExp expressionDelimiters s = do
  filename <- loc_filename <$> location
  exts <- extsEnabled
  let wrapFromString e =
        if OverloadedStrings `elem` exts
          then [|fromString $(e)|]
          else e
  let context = ParsingContext expressionDelimiters exts
  case runReader (runParserT parseGenericFormatString filename s) context of
    Left err -> do
      err' <- overrideErrorForFile filename err
      fail (errorBundlePretty err')
    Right items -> wrapFromString (goFormat items)

-- Megaparsec displays error relative to what they parsed
-- However the formatting string is part of a more complex file and we
-- want error reporting relative to that file
overrideErrorForFile :: FilePath -> ParseErrorBundle String e -> Q (ParseErrorBundle String e)
-- We have no may to recover interactive content
-- So we won't do better than displaying the megaparsec
-- error relative to the quasi quote content
overrideErrorForFile "<interactive>" err = pure err
-- We know the content of the file here
overrideErrorForFile filename err = do
  (line, col) <- loc_start <$> location
  fileContent <- runIO (readFile filename)
  let -- drop the first lines of the file up to the line containing the quasiquote
      -- then, split in what is before the QQ and what is after.
      -- e.g.  blablabla [fmt|hello|] will split to
      -- "blablabla [fmt|" and "hello|]"
      (prefix, postfix) = splitAt (col - 1) $ unlines $ drop (line - 1) (lines fileContent)
  pure $
    err
      { bundlePosState =
          (bundlePosState err)
            { pstateInput = postfix,
              pstateSourcePos = SourcePos filename (mkPos line) (mkPos col),
              pstateOffset = 0,
              pstateLinePrefix = prefix
            }
      }

toExpPython :: String -> Q Exp
toExpPython = toExp ('{', '}')

{-
Note: Empty String Lifting

Empty string are lifted as [] instead of "", so I'm using LitE (String L) instead
-}

goFormat :: [Item] -> Q Exp
-- We special case on empty list in order to generate an empty string
goFormat [] = pure $ LitE (StringL "") -- see [Empty String Lifting]
goFormat items = foldl1 sappendQ <$> mapM toFormat items

-- | call `<>` between two 'Exp'
sappendQ :: Exp -> Exp -> Exp
sappendQ s0 s1 = InfixE (Just s0) (VarE '(<>)) (Just s1)

-- Real formatting is here

toFormat :: Item -> Q Exp
toFormat (Raw x) = pure $ LitE (StringL x) -- see [Empty String Lifting]
toFormat (Replacement expr y) = do
  formatExpr <- padAndFormat (fromMaybe DefaultFormatMode y)
  pure (formatExpr `AppE` expr)

-- | Default precision for floating point
defaultFloatPrecision :: Maybe Int
defaultFloatPrecision = Just 6

-- | Precision to maybe
splicePrecision :: Maybe Int -> Precision -> Q Exp
splicePrecision def PrecisionDefault = [|def|]
splicePrecision _ (Precision p) = case p of
  Value n -> [|Just n|]
  HaskellExpr e -> [|Just $(pure e)|]

toGrp :: Maybe Char -> Int -> Q Exp
toGrp mb a = [|grp|]
  where
    grp = (a,) <$> mb

withAlt :: AlternateForm -> Formatters.Format t t' t'' -> Q Exp
withAlt NormalForm e = [|e|]
withAlt AlternateForm e = [|Formatters.Alternate e|]

padAndFormat :: FormatMode -> Q Exp
padAndFormat (FormatMode padding tf grouping) = case tf of
  -- Integrals
  BinaryF alt s -> [|formatAnyIntegral $(withAlt alt Formatters.Binary) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  CharacterF -> [|formatAnyIntegral Formatters.Character Formatters.Minus $(newPaddingQ padding) Nothing|]
  DecimalF s -> [|formatAnyIntegral Formatters.Decimal s $(newPaddingQ padding) $(toGrp grouping 3)|]
  HexF alt s -> [|formatAnyIntegral $(withAlt alt Formatters.Hexa) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  OctalF alt s -> [|formatAnyIntegral $(withAlt alt Formatters.Octal) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  HexCapsF alt s -> [|formatAnyIntegral (Formatters.Upper $(withAlt alt Formatters.Hexa)) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  -- Floating
  ExponentialF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Exponent) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  ExponentialCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Exponent)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  GeneralF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Generic) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  GeneralCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Generic)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  FixedF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Fixed) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  FixedCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Fixed)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  PercentF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Percent) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  -- Default / String
  DefaultF prec s -> [|formatAny s $(paddingToPaddingK padding) $(toGrp grouping 3) $(splicePrecision Nothing prec)|]
  StringF prec -> [|Formatters.formatString (newPaddingKForString $(paddingToPaddingK padding)) $(splicePrecision Nothing prec) . pyfToString|]

newPaddingQ :: Padding -> Q Exp
newPaddingQ pad = [|pad'|]
  where
    pad' = newPaddingUnQ pad

newPaddingUnQ :: Padding -> Maybe (Integer, AnyAlign, Char)
newPaddingUnQ padding = case padding of
  PaddingDefault -> Nothing
  (Padding i al) -> case al of
    Nothing -> Just (i, AnyAlign Formatters.AlignRight, ' ') -- Right align and space is default for any object, except string
    Just (Nothing, a) -> Just (i, a, ' ')
    Just (Just c, a) -> Just (i, a, c)

data PaddingK k where
  PaddingDefaultK :: PaddingK 'Formatters.AlignAll
  PaddingK :: Integer -> Maybe (Maybe Char, Formatters.AlignMode k) -> PaddingK k

paddingToPaddingK :: Padding -> Q Exp
paddingToPaddingK p = case p of
  PaddingDefault -> [|PaddingDefaultK|]
  Padding i Nothing -> [|PaddingK i Nothing :: PaddingK 'Formatters.AlignAll|]
  Padding i (Just (c, AnyAlign a)) -> [|PaddingK i (Just (c, a))|]

paddingKToPadding :: PaddingK k -> Padding
paddingKToPadding p = case p of
  PaddingDefaultK -> PaddingDefault
  PaddingK i Nothing -> Padding i Nothing
  PaddingK i (Just (c, a)) -> Padding i (Just (c, AnyAlign a))

formatAnyIntegral :: (Show i, Integral i) => Formatters.Format t t' 'Formatters.Integral -> Formatters.SignMode -> Maybe (Integer, AnyAlign, Char) -> Maybe (Int, Char) -> i -> String
formatAnyIntegral f s Nothing grouping i = Formatters.formatIntegral f s Nothing grouping i
formatAnyIntegral f s (Just (padSize, AnyAlign alignMode, c)) grouping i = Formatters.formatIntegral f s (Just (fromIntegral padSize, alignMode, c)) grouping i

formatAnyFractional :: (RealFloat i) => Formatters.Format t t' 'Formatters.Fractional -> Formatters.SignMode -> Maybe (Integer, AnyAlign, Char) -> Maybe (Int, Char) -> Maybe Int -> i -> String
formatAnyFractional f s Nothing grouping p i = Formatters.formatFractional f s Nothing grouping p i
formatAnyFractional f s (Just (padSize, AnyAlign alignMode, c)) grouping p i = Formatters.formatFractional f s (Just (fromIntegral padSize, alignMode, c)) grouping p i

class FormatAny i k where
  formatAny :: Formatters.SignMode -> PaddingK k -> Maybe (Int, Char) -> Maybe Int -> i -> String

instance (FormatAny2 (PyFClassify t) t k) => FormatAny t k where
  formatAny = formatAny2 (Proxy :: Proxy (PyFClassify t))

class FormatAny2 (c :: PyFCategory) (i :: Type) (k :: Formatters.AlignForString) where
  formatAny2 :: Proxy c -> Formatters.SignMode -> PaddingK k -> Maybe (Int, Char) -> Maybe Int -> i -> String

instance (Show t, Integral t) => FormatAny2 'PyFIntegral t k where
  formatAny2 _ s a p _precision = formatAnyIntegral Formatters.Decimal s (newPaddingUnQ (paddingKToPadding a)) p

instance (RealFloat t) => FormatAny2 'PyFFractional t k where
  formatAny2 _ s a = formatAnyFractional Formatters.Generic s (newPaddingUnQ (paddingKToPadding a))

newPaddingKForString :: PaddingK 'Formatters.AlignAll -> Maybe (Int, Formatters.AlignMode 'Formatters.AlignAll, Char)
newPaddingKForString padding = case padding of
  PaddingDefaultK -> Nothing
  PaddingK i Nothing -> Just (fromIntegral i, Formatters.AlignLeft, ' ') -- default align left and fill with space for string
  PaddingK i (Just (mc, a)) -> Just (fromIntegral i, a, fromMaybe ' ' mc)

-- TODO: _s(ign) and _grouping should trigger errors
instance (PyFToString t) => FormatAny2 'PyFString t 'Formatters.AlignAll where
  formatAny2 _ _s a _grouping precision t = Formatters.formatString (newPaddingKForString a) precision (pyfToString t)

instance TypeError ('Text "String type is incompatible with inside padding (=).") => FormatAny2 'PyFString t 'Formatters.AlignNumber where
  formatAny2 = error "Unreachable"
