{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

{- | This module uses the python mini language detailed in
'PyF.Internal.PythonSyntax' to build an template haskell expression
representing a formatted string.

-}
module PyF.Internal.QQ (
  toExp,
  toExpPython)
where

import Text.Megaparsec

import           Language.Haskell.TH

import Data.Maybe (fromMaybe)

import qualified Data.Maybe

import PyF.Internal.PythonSyntax
import PyF.Internal.Extensions

import qualified PyF.Formatters as Formatters
import PyF.Formatters (AnyAlign(..))
import Data.Proxy
import GHC.TypeLits
import Data.String
import PyF.Class

-- Be Careful: empty format string
-- | Parse a string and return a formatter for it
toExp:: (Char, Char) -> String -> Q Exp
toExp delimiters s = do
  filename <- loc_filename <$> location

  exts <- Data.Maybe.mapMaybe thExtToMetaExt <$> extsEnabled
  wrapString <- wrapOverloadedStrings

  case parse (parseGenericFormatString exts delimiters) filename s of
    Left err -> do
      err' <- overrideErrorForFile filename err
      fail (errorBundlePretty err')
    Right items -> goFormat wrapString items

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

  let
    -- drop the first lines of the file up to the line containing the quasiquote
    -- then, split in what is before the QQ and what is after.
    -- e.g.  blablabla [f|hello|] will split to
    -- "blablabla [f|" and "hello|]"
    (prefix, postfix) = splitAt (col - 1) $ unlines $ drop (line - 1) (lines fileContent)


  pure $ err {
    bundlePosState = (bundlePosState err) {
        pstateInput = postfix,
        pstateSourcePos = SourcePos filename (mkPos line) (mkPos col),
        pstateOffset = 0,
        pstateLinePrefix = prefix
        }}

toExpPython :: String -> Q Exp
toExpPython = toExp ('{', '}')

-- | Detect if overloaded string is enabled
--   And returns a wrapping function which wraps literal strings inside
--   'fromString' if @OverloadedStrings@ is not enabled.
wrapOverloadedStrings :: Q (String -> Q Exp)
wrapOverloadedStrings = do
  thExtsEnabled <- extsEnabled
  if OverloadedStrings `elem` thExtsEnabled
    -- NOTE: we cannot use [| s |] because empty string is lifted as
    -- [], which breaks OverloadedStrings
    then pure $ \s -> pure $ LitE (StringL s)
    else pure $ \s -> [| fromString s |]

goFormat :: (String -> Q Exp) -> [Item] -> Q Exp
goFormat wrapString [] = wrapString ""
goFormat wrapString items = foldl1 fofo <$> (mapM (toFormat wrapString) items)

fofo :: Exp -> Exp -> Exp
fofo s0 s1 = InfixE (Just s0) (VarE '(<>)) (Just s1)

-- Real formatting is here

toFormat :: (String -> Q Exp) -> Item -> Q Exp
toFormat wrapString (Raw x) = wrapString x
toFormat _ (Replacement expr y) = do
  formatExpr <- padAndFormat (fromMaybe DefaultFormatMode y)
  pure (formatExpr `AppE` expr)

changePrec :: Precision -> Q Exp
changePrec PrecisionDefault = [| Just 6 |]
changePrec (Precision n) = [| Just n |]

changePrec' :: Precision ->  Q Exp
changePrec' PrecisionDefault = [| Nothing |]
changePrec' (Precision n) = [| Just n |]

toGrp :: Maybe Char -> Int -> Q Exp
toGrp mb a = [| grp |]
  where grp = (a,) <$> mb

withAlt :: AlternateForm -> Formatters.Format t t' t'' -> Q Exp
withAlt NormalForm e = [| e |]
withAlt AlternateForm e = [| Formatters.Alternate e |]

padAndFormat :: FormatMode -> Q Exp
padAndFormat (FormatMode padding tf grouping) = case tf of
  -- Integrals
  BinaryF alt s -> [| formatAnyIntegral $(withAlt alt Formatters.Binary) s $(newPaddingQ padding) $(toGrp grouping 4) |]
  CharacterF -> [| formatAnyIntegral Formatters.Character Formatters.Minus $(newPaddingQ padding) Nothing |]
  DecimalF s -> [| formatAnyIntegral Formatters.Decimal s $(newPaddingQ padding) $(toGrp grouping 3) |]
  HexF alt s -> [| formatAnyIntegral $(withAlt alt Formatters.Hexa) s $(newPaddingQ padding) $(toGrp grouping 4) |]
  OctalF alt s -> [| formatAnyIntegral $(withAlt alt Formatters.Octal) s $(newPaddingQ padding) $(toGrp grouping 4) |]
  HexCapsF alt s -> [| formatAnyIntegral (Formatters.Upper $(withAlt alt Formatters.Hexa)) s $(newPaddingQ padding) $(toGrp grouping 4) |]

  -- Floating
  ExponentialF prec alt s -> [| formatAnyFractional $(withAlt alt Formatters.Exponent) s $(newPaddingQ padding) $(toGrp grouping 3) $(changePrec prec) |]
  ExponentialCapsF prec alt s -> [| formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Exponent)) s $(newPaddingQ padding) $(toGrp grouping 3) $(changePrec prec) |]
  GeneralF prec alt s -> [| formatAnyFractional $(withAlt alt Formatters.Generic) s $(newPaddingQ padding) $(toGrp grouping 3) $(changePrec prec) |]
  GeneralCapsF prec alt s -> [| formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Generic)) s $(newPaddingQ padding) $(toGrp grouping 3) $(changePrec prec) |]
  FixedF prec alt s -> [| formatAnyFractional $(withAlt alt Formatters.Fixed) s $(newPaddingQ padding) $(toGrp grouping 3) $(changePrec prec) |]
  FixedCapsF prec alt s -> [| formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Fixed)) s $(newPaddingQ padding) $(toGrp grouping 3) $(changePrec prec) |]
  PercentF prec alt s -> [| formatAnyFractional $(withAlt alt Formatters.Percent) s $(newPaddingQ padding) $(toGrp grouping 3) $(changePrec prec) |]

  -- Default / String
  DefaultF prec s -> [| formatAny s $(paddingToPaddingK padding) $(toGrp grouping 3) $(changePrec' prec) |]
  StringF prec -> [| Formatters.formatString (newPaddingKForString $(paddingToPaddingK padding)) $(changePrec' prec) . pyfToString |]

newPaddingQ :: Padding -> Q Exp
newPaddingQ pad = [| pad' |]
  where pad' = newPaddingUnQ pad

newPaddingUnQ :: Padding -> Maybe (Integer, AnyAlign, Char)
newPaddingUnQ padding = case padding of
    PaddingDefault -> Nothing
    (Padding i al) -> case al of
      Nothing -> Just (i, AnyAlign Formatters.AlignRight, ' ') -- Right align and space is default for any object, except string
      Just (Nothing, a) -> Just (i, a, ' ')
      Just (Just c, a) -> Just (i, a, c)

data PaddingK k where
  PaddingDefaultK :: PaddingK 'Formatters.AlignAll
  PaddingK :: Integer -> (Maybe (Maybe Char, Formatters.AlignMode k)) -> PaddingK k

paddingToPaddingK :: Padding -> Q Exp
paddingToPaddingK p = case p of
  PaddingDefault -> [| PaddingDefaultK |]
  Padding i Nothing -> [| PaddingK i Nothing :: PaddingK 'Formatters.AlignAll |]
  Padding i (Just (c, AnyAlign a)) -> [| PaddingK i (Just (c, a)) |]

paddingKToPadding :: PaddingK k -> Padding
paddingKToPadding p = case p of
  PaddingDefaultK -> PaddingDefault
  PaddingK i Nothing -> Padding i Nothing
  PaddingK i (Just (c, a)) -> Padding i (Just (c, AnyAlign a))

formatAnyIntegral :: (Show i, Integral i, IsString s) => Formatters.Format t t' 'Formatters.Integral -> Formatters.SignMode -> Maybe (Integer, AnyAlign, Char) -> Maybe (Int, Char) -> i -> s
formatAnyIntegral f s Nothing grouping i = fromString $ Formatters.formatIntegral f s Nothing grouping i
formatAnyIntegral f s (Just (padSize, AnyAlign alignMode, c)) grouping i = fromString $ Formatters.formatIntegral f s (Just (fromIntegral padSize, alignMode, c)) grouping i

formatAnyFractional :: (RealFloat i, IsString s) => Formatters.Format t t' 'Formatters.Fractional -> Formatters.SignMode -> Maybe (Integer, AnyAlign, Char) -> Maybe (Int, Char) -> Maybe Int -> i -> s
formatAnyFractional f s Nothing grouping p i = fromString $ Formatters.formatFractional f s Nothing grouping p i
formatAnyFractional f s (Just (padSize, AnyAlign alignMode, c)) grouping p i = fromString $ Formatters.formatFractional f s (Just (fromIntegral padSize, alignMode, c)) grouping p i

class FormatAny i k where
  formatAny :: IsString s => Formatters.SignMode -> PaddingK k -> Maybe (Int, Char) -> Maybe Int -> i -> s

instance (FormatAny2 (PyFClassify t) t k) => FormatAny t k where
  formatAny = formatAny2 (Proxy :: Proxy (PyFClassify t))

class FormatAny2 (c :: PyFCategory) (i :: *) (k :: Formatters.AlignForString) where
  formatAny2 :: IsString s => Proxy c -> Formatters.SignMode -> PaddingK k -> Maybe (Int, Char) -> Maybe Int -> i -> s

instance (Show t, Integral t) => FormatAny2 'PyFIntegral t k where
  formatAny2 _ s a p _precision i = formatAnyIntegral Formatters.Decimal s (newPaddingUnQ (paddingKToPadding a)) p i

instance (RealFloat t) => FormatAny2 'PyFFractional t k where
  formatAny2 _ s a p precision t = formatAnyFractional Formatters.Generic s (newPaddingUnQ (paddingKToPadding a)) p precision t

newPaddingKForString :: PaddingK 'Formatters.AlignAll -> Maybe (Int, Formatters.AlignMode 'Formatters.AlignAll, Char)
newPaddingKForString padding = case padding of
    PaddingDefaultK -> Nothing
    PaddingK i Nothing -> Just (fromIntegral i, Formatters.AlignLeft, ' ') -- default align left and fill with space for string
    PaddingK i (Just (mc, a)) -> Just (fromIntegral i, a, fromMaybe ' ' mc)


-- TODO: _s(ign) and _grouping should trigger errors
instance (PyFToString t) => FormatAny2 'PyFString t 'Formatters.AlignAll where
  formatAny2 _ _s a _grouping precision t = fromString $ Formatters.formatString (newPaddingKForString a) precision (pyfToString t)

instance TypeError ('Text "String type is incompatible with inside padding (=).") => FormatAny2 'PyFString t 'Formatters.AlignNumber where
  formatAny2 = error "Unreachable"

type family ToFmt t where
  ToFmt 'PyFIntegral = 'Formatters.Integral
  ToFmt 'PyFFractional = 'Formatters.Fractional
