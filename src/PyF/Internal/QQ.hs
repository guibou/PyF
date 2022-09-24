{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | This module uses the python mini language detailed in
-- 'PyF.Internal.PythonSyntax' to build an template haskell expression
-- representing a formatted string.
module PyF.Internal.QQ
  ( toExp,
    Config (..),
    wrapFromString,
    expQQ,
  )
where

import Control.Monad.Reader
import Data.Data (Data (gmapQ), Typeable, cast)
import Data.Kind
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy
import Data.String (fromString)
import GHC (GenLocated (L), moduleNameString)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Tc.Utils.Monad (addErrAt)
import GHC.Tc.Types (TcM)
import GHC.Types.Name (occNameString)
#else
import OccName
import TcRnTypes (TcM)
import TcSplice (lookupThName_maybe)
import TcRnMonad (addErrAt)
#endif

#if MIN_VERSION_ghc(9,3,0)
import GHC.Tc.Errors.Types
import GHC.Types.Error
import GHC.Driver.Errors.Types
import GHC.Parser.Errors.Types
import GHC.Utils.Outputable (text)
#endif



#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Name.Reader
#else
import FastString
import RdrName
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Expr as Expr
import GHC.Hs.Extension as Ext
import GHC.Hs.Pat as Pat
#else
import HsExpr as Expr
import HsExtension as Ext
import HsPat as Pat
import HsLit
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc
#else
import SrcLoc
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs
#else
import HsSyn
#endif

import GHC.TypeLits
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Q (Q))
import PyF.Class
import PyF.Formatters (AnyAlign (..))
import qualified PyF.Formatters as Formatters
import PyF.Internal.Meta (toName)
import PyF.Internal.PythonSyntax
import Text.Parsec
import Text.Parsec.Error
  ( errorMessages,
    messageString,
    newErrorMessage,
    setErrorPos,
    showErrorMessages,
  )
import Text.Parsec.Pos (newPos, initialPos)
import Text.ParserCombinators.Parsec.Error (Message (..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (isJust)

-- | Configuration for the quasiquoter
data Config = Config
  { -- | What are the delimiters for interpolation. 'Nothing' means no
    -- interpolation / formatting.
    delimiters :: Maybe (Char, Char),
    -- | Post processing. The input 'Exp' represents a 'String'. Common use
    -- case includes using 'wrapFromString' to add 'fromString' in the context
    -- of 'OverloadedStrings'.
    postProcess :: Q Exp -> Q Exp
  }

-- | Build a quasiquoter for expression
expQQ :: String -> (String -> Q Exp) -> QuasiQuoter
expQQ fName qExp =
  QuasiQuoter
    { quoteExp = qExp,
      quotePat = err "pattern",
      quoteType = err "type",
      quoteDec = err "declaration"
    }
  where
    err :: String -> t
    err name = error (fName ++ ": This QuasiQuoter can not be used as a " ++ name ++ "!")

-- | If 'OverloadedStrings' is enabled, from the input expression with
-- 'fromString'.
wrapFromString :: ExpQ -> Q Exp
wrapFromString e = do
  exts <- extsEnabled
  if OverloadedStrings `elem` exts
    then [|fromString $(e)|]
    else e

-- | Parse a string and return a formatter for it
toExp :: Config -> String -> Q Exp
toExp Config {delimiters = expressionDelimiters, postProcess} s = do
  loc <- location
  exts <- extsEnabled
  let context = ParsingContext expressionDelimiters exts

  -- Setup the parser so it matchs the real original position in the source
  -- code.
  let filename = loc_filename loc
  let initPos = setSourceColumn (setSourceLine (initialPos filename) (fst $ loc_start loc)) (snd $ loc_start loc)
  case runReader (runParserT (setPosition initPos >> parseGenericFormatString) () filename s) context of
    Left err -> do
      reportParserErrorAt err
      -- returns a dummy exp, so TH continues its life. This TH code won't be
      -- executed anyway, there is an error
      [|()|]
    Right items -> do
      checkResult <- checkVariables items
      case checkResult of
        Nothing -> postProcess (goFormat items)
        Just (srcSpan, msg) -> do
          reportErrorAt srcSpan msg
          [|()|]

findFreeVariablesInFormatMode :: Maybe FormatMode -> [(SrcSpan, RdrName)]
findFreeVariablesInFormatMode Nothing = []
findFreeVariablesInFormatMode (Just (FormatMode padding tf _ )) = findFreeVariables tf <> case padding of
  PaddingDefault -> []
  Padding eoi _ -> findFreeVariables eoi

checkOneItem :: Item -> Q (Maybe (SrcSpan, String))
checkOneItem (Raw _) = pure Nothing
checkOneItem (Replacement (hsExpr, _) formatMode) = do
  let allNames = findFreeVariables hsExpr <> findFreeVariablesInFormatMode formatMode
  res <- mapM doesExists allNames
  let resFinal = catMaybes res

  case resFinal of
    [] -> pure Nothing
    ((err, span) : _) -> pure $ Just (span, err)


findFreeVariables :: Data a => a -> [(SrcSpan, RdrName)]
findFreeVariables item = allNames
  where
    -- Find all free Variables in an HsExpr
    f :: forall a. (Data a, Typeable a) => a -> [Located RdrName]
    f e = case cast @_ @(HsExpr GhcPs) e of
#if MIN_VERSION_ghc(9,2,0)
      Just (HsVar _ l@(L a b)) -> [L (locA a) (unLoc l)]
#else
      Just (HsVar _ l) -> [l]
#endif
      Just (HsLam _ (MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc -> ps) (GRHSs _ [unLoc -> GRHS _ _ (unLoc -> e)] _)])) _)) -> filter keepVar subVars
        where
          keepVar (L _ n) = n `notElem` subPats
          subVars = concat $ gmapQ f [e]
          subPats = concat $ gmapQ findPats ps
      _ -> concat $ gmapQ f e

    -- Find all Variables bindings (i.e. patterns) in an HsExpr
    findPats :: forall a. (Data a, Typeable a) => a -> [RdrName]
    findPats p = case cast @_ @(Pat.Pat GhcPs) p of
      Just (VarPat _ (unLoc -> name)) -> [name]
      _ -> concat $ gmapQ findPats p
    -- Be careful, we wrap hsExpr in a list, so the toplevel hsExpr will be
    -- seen by gmapQ. Otherwise it will miss variables if they are the top
    -- level expression: gmapQ only checks sub constructors.
    allVars = concat $ gmapQ f [item]
    allNames = map (\(L l e) -> (l, e)) allVars

lookupName :: RdrName -> Q Bool
lookupName n = case n of
  (Unqual o) -> isJust <$> lookupValueName (occNameString o)
  (Qual m o) -> isJust <$> lookupValueName (moduleNameString m <> "." <> occNameString o)
  -- No idea how to lookup for theses names, so consider that they exists
  (Orig _m _o) -> pure True
  (Exact _) -> pure True

doesExists :: (b, RdrName) -> Q (Maybe (String, b))
doesExists (loc, name) = do
  res <- lookupName name
  case res of
    False -> pure (Just ("Variable not in scope: " <> show (toName name), loc))
    True -> pure Nothing

-- | Check that all variables used in 'Item' exists, otherwise, fail.
checkVariables :: [Item] -> Q (Maybe (SrcSpan, String))
checkVariables [] = pure Nothing
checkVariables (x : xs) = do
  r <- checkOneItem x
  case r of
    Nothing -> checkVariables xs
    Just err -> pure $ Just err

-- Stolen from: https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
-- This allows to hack inside the the GHC api and use function not exported by template haskell.
-- This may not be always safe, see https://github.com/guibou/PyF/issues/115,
-- hence keep that for "failing path" (i.e. error reporting), but not on
-- codepath which are executed otherwise.
unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = Q (unsafeCoerce m)

-- | This function is similar to TH reportError, however it also provide
-- correct SrcSpan, so error are localised at the correct position in the TH
-- splice instead of being at the beginning.
reportErrorAt :: SrcSpan -> String -> Q ()
reportErrorAt loc msg = unsafeRunTcM $ addErrAt loc msg'
  where
#if MIN_VERSION_ghc(9,3,0)
    msg' = TcRnUnknownMessage (GhcPsMessage $ PsUnknownMessage $ mkPlainError noHints $
                         text msg)
#else
    msg' = fromString msg
#endif

reportParserErrorAt :: ParseError -> Q ()
reportParserErrorAt err = reportErrorAt span msg
  where
    msg = intercalate "\n" $ formatErrorMessages err

    span :: SrcSpan
    span = mkSrcSpan loc loc'

    loc = srcLocFromParserError (errorPos err)
    loc' = srcLocFromParserError (incSourceColumn (errorPos err) 1)

srcLocFromParserError :: SourcePos -> SrcLoc
srcLocFromParserError sourceLoc = srcLoc
  where
    line = sourceLine sourceLoc
    column = sourceColumn sourceLoc
    name = sourceName sourceLoc

    srcLoc = mkSrcLoc (fromString name) line column

formatErrorMessages :: ParseError -> [String]
formatErrorMessages err
  -- If there is an explicit error message from parsec, use only that
  | not $ null messages = map messageString messages
  -- Otherwise, uses parsec formatting
  | otherwise = [showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)]
  where
    (_sysUnExpect, msgs1) = span (SysUnExpect "" ==) (errorMessages err)
    (_unExpect, msgs2) = span (UnExpect "" ==) msgs1
    (_expect, messages) = span (Expect "" ==) msgs2
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
toFormat (Replacement ( _, expr) y) = do
  formatExpr <- padAndFormat (fromMaybe DefaultFormatMode y)
  pure (formatExpr `AppE` expr)

-- | Default precision for floating point
defaultFloatPrecision :: Maybe Int
defaultFloatPrecision = Just 6

-- | Precision to maybe
splicePrecision :: Maybe Int -> Precision -> Q Exp
splicePrecision def PrecisionDefault = [|def :: Maybe Int|]
splicePrecision _ (Precision p) = [|Just $(exprToInt p)|]

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
newPaddingQ padding = case padding of
  PaddingDefault -> [|Nothing :: Maybe (Int, AnyAlign, Char)|]
  (Padding i al) -> case al of
    Nothing -> [|Just ($(exprToInt i), AnyAlign Formatters.AlignRight, ' ')|] -- Right align and space is default for any object, except string
    Just (Nothing, a) -> [|Just ($(exprToInt i), a, ' ')|]
    Just (Just c, a) -> [|Just ($(exprToInt i), a, c)|]

exprToInt :: ExprOrValue Int -> Q Exp
-- Note: this is a literal provided integral. We use explicit case to ::Int so it won't warn about defaulting
exprToInt (Value i) = [|$(pure $ LitE (IntegerL (fromIntegral i))) :: Int|]
exprToInt (HaskellExpr (_, e)) = [|$(pure e)|]

data PaddingK k i where
  PaddingDefaultK :: PaddingK 'Formatters.AlignAll Int
  PaddingK :: i -> Maybe (Maybe Char, Formatters.AlignMode k) -> PaddingK k i

paddingToPaddingK :: Padding -> Q Exp
paddingToPaddingK p = case p of
  PaddingDefault -> [|PaddingDefaultK|]
  Padding i Nothing -> [|PaddingK ($(exprToInt i)) Nothing :: PaddingK 'Formatters.AlignAll Int|]
  Padding i (Just (c, AnyAlign a)) -> [|PaddingK $(exprToInt i) (Just (c, a))|]

paddingKToPadding :: PaddingK k i -> Maybe (i, AnyAlign, Char)
paddingKToPadding p = case p of
  PaddingDefaultK -> Nothing
  (PaddingK i al) -> case al of
    Nothing -> Just (i, AnyAlign Formatters.AlignRight, ' ') -- Right align and space is default for any object, except string
    Just (Nothing, a) -> Just (i, AnyAlign a, ' ')
    Just (Just c, a) -> Just (i, AnyAlign a, c)

formatAnyIntegral :: forall i paddingWidth t t'. Integral paddingWidth => PyfFormatIntegral i => Formatters.Format t t' 'Formatters.Integral -> Formatters.SignMode -> Maybe (paddingWidth, AnyAlign, Char) -> Maybe (Int, Char) -> i -> String
formatAnyIntegral f s Nothing grouping i = pyfFormatIntegral @i @paddingWidth f s Nothing grouping i
formatAnyIntegral f s (Just (padSize, AnyAlign alignMode, c)) grouping i = pyfFormatIntegral f s (Just (padSize, alignMode, c)) grouping i

formatAnyFractional :: forall paddingWidth precision i t t'. (Integral paddingWidth, Integral precision, PyfFormatFractional i) => Formatters.Format t t' 'Formatters.Fractional -> Formatters.SignMode -> Maybe (paddingWidth, AnyAlign, Char) -> Maybe (Int, Char) -> Maybe precision -> i -> String
formatAnyFractional f s Nothing grouping p i = pyfFormatFractional @i @paddingWidth @precision f s Nothing grouping p i
formatAnyFractional f s (Just (padSize, AnyAlign alignMode, c)) grouping p i = pyfFormatFractional f s (Just (padSize, alignMode, c)) grouping p i

class FormatAny i k where
  formatAny :: forall paddingWidth precision. (Integral paddingWidth, Integral precision) => Formatters.SignMode -> PaddingK k paddingWidth -> Maybe (Int, Char) -> Maybe precision -> i -> String

instance (FormatAny2 (PyFClassify t) t k) => FormatAny t k where
  formatAny = formatAny2 (Proxy :: Proxy (PyFClassify t))

class FormatAny2 (c :: PyFCategory) (i :: Type) (k :: Formatters.AlignForString) where
  formatAny2 :: forall paddingWidth precision. (Integral paddingWidth, Integral precision) => Proxy c -> Formatters.SignMode -> PaddingK k paddingWidth -> Maybe (Int, Char) -> Maybe precision -> i -> String

instance (Show t, Integral t) => FormatAny2 'PyFIntegral t k where
  formatAny2 _ s a p _precision = formatAnyIntegral Formatters.Decimal s (paddingKToPadding a) p

instance (PyfFormatFractional t) => FormatAny2 'PyFFractional t k where
  formatAny2 _ s a = formatAnyFractional Formatters.Generic s (paddingKToPadding a)

newPaddingKForString :: Integral i => PaddingK 'Formatters.AlignAll i -> Maybe (Int, Formatters.AlignMode 'Formatters.AlignAll, Char)
newPaddingKForString padding = case padding of
  PaddingDefaultK -> Nothing
  PaddingK i Nothing -> Just (fromIntegral i, Formatters.AlignLeft, ' ') -- default align left and fill with space for string
  PaddingK i (Just (mc, a)) -> Just (fromIntegral i, a, fromMaybe ' ' mc)

-- TODO: _s(ign) and _grouping should trigger errors
instance (PyFToString t) => FormatAny2 'PyFString t 'Formatters.AlignAll where
  formatAny2 _ _s a _grouping precision t = Formatters.formatString (newPaddingKForString a) precision (pyfToString t)

instance TypeError ('Text "String type is incompatible with inside padding (=).") => FormatAny2 'PyFString t 'Formatters.AlignNumber where
  formatAny2 = error "Unreachable"
