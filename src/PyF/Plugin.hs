{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module PyF.Plugin (plugin) where

import Data.Data

-- import Data.Generics

#if MIN_VERSION_ghc(9,0,0)
import GHC.Hs
import GHC.Plugins
#else
import GHC
import GhcPlugins
#endif

import Control.Monad.Reader (runReader)
import Data.Generics
import Data.Maybe (fromMaybe)
import qualified GHC.Types.Name.Occurrence as GHC.Types.Name.Occurence
import GHC.Types.SourceText (SourceText (..), mkIntegralLit)
import PyF (Format (..), defaultFloatPrecision, fmtConfig, trimIndent)
import PyF.Formatters (AnyAlign (..), SignMode (..))
import qualified PyF.Internal.Parser as ParseExp
import PyF.Internal.PythonSyntax
  ( AlternateForm (..),
    ExprOrValue (..),
    FormatMode (..),
    Item (..),
    Padding (..),
    ParsingContext (..),
    Precision (..),
    TypeFormat (..),
    parseGenericFormatString,
    pattern DefaultFormatMode,
  )
import PyF.Internal.QQ (Config (..))
import Text.Parsec (SourcePos, runParserT, setSourceLine)
import Text.Parsec.Pos
import Text.Parsec.Prim (setPosition)

-- | A plugin which replaces all [fmt|...] from PyF by a pure haskell code
-- without template haskell.
plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = purePlugin,
      parsedResultAction = \_ _ parsedResult -> do
        m <- action $ parsedResultModule parsedResult
        pure $ parsedResult {parsedResultModule = m}
    }

action :: HsParsedModule -> Hsc HsParsedModule
action parsed@HsParsedModule {hpm_module = m} =
  (\m' -> parsed {hpm_module = m'})
    <$> gmapM (everywhereM (mkM replaceSplice)) m

-- | Replace a splice entry
replaceSplice :: HsExpr GhcPs -> Hsc (HsExpr GhcPs)
replaceSplice e = do
  case e of
    HsUntypedSplice _xsplit (HsQuasiQuote _xquasi (Unqual name) (L loc s))
      | mkVarOcc "fmt" == name -> applyPyf loc $ unpackFS s
      | mkVarOcc "fmtTrim" == name -> applyPyf loc (trimIndent $ unpackFS s)
      | mkVarOcc "str" == name -> pure $ HsLit noExtField' (HsString NoSourceText s)
      | mkVarOcc "strTrim" == name -> pure $ HsLit noExtField' (HsString NoSourceText (mkFastString $ trimIndent $ unpackFS s))
    _ -> do
      pure e

applyPyf :: SrcAnn NoEpAnns -> String -> Hsc (HsExpr GhcPs)
applyPyf loc s = do
  items <- mapM toString $ pyf loc s
  pure $
    HsApp
      noExtField'
      ( L
          noSrcSpanA
          ( HsVar
              NoExtField
              ( L noSrcSpanA $
                  mkUnqual GHC.Types.Name.Occurence.varName (mkFastString "mconcat")
              )
          )
      )
      (L noSrcSpanA $ ExplicitList emptyAnnList $ map (L noSrcSpanA) items)

toString :: Item -> Hsc (HsExpr GhcPs)
toString (Raw s) = pure $ HsLit noExtField' $ HsString NoSourceText (mkFastString s)
toString (Replacement loc s formatMode) = do
  expr <- toHsExpr loc s
  let formatExpr = padAndFormat (fromMaybe DefaultFormatMode formatMode)
  pure $ app formatExpr expr

pyf :: SrcAnn NoEpAnns -> String -> [Item]
pyf (SrcSpanAnn _ srcSpan) s = case runReader (runParserT (setPosition initPos >> parseGenericFormatString) () filename s) context of
  Right r -> r
  Left e -> error $ show e
  where
    filename = unpackFS $ srcLocFile start
    Config {..} = fmtConfig
    context = ParsingContext {..}

    initPos = setSourceColumn (setSourceLine (initialPos filename) (srcLocLine start)) (srcLocCol start)
    RealSrcLoc start _ = srcSpanStart srcSpan

app :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
app a b = HsApp noExtField' (L noSrcSpanA a) (L noSrcSpanA b)

var :: String -> HsExpr GhcPs
var name =
  ( HsVar
      NoExtField
      ( L noSrcSpanA $
          mkUnqual GHC.Types.Name.Occurence.varName (mkFastString name)
      )
  )

ctor :: String -> HsExpr GhcPs
ctor name =
  ( HsVar
      NoExtField
      ( L noSrcSpanA $
          mkUnqual GHC.Types.Name.Occurence.dataName (mkFastString name)
      )
  )

padAndFormat :: FormatMode -> HsExpr GhcPs
padAndFormat (FormatMode padding tf grouping) = case tf of
  -- Integrals
  BinaryF alt s -> var "formatAnyIntegral" `app` withAlt alt Binary `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 4
  CharacterF -> var "formatAnyIntegral" `app` ctor "Character" `app` ctor "Minus" `app` mkPadding padding `app` ctor "Nothing"
  DecimalF s -> var "formatAnyIntegral" `app` ctor "Decimal" `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3
  HexF alt s -> var "formatAnyIntegral" `app` withAlt alt Hexa `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 4
  OctalF alt s -> var "formatAnyIntegral" `app` withAlt alt Octal `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 4
  HexCapsF alt s -> var "formatAnyIntegral" `app` (ctor "Upper" `app` (withAlt alt Hexa)) `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 4
  -- Floating
  GeneralF prec alt s -> var "formatAnyFractional" `app` withAlt alt Generic `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  GeneralCapsF prec alt s -> var "formatAnyFractional" `app` (ctor "Upper" `app` (withAlt alt Generic)) `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  ExponentialF prec alt s -> var "formatAnyFractional" `app` withAlt alt Exponent `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  ExponentialCapsF prec alt s -> var "formatAnyFractional" `app` (ctor "Upper" `app` (withAlt alt Exponent)) `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  FixedF prec alt s -> var "formatAnyFractional" `app` withAlt alt Fixed `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  FixedCapsF prec alt s -> var "formatAnyFractional" `app` (ctor "Upper" `app` (withAlt alt Fixed)) `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  PercentF prec alt s -> var "formatAnyFractional" `app` withAlt alt Percent `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  -- Default / String
  DefaultF prec s -> var "formatAny" `app` toSignMode s `app` mkPaddingToPaddingK padding `app` toGrp grouping 3 `app` mkPrecision Nothing prec
  StringF prec -> (var ".") `app` (var "formatString" `app` (newPaddingKForString padding) `app` mkPrecision Nothing prec) `app` (var "pyfToString")

mkPaddingToPaddingK :: Padding -> HsExpr GhcPs
mkPaddingToPaddingK p = case p of
  PaddingDefault -> ctor "PaddingDefaultK"
  Padding i Nothing -> ctor "PaddingK" `app` exprToInt i `app` ctor "Nothing"
  Padding i (Just (c, AnyAlign a)) -> ctor "PaddingK" `app` exprToInt i `app` (ctor "Just" `app` (error "tuple (c, a)"))

newPaddingKForString :: Padding -> HsExpr GhcPs
newPaddingKForString padding = case padding of
  PaddingDefault -> ctor "Nothing"
  Padding i Nothing -> ctor "Just" `app` (error "(fromIntegral i, Formatters.AlignLeft, ' ')") -- default align left and fill with space for string
  Padding i (Just (c, AnyAlign a)) -> ctor "Just" `app` (error "(fromIntegral i, a, fromMaybe ' ' mc)")

toSignMode :: SignMode -> HsExpr GhcPs
toSignMode Plus = ctor "Plus"
toSignMode Minus = ctor "Minus"
toSignMode Space = ctor "Space"

toGrp :: Maybe Char -> Int -> HsExpr GhcPs
toGrp Nothing _ = ctor "Nothing"

withAlt :: AlternateForm -> Format a b c -> HsExpr GhcPs
withAlt NormalForm formatter = var $ show formatter

-- toGrp (Just v) a = ctor "Just" `app` (error "not done to grp")
--   where
--     grp = (a,v)

mkPadding :: Padding -> HsExpr GhcPs
mkPadding PaddingDefault = ctor "Nothing"

mkPrecision :: Maybe Int -> Precision -> HsExpr GhcPs
mkPrecision Nothing PrecisionDefault = ctor "Nothing"
mkPrecision (Just v) PrecisionDefault = ctor "Just" `app` (HsLit noExtField' $ HsInt NoExtField (mkIntegralLit v))
mkPrecision _ (Precision p) = ctor "Just" `app` exprToInt p

exprToInt :: ExprOrValue Int -> HsExpr GhcPs
exprToInt (Value i) = HsLit noExtField' $ HsInt NoExtField (mkIntegralLit i)

-- exprToInt (HaskellExpr s) = toHsExpr s

toHsExpr :: SourcePos -> String -> Hsc (HsExpr GhcPs)
toHsExpr loc s = do
  dynFlags <- getDynFlags
  -- TODO
  let initLoc = mkRealSrcLoc (mkFastString "file") 10 10

  case ParseExp.parseExpression initLoc s dynFlags of
    Right res -> pure res
    Left e -> error $ show e

#if MIN_VERSION_ghc(9,10,0)
noExtField' :: NoExtField
noExtField' = NoExtField

emptyAnnList :: AnnList
emptyAnnList = AnnList Nothing Nothing Nothing [] []
#else
noExtField' :: EpAnn ann
noExtField' = EpAnnNotUsed

emptyAnnList :: EpAnn ann
emptyAnnList = EpAnnNotUsed
#endif
