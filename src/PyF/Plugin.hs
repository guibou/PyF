{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

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
import PyF (Format (..), defaultFloatPrecision, fmtConfig)
import PyF.Formatters (SignMode (..))
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
import Text.Parsec (runParserT)

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
    <$> gmapM (everywhereM (mkM sayYes)) m

sayYes :: HsExpr GhcPs -> Hsc (HsExpr GhcPs)
sayYes e = do
  case e of
    HsUntypedSplice _xsplit (HsQuasiQuote _xquasi (Unqual name) s)
      | mkVarOcc "fmt" == name -> do
          liftIO $ print $ (showPprUnsafe name)
          let items = map toString $ pyf (unpackFS $ unLoc s)
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
    _ -> do
      pure e

toString :: Item -> HsExpr GhcPs
toString (Raw s) = HsLit noExtField' $ HsString NoSourceText (mkFastString s)
toString (Replacement (expr, _unusedThExp) formatMode) = do
  let formatExpr = padAndFormat (fromMaybe DefaultFormatMode formatMode)
  app formatExpr expr

pyf :: String -> [Item]
pyf s = case runReader (runParserT (parseGenericFormatString) () filename s) context of
  Right r -> r
  Left e -> error $ show e
  where
    filename = "TODO"
    Config {..} = fmtConfig
    context = ParsingContext {enabledExtensions = [], ..}

-- TODO:
-- - missing fileposition for correct error reporting
-- - Maybe we can leverage the current "Hsc" in order to parser the internal values?
-- - correct filename during parsing
-- - todo: list the extensions (maybe not useful, if we leverage the
-- current Hsc for parsing)
--

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
  {-
  HexCapsF alt s -> [|formatAnyIntegral (Formatters.Upper $(withAlt alt Formatters.Hexa)) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  -}
  -- Floating
  GeneralF prec alt s -> var "formatAnyFractional" `app` withAlt alt Exponent `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  {-
  ExponentialCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Exponent)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  -}
  GeneralF prec alt s -> var "formatAnyFractional" `app` withAlt alt Generic `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  {-
  GeneralCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Generic)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  -}
  FixedF prec alt s -> var "formatAnyFractional" `app` withAlt alt Fixed `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  {-
  FixedCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Fixed)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  -}
  PercentF prec alt s -> var "formatAnyFractional" `app` withAlt alt Percent `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision defaultFloatPrecision prec
  -- Default / String
  DefaultF prec s -> var "formatAny" `app` toSignMode s `app` mkPadding padding `app` toGrp grouping 3 `app` mkPrecision Nothing prec
  StringF prec -> (var ".") `app` (var "formatString" `app` mkPadding padding `app` mkPrecision Nothing prec) `app` (var "pyfToString")
  e -> error $ "Not handled: " Prelude.<> show e

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
exprToInt (HaskellExpr (expr, _)) = expr

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
