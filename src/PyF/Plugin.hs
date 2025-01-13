{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module PyF.Plugin (plugin, forceError) where

import Data.Data
import qualified GHC.LanguageExtensions as LangExt

-- import Data.Generics

#if MIN_VERSION_ghc(9,0,0)
import GHC.Hs
import GHC.Plugins hiding (msg)
#else
import GHC
import GhcPlugins
#endif

import Control.Monad (join)
import Control.Monad.Reader (runReader)
import Data.Generics
import Data.Maybe (fromMaybe)
import GHC.TypeLits
import qualified GHC.Types.Name.Occurrence as GHC.Types.Name.Occurence
import GHC.Types.SourceText (SourceText (..), mkIntegralLit)
import PyF (defaultFloatPrecision, fmtConfig, trimIndent)
import PyF.Formatters
import qualified PyF.Internal.Parser as ParseExp
import PyF.Internal.PythonSyntax
  ( AlternateForm (..),
    ExprOrValue (..),
    FormatModeT (..),
    Item (..),
    PaddingT (..),
    ParsingContext (..),
    PrecisionT (..),
    TypeFormatT (..),
    parseGenericFormatString,
    pattern DefaultFormatMode,
  )
import PyF.Internal.QQ (Config (..), parseErrorToLocAndMessage)
import Text.Parsec (runParserT)
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

{-
 - This is not used, the idea was to report the error during the plugin, but it
 - actually fails the compilation completly.
reportError theLoc theMsg = do
  Hsc $ \env messages -> do
    pure
      ( (),
        addMessage
          ( MsgEnvelope
              { errMsgSpan = RealSrcSpan (realSrcLocSpan theLoc) mempty,
                -- TODO: maybe alwaysqualify can be refined
                errMsgContext = alwaysQualify,
                errMsgDiagnostic = GhcUnknownMessage (UnknownDiagnostic (mkPlainError noHints (text theMsg))),
                errMsgSeverity = SevWarning
              }
          )
          messages
      )
-}

applyPyf :: SrcAnn NoEpAnns -> String -> Hsc (HsExpr GhcPs)
applyPyf loc s = do
  let pyfItems = pyf loc s
  (join . fmap sequenceA -> itemsM) <- sequenceA (mapM toString <$> pyfItems)
  case itemsM of
    Left (theLoc, theMsg) -> do
      pure $ HsPar noExtField' noHsTok (L ((SrcSpanAnn noExtField' (RealSrcSpan (realSrcLocSpan theLoc) mempty))) $ var "forceError" `app` (ctor "Proxy" `appTypeSymbol` theMsg)) noHsTok
    Right items -> do
      dynFlags <- getDynFlags
      let toOverloaded
            | xopt LangExt.OverloadedStrings dynFlags = app (var "fromString")
            | otherwise = id
      pure $
        toOverloaded $
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
            (L noSrcSpanA $ ExplicitList emptyAnnList $ items)

appTypeSymbol :: HsExpr GhcPs -> String -> HsExpr GhcPs
appTypeSymbol a name = HsAppType NoExtField (L noSrcSpanA a) (L NoTokenLoc (HsTok)) (HsWC NoExtField (L noSrcSpanA (HsTyLit NoExtField (HsStrTy NoSourceText (mkFastString name)))))

-- TODO: a lot of the Either could be "Validation" and generate MULTIPLES
-- errors messages, but for now GHC is not able to handle multiples errors
toString :: Item -> Hsc (Either (RealSrcLoc, String) (GenLocated (SrcAnn AnnListItem) (HsExpr GhcPs)))
toString (Raw s) = pure $ pure $ L noSrcSpanA (HsLit noExtField' $ HsString NoSourceText (mkFastString s)) -- TODO: restore the correct location for the "raw" string
toString (Replacement loc s formatMode) = do
  exprM <- toHsExpr loc s
  formatExprM <- padAndFormat (fromMaybe DefaultFormatMode formatMode)

  -- We wrap the formatted expression using the location of the original expression
  -- Hence GHC will report type error at that location, if relevant
  pure $ do
    expr <- exprM
    formatExpr <- formatExprM
    let loc' = getLoc expr
    pure $ L loc' (formatExpr `app'` expr)

pyf :: SrcAnn NoEpAnns -> String -> Either (RealSrcLoc, String) [Item]
pyf (SrcSpanAnn _ srcSpan) s = case runReader (runParserT (setPosition initPos >> parseGenericFormatString) () filename s) context of
  Right r -> Right r
  Left e -> Left (loc, msg)
    where
      (loc, msg) = parseErrorToLocAndMessage e
  where
    filename = unpackFS $ srcLocFile start
    Config {..} = fmtConfig
    context = ParsingContext {..}

    initPos = setSourceColumn (setSourceLine (initialPos filename) (srcLocLine start)) (srcLocCol start)
    start = case srcSpanStart srcSpan of
      RealSrcLoc startLoc _ -> startLoc
      _ -> error "Plugin API does not know it's RealSrcLoc"

appType :: HsExpr GhcPs -> String -> HsExpr GhcPs
appType a name = HsAppType NoExtField (L noSrcSpanA a) (L NoTokenLoc (HsTok)) (HsWC NoExtField (L noSrcSpanA (HsTyVar noExtField' NotPromoted (L noSrcSpanA (mkUnqual dataName (mkFastString name))))))

appType' :: HsExpr GhcPs -> String -> HsExpr GhcPs
appType' a name = HsAppType NoExtField (L noSrcSpanA a) (L NoTokenLoc (HsTok)) (HsWC NoExtField (L noSrcSpanA (HsTyVar noExtField' NotPromoted (L noSrcSpanA (mkUnqual tcName (mkFastString name))))))

app :: HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
app a b = HsApp noExtField' (L noSrcSpanA a) (L noSrcSpanA b)

app' :: HsExpr GhcPs -> (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> HsExpr GhcPs
app' a b = HsApp noExtField' (L noSrcSpanA a) b

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

padAndFormat :: FormatModeT (ExprOrValue Int) -> Hsc (Either (RealSrcLoc, String) (HsExpr GhcPs))
padAndFormat formatMode' = do
  formatModeM <- evalSubExpression formatMode'
  pure $ do
    FormatMode padding tf grouping <- formatModeM
    pure $ case tf of
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

evalSubExpression :: FormatModeT (ExprOrValue Int) -> Hsc (Either (RealSrcLoc, String) (FormatModeT (LocatedA (HsExpr GhcPs))))
evalSubExpression (FormatMode padding tf grouping) = do
  padding' <- evalPadding padding
  tf' <- evalTf tf
  pure $ FormatMode <$> padding' <*> tf' <*> pure grouping

evalTf :: TypeFormatT (ExprOrValue Int) -> Hsc (Either (RealSrcLoc, String) (TypeFormatT (LocatedA (HsExpr GhcPs))))
evalTf tf = case tf of
  -- Integrals
  BinaryF alt s -> pure $ pure $ BinaryF alt s
  CharacterF -> pure $ pure $ CharacterF
  DecimalF s -> pure $ pure $ DecimalF s
  HexF alt s -> pure $ pure $ HexF alt s
  OctalF alt s -> pure $ pure $ OctalF alt s
  HexCapsF alt s -> pure $ pure $ HexCapsF alt s
  -- Floating
  GeneralF prec alt s -> do
    prec' <- evalPrecision prec
    pure $ GeneralF <$> prec' <*> pure alt <*> pure s
  GeneralCapsF prec alt s -> do
    prec' <- evalPrecision prec
    pure $ GeneralCapsF <$> prec' <*> pure alt <*> pure s
  ExponentialF prec alt s -> do
    prec' <- evalPrecision prec
    pure $ ExponentialF <$> prec' <*> pure alt <*> pure s
  ExponentialCapsF prec alt s -> do
    prec' <- evalPrecision prec
    pure $ ExponentialCapsF <$> prec' <*> pure alt <*> pure s
  FixedF prec alt s -> do
    prec' <- evalPrecision prec
    pure $ FixedF <$> prec' <*> pure alt <*> pure s
  FixedCapsF prec alt s -> do
    prec' <- evalPrecision prec
    pure $ FixedCapsF <$> prec' <*> pure alt <*> pure s
  PercentF prec alt s -> do
    prec' <- evalPrecision prec
    pure $ PercentF <$> prec' <*> pure alt <*> pure s
  -- Default / String
  DefaultF prec s -> do
    prec' <- evalPrecision prec
    pure $ DefaultF <$> prec' <*> pure s
  StringF prec -> do
    prec' <- evalPrecision prec
    pure $ StringF <$> prec'

evalPrecision :: PrecisionT (ExprOrValue Int) -> Hsc (Either (RealSrcLoc, String) (PrecisionT (LocatedA (HsExpr GhcPs))))
evalPrecision (PrecisionDefault) = pure $ pure PrecisionDefault
evalPrecision (Precision e) = fmap Precision <$> exprToInt e

evalPadding :: PaddingT (ExprOrValue Int) -> Hsc (Either (RealSrcLoc, String) (PaddingT (LocatedA (HsExpr GhcPs))))
evalPadding p = case p of
  PaddingDefault -> pure $ pure PaddingDefault
  Padding i v -> do
    i' <- exprToInt i
    pure $ Padding <$> i' <*> pure v

mkPaddingToPaddingK :: PaddingT (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> HsExpr GhcPs
mkPaddingToPaddingK p = case p of
  PaddingDefault -> ctor "PaddingDefaultK"
  Padding i Nothing -> appType (appType' (ctor "PaddingK") "Int") "AlignAll" `app'` i `app` (liftHsExpr $ (Nothing :: Maybe (Int, AnyAlign, Char)))
  Padding i (Just (c, AnyAlign a)) -> ctor "PaddingK" `app'` i `app` liftHsExpr (Just (c, a))

newPaddingKForString :: PaddingT (LocatedA (HsExpr GhcPs)) -> HsExpr GhcPs
newPaddingKForString padding = case padding of
  PaddingDefault -> ctor "Nothing"
  Padding i Nothing -> liftHsExpr (Just (i, AlignLeft, ' ')) -- default align left and fill with space for string
  Padding i (Just (mc, AnyAlign a)) -> liftHsExpr (Just (i, a, fromMaybe ' ' mc))

mkPadding :: PaddingT (LocatedA (HsExpr GhcPs)) -> HsExpr GhcPs
mkPadding padding = case padding of
  PaddingDefault -> liftHsExpr (Nothing :: Maybe (Int, AnyAlign, Char))
  (Padding i al) -> case al of
    Nothing -> liftHsExpr (Just (i, AnyAlign AlignRight, ' ')) -- Right align and space is default for any object, except string
    Just (Nothing, a) -> liftHsExpr $ Just (i, a, ' ')
    Just (Just c, a) -> liftHsExpr $ Just (i, a, c)

class LiftHsExpr a where
  liftHsExpr :: a -> HsExpr GhcPs

instance (LiftHsExpr a, LiftHsExpr b) => LiftHsExpr (a, b) where
  liftHsExpr (a, b) = mkTup [liftHsExpr a, liftHsExpr b]

instance (LiftHsExpr a, LiftHsExpr b, LiftHsExpr c) => LiftHsExpr (a, b, c) where
  liftHsExpr (a, b, c) = mkTup [liftHsExpr a, liftHsExpr b, liftHsExpr c]

instance (LiftHsExpr a) => LiftHsExpr (Maybe a) where
  liftHsExpr Nothing = ctor "Nothing"
  liftHsExpr (Just v) = ctor "Just" `app` liftHsExpr v

instance LiftHsExpr (AlignMode k) where
  liftHsExpr v = ctor (show v)

instance LiftHsExpr Char where
  liftHsExpr c = HsLit noExtField' (HsChar NoSourceText c)

instance LiftHsExpr Int where
  liftHsExpr i = HsLit noExtField' $ HsInt NoExtField (mkIntegralLit i)

instance LiftHsExpr (Format k k' k'') where
  liftHsExpr (Alternate v) = ctor "Alternate" `app` liftHsExpr v
  liftHsExpr (Upper v) = ctor "Upper" `app` liftHsExpr v
  liftHsExpr v = ctor (show v)

instance LiftHsExpr AnyAlign where
  liftHsExpr (AnyAlign v) = ctor "AnyAlign" `app` liftHsExpr v

instance LiftHsExpr (HsExpr GhcPs) where
  liftHsExpr x = x

instance LiftHsExpr (GenLocated SrcSpanAnnA (HsExpr GhcPs)) where
  liftHsExpr x = HsPar noExtField' noHsTok x noHsTok

mkTup :: [HsExpr GhcPs] -> HsExpr GhcPs
mkTup l =
  ExplicitTuple
    noExtField'
    ( map
        (\x -> Present noExtField' (L noSrcSpanA x))
        l
    )
    Boxed

toSignMode :: SignMode -> HsExpr GhcPs
toSignMode Plus = ctor "Plus"
toSignMode Minus = ctor "Minus"
toSignMode Space = ctor "Space"

toGrp :: Maybe Char -> Int -> HsExpr GhcPs
toGrp Nothing _ = ctor "Nothing"
toGrp (Just v) a = liftHsExpr $ Just grp
  where
    grp = (a, v)

withAlt :: AlternateForm -> Format 'CanAlt b c -> HsExpr GhcPs
withAlt NormalForm e = liftHsExpr e
withAlt AlternateForm e = liftHsExpr (Alternate e)

mkPrecision :: Maybe Int -> PrecisionT (LocatedA (HsExpr GhcPs)) -> HsExpr GhcPs
mkPrecision Nothing PrecisionDefault = ctor "Nothing"
mkPrecision (Just v) PrecisionDefault = ctor "Just" `app` (HsLit noExtField' $ HsInt NoExtField (mkIntegralLit v))
mkPrecision _ (Precision p) = liftHsExpr (Just p)

exprToInt :: ExprOrValue Int -> Hsc (Either (RealSrcLoc, String) (LocatedA (HsExpr GhcPs)))
exprToInt (Value i) = pure $ pure $ noLocA $ HsLit noExtField' $ HsInt NoExtField (mkIntegralLit i)
exprToInt (HaskellExpr loc s) = toHsExpr loc s

toHsExpr :: SourcePos -> String -> Hsc (Either (RealSrcLoc, String) (LocatedA (HsExpr GhcPs)))
toHsExpr sourcePos s = do
  dynFlags <- getDynFlags
  let srcLoc = mkRealSrcLoc (mkFastString (sourceName sourcePos)) (sourceLine sourcePos) (sourceColumn sourcePos)

  case ParseExp.parseExpression srcLoc s dynFlags of
    Right res -> pure $ Right res
    Left e -> pure $ Left e

class ForceError (m :: Symbol) where
  forceError :: Proxy m -> t

instance (TypeError (Text m)) => ForceError m where
  forceError = undefined

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
