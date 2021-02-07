{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module PyF.Internal.Meta (toExp, baseDynFlags, translateTHtoGHCExt) where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Hs.Type (HsWildCardBndrs (..), HsType (..))
#elif MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Types (HsWildCardBndrs (..), HsType (..))
#else
import HsTypes (HsWildCardBndrs (..), HsType (..))
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Expr as Expr
import GHC.Hs.Extension as Ext
import GHC.Hs.Pat (HsRecFields (..))
import GHC.Hs.Lit
#else
import HsExpr as Expr
import HsExtension as Ext
import HsPat (HsRecFields (..))
import HsLit
#endif

import qualified Data.ByteString as B
import qualified Language.Haskell.TH.Syntax as GhcTH
import qualified "template-haskell" Language.Haskell.TH.Syntax as TH
import PyF.Internal.ParserEx (fakeLlvmConfig, fakeSettings)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Data.FastString
import GHC.Utils.Outputable (ppr, showSDocDebug)
import GHC.Types.Basic (il_value, fl_value)
import GHC.Driver.Session (DynFlags, xopt_set, defaultDynFlags)
#else
import SrcLoc
import Name
import RdrName
import FastString
import Outputable (ppr, showSDocDebug)
import BasicTypes (il_value, fl_value)
import DynFlags (DynFlags, xopt_set, defaultDynFlags)
#endif

toName :: Ext.IdP GhcPs -> TH.Name
toName = TH.mkName . occNameString . rdrNameOcc

toLit :: HsLit GhcPs -> TH.Lit
toLit (HsChar _ c) = TH.CharL c
toLit (HsCharPrim _ c) = TH.CharPrimL c
toLit (HsString _ s) = TH.StringL (unpackFS s)
toLit (HsStringPrim _ s) = TH.StringPrimL (B.unpack s)
toLit (HsInt _ i) = TH.IntegerL (il_value i)
toLit (HsIntPrim _ i) = TH.IntPrimL i
toLit (HsWordPrim _ i) = TH.WordPrimL i
toLit (HsInt64Prim _ i) = TH.IntegerL i
toLit (HsWord64Prim _ i) = TH.WordPrimL i
toLit (HsInteger _ i _) = TH.IntegerL i
toLit (HsRat _ f _) = TH.FloatPrimL (fl_value f)
toLit (HsFloatPrim _ f) = TH.FloatPrimL (fl_value f)
toLit (HsDoublePrim _ f) = TH.DoublePrimL (fl_value f)
toLit (XLit _) = noTH "toLit" "XLit"

toLit' :: OverLitVal -> TH.Lit
toLit' (HsIntegral i) = TH.IntegerL (il_value i)
toLit' (HsFractional f) = TH.RationalL (fl_value f)
toLit' (HsIsString _ fs) = TH.StringL (unpackFS fs)

toType :: HsType GhcPs -> TH.Type
toType (HsTyVar _ _ n) =
  let n' = unLoc n
   in if isRdrTyVar n'
        then TH.VarT (toName n')
        else TH.ConT (toName n')
toType t = todo "toType" (showSDocDebug (baseDynFlags []) . ppr $ t)

toFieldExp :: a
toFieldExp = undefined

toExp :: DynFlags -> Expr.HsExpr GhcPs -> TH.Exp
toExp _ (Expr.HsVar _ n) =
  let n' = unLoc n
   in if isRdrDataCon n'
        then TH.ConE (toName n')
        else TH.VarE (toName n')
#if MIN_VERSION_ghc(9,0,0)
toExp _ (Expr.HsUnboundVar _ n)              = TH.UnboundVarE (TH.mkName . occNameString $ n)
#else
toExp _ (Expr.HsUnboundVar _ n)              = TH.UnboundVarE (TH.mkName . occNameString . Expr.unboundVarOcc $ n)
#endif
toExp _ Expr.HsIPVar {} = noTH "toExp" "HsIPVar"
toExp _ (Expr.HsLit _ l) = TH.LitE (toLit l)
toExp _ (Expr.HsOverLit _ OverLit {ol_val}) = TH.LitE (toLit' ol_val)
toExp d (Expr.HsApp _ e1 e2) = TH.AppE (toExp d . unLoc $ e1) (toExp d . unLoc $ e2)
toExp d (Expr.HsAppType _ e HsWC {hswc_body}) = TH.AppTypeE (toExp d . unLoc $ e) (toType . unLoc $ hswc_body)
toExp d (Expr.OpApp _ e1 o e2) = TH.UInfixE (toExp d . unLoc $ e1) (toExp d . unLoc $ o) (toExp d . unLoc $ e2)
toExp d (Expr.NegApp _ e _) = TH.AppE (TH.VarE 'negate) (toExp d . unLoc $ e)
-- toExp (Expr.Lambda _ ps e)                    = TH.LamE (fmap toPat ps) (toExp e)
-- toExp (Expr.Let _ bs e)                       = TH.LetE (toDecs bs) (toExp e)
-- toExp (Expr.If _ a b c)                       = TH.CondE (toExp a) (toExp b) (toExp c)
-- toExp (Expr.MultiIf _ ifs)                    = TH.MultiIfE (map toGuard ifs)
-- toExp (Expr.Case _ e alts)                    = TH.CaseE (toExp e) (map toMatch alts)
-- toExp (Expr.Do _ ss)                          = TH.DoE (map toStmt ss)
-- toExp e@Expr.MDo{}                            = noTH "toExp" e
-- toExp (Expr.Tuple _ Exts.Boxed xs)            = TH.TupE (fmap toTupEl xs)
-- toExp (Expr.Tuple _ Exts.Unboxed xs)          = TH.UnboxedTupE (fmap toTupEl xs)
-- toExp e@Expr.TupleSection{}                   = noTH "toExp" e
-- toExp (Expr.List _ xs)                        = TH.ListE (fmap toExp xs)
toExp d (Expr.HsPar _ e) = TH.ParensE (toExp d . unLoc $ e)
-- toExp (Expr.LeftSection _ e o)                = TH.InfixE (Just . toExp $ e) (toExp o) Nothing
-- toExp (Expr.RightSection _ o f)               = TH.InfixE Nothing (toExp o) (Just . toExp $ f)
toExp _ (Expr.RecordCon _ name HsRecFields {rec_flds}) =
  TH.RecConE (toName . unLoc $ name) (fmap toFieldExp rec_flds)
-- toExp (Expr.RecUpdate _ e xs)                 = TH.RecUpdE (toExp e) (fmap toFieldExp xs)
-- toExp (Expr.EnumFrom _ e)                     = TH.ArithSeqE $ TH.FromR (toExp e)
-- toExp (Expr.EnumFromTo _ e f)                 = TH.ArithSeqE $ TH.FromToR (toExp e) (toExp f)
-- toExp (Expr.EnumFromThen _ e f)               = TH.ArithSeqE $ TH.FromThenR (toExp e) (toExp f)
-- toExp (Expr.EnumFromThenTo _ e f g)           = TH.ArithSeqE $ TH.FromThenToR (toExp e) (toExp f) (toExp g)
-- toExp (Expr.ListComp _ e ss)                  = TH.CompE $ map convert ss ++ [TH.NoBindS (toExp e)]
--  where
--   convert (Expr.QualStmt _ st)                = toStmt st
--   convert s                                   = noTH "toExp ListComp" s
-- toExp (Expr.ExpTypeSig _ e t)                 = TH.SigE (toExp e) (toType t)
toExp dynFlags e = todo "toExp" (showSDocDebug dynFlags . ppr $ e)

todo :: (Show e) => String -> e -> a
todo fun thing = error . concat $ [moduleName, ".", fun, ": not implemented: ", show thing]

noTH :: (Show e) => String -> e -> a
noTH fun thing = error . concat $ [moduleName, ".", fun, ": no TemplateHaskell for: ", show thing]

moduleName :: String
moduleName = "PyF.Internal.Meta"

baseDynFlags :: [GhcTH.Extension] -> DynFlags
baseDynFlags exts =
  let enable = GhcTH.TemplateHaskellQuotes : exts
   in foldl xopt_set (defaultDynFlags fakeSettings fakeLlvmConfig) enable

translateTHtoGHCExt :: TH.Extension -> GhcTH.Extension
translateTHtoGHCExt TH.Cpp = GhcTH.Cpp
translateTHtoGHCExt TH.OverlappingInstances = GhcTH.OverlappingInstances
translateTHtoGHCExt TH.UndecidableInstances = GhcTH.UndecidableInstances
translateTHtoGHCExt TH.IncoherentInstances = GhcTH.IncoherentInstances
translateTHtoGHCExt TH.UndecidableSuperClasses = GhcTH.UndecidableSuperClasses
translateTHtoGHCExt TH.MonomorphismRestriction = GhcTH.MonomorphismRestriction
translateTHtoGHCExt TH.MonoPatBinds = GhcTH.MonoPatBinds
translateTHtoGHCExt TH.MonoLocalBinds = GhcTH.MonoLocalBinds
translateTHtoGHCExt TH.RelaxedPolyRec = GhcTH.RelaxedPolyRec
translateTHtoGHCExt TH.ExtendedDefaultRules = GhcTH.ExtendedDefaultRules
translateTHtoGHCExt TH.ForeignFunctionInterface = GhcTH.ForeignFunctionInterface
translateTHtoGHCExt TH.UnliftedFFITypes = GhcTH.UnliftedFFITypes
translateTHtoGHCExt TH.InterruptibleFFI = GhcTH.InterruptibleFFI
translateTHtoGHCExt TH.CApiFFI = GhcTH.CApiFFI
translateTHtoGHCExt TH.GHCForeignImportPrim = GhcTH.GHCForeignImportPrim
translateTHtoGHCExt TH.JavaScriptFFI = GhcTH.JavaScriptFFI
translateTHtoGHCExt TH.ParallelArrays = GhcTH.ParallelArrays
translateTHtoGHCExt TH.Arrows = GhcTH.Arrows
translateTHtoGHCExt TH.TemplateHaskell = GhcTH.TemplateHaskell
translateTHtoGHCExt TH.TemplateHaskellQuotes = GhcTH.TemplateHaskellQuotes
translateTHtoGHCExt TH.QuasiQuotes = GhcTH.QuasiQuotes
translateTHtoGHCExt TH.ImplicitParams = GhcTH.ImplicitParams
translateTHtoGHCExt TH.ImplicitPrelude = GhcTH.ImplicitPrelude
translateTHtoGHCExt TH.ScopedTypeVariables = GhcTH.ScopedTypeVariables
translateTHtoGHCExt TH.AllowAmbiguousTypes = GhcTH.AllowAmbiguousTypes
translateTHtoGHCExt TH.UnboxedTuples = GhcTH.UnboxedTuples
translateTHtoGHCExt TH.UnboxedSums = GhcTH.UnboxedSums
translateTHtoGHCExt TH.BangPatterns = GhcTH.BangPatterns
translateTHtoGHCExt TH.TypeFamilies = GhcTH.TypeFamilies
translateTHtoGHCExt TH.TypeFamilyDependencies = GhcTH.TypeFamilyDependencies
translateTHtoGHCExt TH.TypeInType = GhcTH.TypeInType
translateTHtoGHCExt TH.OverloadedStrings = GhcTH.OverloadedStrings
translateTHtoGHCExt TH.OverloadedLists = GhcTH.OverloadedLists
translateTHtoGHCExt TH.NumDecimals = GhcTH.NumDecimals
translateTHtoGHCExt TH.DisambiguateRecordFields = GhcTH.DisambiguateRecordFields
translateTHtoGHCExt TH.RecordWildCards = GhcTH.RecordWildCards
translateTHtoGHCExt TH.RecordPuns = GhcTH.RecordPuns
translateTHtoGHCExt TH.ViewPatterns = GhcTH.ViewPatterns
translateTHtoGHCExt TH.GADTs = GhcTH.GADTs
translateTHtoGHCExt TH.GADTSyntax = GhcTH.GADTSyntax
translateTHtoGHCExt TH.NPlusKPatterns = GhcTH.NPlusKPatterns
translateTHtoGHCExt TH.DoAndIfThenElse = GhcTH.DoAndIfThenElse
translateTHtoGHCExt TH.BlockArguments = GhcTH.BlockArguments
translateTHtoGHCExt TH.RebindableSyntax = GhcTH.RebindableSyntax
translateTHtoGHCExt TH.ConstraintKinds = GhcTH.ConstraintKinds
translateTHtoGHCExt TH.PolyKinds = GhcTH.PolyKinds
translateTHtoGHCExt TH.DataKinds = GhcTH.DataKinds
translateTHtoGHCExt TH.InstanceSigs = GhcTH.InstanceSigs
translateTHtoGHCExt TH.ApplicativeDo = GhcTH.ApplicativeDo
translateTHtoGHCExt TH.StandaloneDeriving = GhcTH.StandaloneDeriving
translateTHtoGHCExt TH.DeriveDataTypeable = GhcTH.DeriveDataTypeable
translateTHtoGHCExt TH.AutoDeriveTypeable = GhcTH.AutoDeriveTypeable
translateTHtoGHCExt TH.DeriveFunctor = GhcTH.DeriveFunctor
translateTHtoGHCExt TH.DeriveTraversable = GhcTH.DeriveTraversable
translateTHtoGHCExt TH.DeriveFoldable = GhcTH.DeriveFoldable
translateTHtoGHCExt TH.DeriveGeneric = GhcTH.DeriveGeneric
translateTHtoGHCExt TH.DefaultSignatures = GhcTH.DefaultSignatures
translateTHtoGHCExt TH.DeriveAnyClass = GhcTH.DeriveAnyClass
translateTHtoGHCExt TH.DeriveLift = GhcTH.DeriveLift
translateTHtoGHCExt TH.DerivingStrategies = GhcTH.DerivingStrategies
translateTHtoGHCExt TH.DerivingVia = GhcTH.DerivingVia
translateTHtoGHCExt TH.TypeSynonymInstances = GhcTH.TypeSynonymInstances
translateTHtoGHCExt TH.FlexibleContexts = GhcTH.FlexibleContexts
translateTHtoGHCExt TH.FlexibleInstances = GhcTH.FlexibleInstances
translateTHtoGHCExt TH.ConstrainedClassMethods = GhcTH.ConstrainedClassMethods
translateTHtoGHCExt TH.MultiParamTypeClasses = GhcTH.MultiParamTypeClasses
translateTHtoGHCExt TH.NullaryTypeClasses = GhcTH.NullaryTypeClasses
translateTHtoGHCExt TH.FunctionalDependencies = GhcTH.FunctionalDependencies
translateTHtoGHCExt TH.UnicodeSyntax = GhcTH.UnicodeSyntax
translateTHtoGHCExt TH.ExistentialQuantification = GhcTH.ExistentialQuantification
translateTHtoGHCExt TH.MagicHash = GhcTH.MagicHash
translateTHtoGHCExt TH.EmptyDataDecls = GhcTH.EmptyDataDecls
translateTHtoGHCExt TH.KindSignatures = GhcTH.KindSignatures
translateTHtoGHCExt TH.RoleAnnotations = GhcTH.RoleAnnotations
translateTHtoGHCExt TH.ParallelListComp = GhcTH.ParallelListComp
translateTHtoGHCExt TH.TransformListComp = GhcTH.TransformListComp
translateTHtoGHCExt TH.MonadComprehensions = GhcTH.MonadComprehensions
translateTHtoGHCExt TH.GeneralizedNewtypeDeriving = GhcTH.GeneralizedNewtypeDeriving
translateTHtoGHCExt TH.RecursiveDo = GhcTH.RecursiveDo
translateTHtoGHCExt TH.PostfixOperators = GhcTH.PostfixOperators
translateTHtoGHCExt TH.TupleSections = GhcTH.TupleSections
translateTHtoGHCExt TH.PatternGuards = GhcTH.PatternGuards
translateTHtoGHCExt TH.LiberalTypeSynonyms = GhcTH.LiberalTypeSynonyms
translateTHtoGHCExt TH.RankNTypes = GhcTH.RankNTypes
translateTHtoGHCExt TH.ImpredicativeTypes = GhcTH.ImpredicativeTypes
translateTHtoGHCExt TH.TypeOperators = GhcTH.TypeOperators
translateTHtoGHCExt TH.ExplicitNamespaces = GhcTH.ExplicitNamespaces
translateTHtoGHCExt TH.PackageImports = GhcTH.PackageImports
translateTHtoGHCExt TH.ExplicitForAll = GhcTH.ExplicitForAll
translateTHtoGHCExt TH.AlternativeLayoutRule = GhcTH.AlternativeLayoutRule
translateTHtoGHCExt TH.AlternativeLayoutRuleTransitional = GhcTH.AlternativeLayoutRuleTransitional
translateTHtoGHCExt TH.DatatypeContexts = GhcTH.DatatypeContexts
translateTHtoGHCExt TH.NondecreasingIndentation = GhcTH.NondecreasingIndentation
translateTHtoGHCExt TH.RelaxedLayout = GhcTH.RelaxedLayout
translateTHtoGHCExt TH.TraditionalRecordSyntax = GhcTH.TraditionalRecordSyntax
translateTHtoGHCExt TH.LambdaCase = GhcTH.LambdaCase
translateTHtoGHCExt TH.MultiWayIf = GhcTH.MultiWayIf
translateTHtoGHCExt TH.BinaryLiterals = GhcTH.BinaryLiterals
translateTHtoGHCExt TH.NegativeLiterals = GhcTH.NegativeLiterals
translateTHtoGHCExt TH.HexFloatLiterals = GhcTH.HexFloatLiterals
translateTHtoGHCExt TH.DuplicateRecordFields = GhcTH.DuplicateRecordFields
translateTHtoGHCExt TH.OverloadedLabels = GhcTH.OverloadedLabels
translateTHtoGHCExt TH.EmptyCase = GhcTH.EmptyCase
translateTHtoGHCExt TH.PatternSynonyms = GhcTH.PatternSynonyms
translateTHtoGHCExt TH.PartialTypeSignatures = GhcTH.PartialTypeSignatures
translateTHtoGHCExt TH.NamedWildCards = GhcTH.NamedWildCards
translateTHtoGHCExt TH.StaticPointers = GhcTH.StaticPointers
translateTHtoGHCExt TH.TypeApplications = GhcTH.TypeApplications
translateTHtoGHCExt TH.Strict = GhcTH.Strict
translateTHtoGHCExt TH.StrictData = GhcTH.StrictData
translateTHtoGHCExt TH.MonadFailDesugaring = GhcTH.MonadFailDesugaring
translateTHtoGHCExt TH.EmptyDataDeriving = GhcTH.EmptyDataDeriving
translateTHtoGHCExt TH.NumericUnderscores = GhcTH.NumericUnderscores
translateTHtoGHCExt TH.QuantifiedConstraints = GhcTH.QuantifiedConstraints
translateTHtoGHCExt TH.StarIsType = GhcTH.StarIsType

#if MIN_VERSION_ghc(8,10,0)
translateTHtoGHCExt TH.CUSKs = GhcTH.CUSKs
translateTHtoGHCExt TH.ImportQualifiedPost = GhcTH.ImportQualifiedPost
translateTHtoGHCExt TH.UnliftedNewtypes = GhcTH.UnliftedNewtypes
translateTHtoGHCExt TH.StandaloneKindSignatures = GhcTH.StandaloneKindSignatures
#endif
#if MIN_VERSION_ghc(9,0,0)
translateTHtoGHCExt TH.QualifiedDo = GhcTH.QualifiedDo
translateTHtoGHCExt TH.LinearTypes = GhcTH.LinearTypes
translateTHtoGHCExt TH.LexicalNegation = GhcTH.LexicalNegation
#endif
