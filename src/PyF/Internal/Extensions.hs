module PyF.Internal.Extensions
  ( thExtToMetaExt,
  )
where

import qualified Language.Haskell.Exts.Extension as Exts
import qualified Language.Haskell.TH as TH

-- | Associate a template haskell extension to an haskell-src-ext extension
thExtToMetaExt :: TH.Extension -> Maybe Exts.Extension
thExtToMetaExt e = Exts.EnableExtension <$> case e of
  TH.Cpp -> Just Exts.CPP -- LOL, different case ;)
  TH.OverlappingInstances -> Just Exts.OverlappingInstances
  TH.UndecidableInstances -> Just Exts.UndecidableInstances
  TH.IncoherentInstances -> Just Exts.IncoherentInstances
  TH.MonomorphismRestriction -> Just Exts.MonomorphismRestriction
  TH.MonoPatBinds -> Just Exts.MonoPatBinds
  TH.MonoLocalBinds -> Just Exts.MonoLocalBinds
  TH.RelaxedPolyRec -> Just Exts.RelaxedPolyRec
  TH.ExtendedDefaultRules -> Just Exts.ExtendedDefaultRules
  TH.ForeignFunctionInterface -> Just Exts.ForeignFunctionInterface
  TH.UnliftedFFITypes -> Just Exts.UnliftedFFITypes
  TH.InterruptibleFFI -> Just Exts.InterruptibleFFI
  TH.CApiFFI -> Just Exts.CApiFFI
  TH.GHCForeignImportPrim -> Just Exts.GHCForeignImportPrim
  TH.JavaScriptFFI -> Just Exts.JavaScriptFFI
  TH.ParallelArrays -> Just Exts.ParallelArrays
  TH.Arrows -> Just Exts.Arrows
  TH.TemplateHaskell -> Just Exts.TemplateHaskell
  TH.QuasiQuotes -> Just Exts.QuasiQuotes
  TH.ImplicitParams -> Just Exts.ImplicitParams
  TH.ImplicitPrelude -> Just Exts.ImplicitPrelude
  TH.ScopedTypeVariables -> Just Exts.ScopedTypeVariables
  TH.UnboxedSums -> Just Exts.UnboxedSums
  TH.BangPatterns -> Just Exts.BangPatterns
  TH.TypeFamilies -> Just Exts.TypeFamilies
  TH.TypeFamilyDependencies -> Just Exts.TypeFamilyDependencies
  TH.OverloadedStrings -> Just Exts.OverloadedStrings
  TH.DisambiguateRecordFields -> Just Exts.DisambiguateRecordFields
  TH.RecordWildCards -> Just Exts.RecordWildCards
  TH.RecordPuns -> Just Exts.RecordPuns
  TH.ViewPatterns -> Just Exts.ViewPatterns
  TH.GADTs -> Just Exts.GADTs
  TH.NPlusKPatterns -> Just Exts.NPlusKPatterns
  TH.DoAndIfThenElse -> Just Exts.DoAndIfThenElse
  TH.RebindableSyntax -> Just Exts.RebindableSyntax
  TH.ConstraintKinds -> Just Exts.ConstraintKinds
  TH.PolyKinds -> Just Exts.PolyKinds
  TH.DataKinds -> Just Exts.DataKinds
  TH.InstanceSigs -> Just Exts.InstanceSigs
  TH.StandaloneDeriving -> Just Exts.StandaloneDeriving
  TH.DeriveDataTypeable -> Just Exts.DeriveDataTypeable
  TH.DeriveFunctor -> Just Exts.DeriveFunctor
  TH.DeriveTraversable -> Just Exts.DeriveTraversable
  TH.DeriveFoldable -> Just Exts.DeriveFoldable
  TH.DeriveGeneric -> Just Exts.DeriveGeneric
  TH.DefaultSignatures -> Just Exts.DefaultSignatures
  TH.DeriveAnyClass -> Just Exts.DeriveAnyClass
  TH.DerivingStrategies -> Just Exts.DerivingStrategies
  TH.TypeSynonymInstances -> Just Exts.TypeSynonymInstances
  TH.FlexibleContexts -> Just Exts.FlexibleContexts
  TH.FlexibleInstances -> Just Exts.FlexibleInstances
  TH.ConstrainedClassMethods -> Just Exts.ConstrainedClassMethods
  TH.MultiParamTypeClasses -> Just Exts.MultiParamTypeClasses
  TH.FunctionalDependencies -> Just Exts.FunctionalDependencies
  TH.UnicodeSyntax -> Just Exts.UnicodeSyntax
  TH.ExistentialQuantification -> Just Exts.ExistentialQuantification
  TH.MagicHash -> Just Exts.MagicHash
  TH.EmptyDataDecls -> Just Exts.EmptyDataDecls
  TH.KindSignatures -> Just Exts.KindSignatures
  TH.RoleAnnotations -> Just Exts.RoleAnnotations
  TH.ParallelListComp -> Just Exts.ParallelListComp
  TH.TransformListComp -> Just Exts.TransformListComp
  TH.GeneralizedNewtypeDeriving -> Just Exts.GeneralizedNewtypeDeriving
  TH.RecursiveDo -> Just Exts.RecursiveDo
  TH.PostfixOperators -> Just Exts.PostfixOperators
  TH.TupleSections -> Just Exts.TupleSections
  TH.PatternGuards -> Just Exts.PatternGuards
  TH.LiberalTypeSynonyms -> Just Exts.LiberalTypeSynonyms
  TH.RankNTypes -> Just Exts.RankNTypes
  TH.ImpredicativeTypes -> Just Exts.ImpredicativeTypes
  TH.TypeOperators -> Just Exts.TypeOperators
  TH.ExplicitNamespaces -> Just Exts.ExplicitNamespaces
  TH.PackageImports -> Just Exts.PackageImports
  TH.ExplicitForAll -> Just Exts.ExplicitForAll
  TH.DatatypeContexts -> Just Exts.DatatypeContexts
  TH.NondecreasingIndentation -> Just Exts.NondecreasingIndentation
  TH.LambdaCase -> Just Exts.LambdaCase
  TH.MultiWayIf -> Just Exts.MultiWayIf
  TH.BinaryLiterals -> Just Exts.BinaryLiterals
  TH.OverloadedLabels -> Just Exts.OverloadedLabels
  TH.EmptyCase -> Just Exts.EmptyCase
  TH.PatternSynonyms -> Just Exts.PatternSynonyms
  TH.PartialTypeSignatures -> Just Exts.PartialTypeSignatures
  TH.NamedWildCards -> Just Exts.NamedWildCards
  TH.TypeApplications -> Just Exts.TypeApplications
  -- Theses extensions have no associated extensions in haskell-src-exts
  TH.UndecidableSuperClasses -> Nothing
  TH.TemplateHaskellQuotes -> Nothing
  TH.AllowAmbiguousTypes -> Nothing
  TH.UnboxedTuples -> Nothing
  TH.TypeInType -> Nothing
  TH.OverloadedLists -> Nothing
  TH.NumDecimals -> Nothing
  TH.GADTSyntax -> Nothing
  TH.BlockArguments -> Nothing
  TH.ApplicativeDo -> Nothing
  TH.AutoDeriveTypeable -> Nothing
  TH.DeriveLift -> Nothing
  TH.DerivingVia -> Nothing
  TH.NullaryTypeClasses -> Nothing
  TH.MonadComprehensions -> Nothing
  TH.AlternativeLayoutRule -> Nothing
  TH.AlternativeLayoutRuleTransitional -> Nothing
  TH.RelaxedLayout -> Nothing
  TH.TraditionalRecordSyntax -> Nothing
  TH.NegativeLiterals -> Nothing
  TH.HexFloatLiterals -> Nothing
  TH.DuplicateRecordFields -> Nothing
  TH.StaticPointers -> Nothing
  TH.Strict -> Nothing
  TH.StrictData -> Nothing
  TH.MonadFailDesugaring -> Nothing
  TH.EmptyDataDeriving -> Nothing
  TH.NumericUnderscores -> Nothing
  TH.QuantifiedConstraints -> Nothing
  TH.StarIsType -> Nothing
