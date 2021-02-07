{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE CPP #-}
-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

-- Note: This code was mostly copied from:
-- https://github.com/shayne-fletcher/ghc-lib-parser-ex

module PyF.Internal.ParserEx (fakeSettings, fakeLlvmConfig, parseExpression, applyFixities, preludeFixities,baseFixities)
where

#      if __GLASGOW_HASKELL__   == 811
#        define GHCLIB_API_HEAD
#      elif __GLASGOW_HASKELL__ == 900
#        define GHCLIB_API_900
#      elif __GLASGOW_HASKELL__ == 810
#        define GHCLIB_API_810
#      elif __GLASGOW_HASKELL__ == 808
#        define GHCLIB_API_808
#      else
#        error Unsupported GHC API version
#      endif

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
import GHC.Settings.Config
import GHC.Driver.Session
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Settings
#elif defined (GHCLIB_API_810)
import Config
import DynFlags
import Fingerprint
import GHC.Platform
import ToolSettings
#else
import Config
import DynFlags
import Fingerprint
import Platform
#endif

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHCLIB_API_HEAD)
import GHC.Driver.Config
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
import GHC.Parser.PostProcess
import GHC.Driver.Session
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Parser as Parser
import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC.Driver.Backpack.Syntax
import GHC.Unit.Info
import GHC.Types.Name.Reader
#else
import StringBuffer
import Lexer
import qualified Parser
import FastString
import SrcLoc
import RdrName
#endif
#if defined (GHCLIB_API_810)
import RdrHsSyn
#endif

import Data.Data hiding (Fixity)

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
import GHC.Hs
#if defined (GHCLIB_API_HEAD)
import GHC.Types.Fixity
import GHC.Types.SourceText
#else
import GHC.Types.Basic
#endif
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.SrcLoc
#elif defined (GHCLIB_API_810)
import BasicTypes
import OccName
#else
import HsSyn
import BasicTypes
import RdrName
import OccName
import SrcLoc
#endif

import Data.Generics.Uniplate.Data
import Data.Maybe

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)|| defined (GHCLIB_API_810)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sPlatformConstants=platformConstants
  , sToolSettings=toolSettings
  }
#else
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  }
#endif
  where
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)|| defined (GHCLIB_API_810)
    toolSettings = ToolSettings {
      toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion =
      GhcNameVersion{ghcNameVersion_programName="ghc"
                    ,ghcNameVersion_projectVersion=cProjectVersion
                    }
#endif
    platform =
      Platform{
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
    -- It doesn't matter what values we write here as these fields are
    -- not referenced for our purposes. However the fields are strict
    -- so we must say something.
        platformByteOrder=LittleEndian
      , platformHasGnuNonexecStack=True
      , platformHasIdentDirective=False
      , platformHasSubsectionsViaSymbols=False
      , platformIsCrossCompiling=False
      , platformLeadingUnderscore=False
      , platformTablesNextToCode=False
#  if !defined (GHCLIB_API_900)
      , platformConstants=platformConstants
#  endif
      ,
#endif
#if defined (GHCLIB_API_HEAD)
        platformWordSize=PW8
      , platformArchOS=ArchOS {archOS_arch=ArchUnknown, archOS_OS=OSUnknown}
#elif defined (GHCLIB_API_810) || defined (GHCLIB_API_900)
        platformWordSize=PW8
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
#else
        platformWordSize=8
      , platformOS=OSUnknown
#endif
      , platformUnregisterised=True
      }
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)|| defined (GHCLIB_API_810)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#else
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#endif


-- From Language.Haskell.GhclibParserEx.GHC.Parser

parse :: P a -> String -> DynFlags -> ParseResult a
parse p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    buffer = stringToStringBuffer str
    parseState =
#if defined (GHCLIB_API_HEAD)
      initParserState (initParserOpts flags) buffer location
#else
      mkPState flags buffer location
#endif


parseExpression :: String -> DynFlags -> ParseResult (LHsExpr GhcPs)
#if defined (GHCLIB_API_HEAD)
parseExpression s flags =
  case parse Parser.parseExpression s flags of
    POk s e -> unP (runPV . unECP $ e) s
    PFailed ps -> PFailed ps
#elif defined (GHCLIB_API_810) || defined (GHCLIB_API_900)
parseExpression s flags =
  case parse Parser.parseExpression s flags of
    POk s e -> unP (runECP_P e) s
    PFailed ps -> PFailed ps
#else
parseExpression = parse Parser.parseExpression
#endif


-- from Language.Haskell.GhclibParserEx.Fixity.html

-- | Rearrange a parse tree to account for fixities.
applyFixities :: Data a => [(String, Fixity)] -> a -> a
applyFixities fixities m =
  let m'  = transformBi (expFix fixities) m
      m'' = transformBi (patFix fixities) m'
  in m''

preludeFixities :: [(String, Fixity)]
preludeFixities = concat
    [ infixr_ 9  ["."]
    , infixl_ 9  ["!!"]
    , infixr_ 8  ["^","^^","**"]
    , infixl_ 7  ["*","/","quot","rem","div","mod",":%","%"]
    , infixl_ 6  ["+","-"]
    , infixr_ 5  [":","++"]
    , infix_  4  ["==","/=","<","<=",">=",">","elem","notElem"]
    , infixr_ 3  ["&&"]
    , infixr_ 2  ["||"]
    , infixl_ 1  [">>",">>="]
    , infixr_ 1  ["=<<"]
    , infixr_ 0  ["$","$!","seq"]
    ]

-- All fixities defined in the base package. Note that the @+++@
-- operator appears in both Control.Arrows and
-- Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
-- this list is that of Control.Arrows.
baseFixities :: [(String, Fixity)]
baseFixities = preludeFixities ++ concat
    [ infixr_ 9 ["Compose"]
    , infixl_ 9 ["!","//","!:"]
    , infixl_ 8 ["shift","rotate","shiftL","shiftR","rotateL","rotateR"]
    , infixl_ 7 [".&."]
    , infixl_ 6 ["xor"]
    , infix_  6 [":+"]
    , infixr_ 6 ["<>"]
    , infixl_ 5 [".|."]
    , infixr_ 5 ["+:+","<++","<+>","<|"] -- Fixity conflict for +++ between ReadP and Arrow.
    , infix_  5 ["\\\\"]
    , infixl_ 4 ["<$>","<$","$>","<*>","<*","*>","<**>","<$!>"]
    , infix_  4 ["elemP","notElemP",":~:", ":~~:"]
    , infixl_ 3 ["<|>"]
    , infixr_ 3 ["&&&","***"]
    , infixr_ 2 ["+++","|||"]
    , infixr_ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    , infixl_ 0 ["on"]
    , infixr_ 0 ["par","pseq"]
    ]

infixr_, infixl_, infix_ :: Int -> [String] -> [(String,Fixity)]
infixr_ = fixity InfixR
infixl_ = fixity InfixL
infix_  = fixity InfixN

fixity :: FixityDirection -> Int -> [String] -> [(String, Fixity)]
fixity a p = map (,Fixity (SourceText "") p a)

expFix :: [(String, Fixity)] -> LHsExpr GhcPs -> LHsExpr GhcPs
expFix fixities (L loc (OpApp _ l op r)) =
  mkOpApp (getFixities fixities) loc l op (findFixity (getFixities fixities) op) r
expFix _ e = e

-- LPat and Pat have gone through a lot of churn. See
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/1925 for details.
patFix :: [(String, Fixity)] -> LPat GhcPs -> LPat GhcPs
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
patFix fixities (L loc (ConPat _ op (InfixCon pat1 pat2))) =
  L loc (mkConOpPat (getFixities fixities) op (findFixity' (getFixities fixities) op) pat1 pat2)
#elif defined (GHCLIB_API_810)
patFix fixities (L loc (ConPatIn op (InfixCon pat1 pat2))) =
  L loc (mkConOpPat (getFixities fixities) op (findFixity' (getFixities fixities) op) pat1 pat2)
#else
patFix fixities (dL -> L _ (ConPatIn op (InfixCon pat1 pat2))) =
  mkConOpPat (getFixities fixities) op (findFixity' (getFixities fixities) op) pat1 pat2
#endif
patFix _ p = p

mkConOpPat ::
  [(String, Fixity)]
  -> Located RdrName -> Fixity
  -> LPat GhcPs -> LPat GhcPs
  -> Pat GhcPs
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
mkConOpPat fs op2 fix2 p1@(L loc (ConPat _ op1 (InfixCon p11 p12))) p2
#elif defined (GHCLIB_API_810)
mkConOpPat fs op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
#else
mkConOpPat fs op2 fix2 p1@(dL->L loc (ConPatIn op1 (InfixCon p11 p12))) p2
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
  | nofix_error = ConPat noExtField op2 (InfixCon p1 p2)
#else
  | nofix_error = ConPatIn op2 (InfixCon p1 p2)
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
  | associate_right = ConPat noExtField op1 (InfixCon p11 (L loc (mkConOpPat fs op2 fix2 p12 p2)))
#elif defined (GHCLIB_API_810)
  | associate_right = ConPatIn op1 (InfixCon p11 (L loc (mkConOpPat fs op2 fix2 p12 p2)))
#else
  | associate_right = ConPatIn op1 (InfixCon p11 (cL loc (mkConOpPat fs op2 fix2 p12 p2)))
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
  | otherwise = ConPat noExtField op2 (InfixCon p1 p2)
#else
  | otherwise = ConPatIn op2 (InfixCon p1 p2)
#endif
  where
    fix1 = findFixity' fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900)
mkConOpPat _ op _ p1 p2 = ConPat noExtField op (InfixCon p1 p2)
#else
mkConOpPat _ op _ p1 p2 = ConPatIn op (InfixCon p1 p2)
#endif

mkOpApp ::
  [(String, Fixity)]
  -> SrcSpan
  -> LHsExpr GhcPs -- Left operand; already rearrange.
  -> LHsExpr GhcPs -> Fixity -- Operator and fixity.
  -> LHsExpr GhcPs -- Right operand (not an OpApp, but might be a NegApp).
  -> LHsExpr GhcPs
--      (e11 `op1` e12) `op2` e2
mkOpApp fs loc e1@(L _ (OpApp x1 e11 op1 e12)) op2 fix2 e2
  | nofix_error = L loc (OpApp noExt e1 op2 e2)
  | associate_right = L loc (OpApp x1 e11 op1 (mkOpApp fs loc' e12 op2 fix2 e2 ))
  where
    loc'= combineLocs e12 e2
    fix1 = findFixity fs op1
    (nofix_error, associate_right) = compareFixity fix1 fix2
--      (- neg_arg) `op` e2
mkOpApp fs loc e1@(L _ (NegApp _ neg_arg neg_name)) op2 fix2 e2
  | nofix_error = L loc (OpApp noExt e1 op2 e2)
  | associate_right = L loc (NegApp noExt (mkOpApp fs loc' neg_arg op2 fix2 e2) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2
--      e1 `op` - neg_arg
mkOpApp _ loc e1 op1 fix1 e2@(L _ NegApp {}) -- NegApp can occur on the right.
  | not associate_right  = L loc (OpApp noExt e1 op1 e2)-- We *want* right association.
  where
    (_, associate_right) = compareFixity fix1 negateFixity
 --     Default case, no rearrangment.
mkOpApp _ loc e1 op _fix e2 = L loc (OpApp noExt e1 op e2)

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
noExt :: NoExtField
noExt = noExtField
#endif

findFixity' :: [(String, Fixity)] -> Located RdrName -> Fixity
findFixity' fs r = askFix fs (occNameString . rdrNameOcc . unLoc $ r) -- Patterns.

askFix :: [(String, Fixity)] -> String -> Fixity
askFix xs = \k -> lookupWithDefault defaultFixity k xs
  where lookupWithDefault def_v k mp1 = fromMaybe def_v $ lookup k mp1

-- If there are no fixities, give 'baseFixities'.
getFixities :: [(String, Fixity)] -> [(String, Fixity)]
getFixities fixities = if null fixities then baseFixities else fixities

findFixity :: [(String, Fixity)] -> LHsExpr GhcPs -> Fixity
findFixity fs r = askFix fs (getIdent r) -- Expressions.

getIdent :: LHsExpr GhcPs -> String
getIdent (unLoc -> HsVar _ (L _ n)) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"
