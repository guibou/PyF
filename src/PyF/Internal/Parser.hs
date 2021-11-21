{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module is here to parse Haskell expression using the GHC Api
module PyF.Internal.Parser (parseExpression) where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Parser.Lexer (ParseResult (..), PState (..))
#elif MIN_VERSION_ghc(8,10,0)
import Lexer (ParseResult (..), PState (..))
#else
import Lexer (ParseResult (..))
#endif

#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Parser.Errors.Ppr as ParserErrorPpr
#endif

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Types.SrcLoc as SrcLoc
#else
import qualified SrcLoc
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Session (DynFlags)
#else
import DynFlags (DynFlags)
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Expr as Expr
import GHC.Hs.Extension as Ext
#else
import HsExpr as Expr
import HsExtension as Ext
import Outputable (showSDoc)
#endif

import qualified PyF.Internal.ParserEx as ParseExp

parseExpression :: String -> DynFlags -> Either (Int, Int, String) (HsExpr GhcPs)
parseExpression s dynFlags =
  case ParseExp.parseExpression s dynFlags of
    POk _ locatedExpr ->
      let expr = SrcLoc.unLoc locatedExpr
       in Right
            expr

{- ORMOLU_DISABLE #-}
#if MIN_VERSION_ghc(9,2,0)
    -- TODO messages?
    PFailed PState{loc=SrcLoc.psRealLoc -> srcLoc, errors=errorMessages} ->
#elif MIN_VERSION_ghc(9,0,0)
    PFailed PState{loc=SrcLoc.psRealLoc -> srcLoc, messages=msgs} ->
#elif MIN_VERSION_ghc(8,10,0)
    PFailed PState{loc=srcLoc, messages=msgs} ->
#else
    -- TODO: check for pattern failure
    PFailed _ (SrcLoc.srcSpanEnd -> SrcLoc.RealSrcLoc srcLoc) doc ->
#endif
#if MIN_VERSION_ghc(9,2,0)
            let
                psErrToString e = show $ ParserErrorPpr.pprError e
                err = concatMap psErrToString errorMessages
                -- err = concatMap show errorMessages
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#elif MIN_VERSION_ghc(8,10,0)
            let -- TODO: do not ignore "warnMessages"
                -- I have no idea what they can be
                (_warnMessages, errorMessages) = msgs dynFlags
                err = concatMap show errorMessages
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#else
            let err = showSDoc dynFlags doc
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#endif
