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
#endif

import qualified PyF.Internal.ParserEx as ParseExp

parseExpression :: String -> DynFlags -> Either (Int, Int) (HsExpr GhcPs)
parseExpression s dynFlags =
  case ParseExp.parseExpression s dynFlags of
    POk _ locatedExpr ->
      let expr = SrcLoc.unLoc locatedExpr
       in Right
            expr
#if MIN_VERSION_ghc(9,0,0)
    PFailed PState{loc=SrcLoc.psRealLoc -> srcLoc} ->
#elif MIN_VERSION_ghc(8,10,0)
    PFailed PState{loc=srcLoc} ->
#else
    -- TODO: check for pattern failure
    PFailed _ (SrcLoc.srcSpanEnd -> SrcLoc.RealSrcLoc srcLoc) _ ->
#endif
            let line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
             in Left (line, col)
