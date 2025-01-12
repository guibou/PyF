{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module is here to parse Haskell expression using the GHC Api
module PyF.Internal.Parser (parseExpression) where

#if MIN_VERSION_ghc(9,6,0)
import GHC.Parser.Errors.Types (PsMessage)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Parser.Lexer (ParseResult (..), PState (..))
#elif MIN_VERSION_ghc(8,10,0)
import Lexer (ParseResult (..), PState (..))
#else
import Lexer (ParseResult (..))
#endif

#if MIN_VERSION_ghc(9,3,0)
import GHC.Types.Error
import GHC.Utils.Outputable
import GHC.Utils.Error
#endif

#if MIN_VERSION_ghc(9,4,0)
#elif MIN_VERSION_ghc(9,2,0)
import qualified GHC.Parser.Errors.Ppr as ParserErrorPpr
#endif

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Types.SrcLoc as SrcLoc
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Session (DynFlags)
import GHC.Types.SrcLoc
#else
import DynFlags (DynFlags)
import SrcLoc
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

parseExpression :: RealSrcLoc -> String -> DynFlags -> Either (RealSrcLoc, String) (HsExpr GhcPs)
parseExpression initLoc s dynFlags =
  case ParseExp.parseExpression initLoc s dynFlags of
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
    PFailed _ (SrcLoc.srcSpanEnd -> SrcLoc.RealSrcLoc srcLoc) doc ->
#endif

#if MIN_VERSION_ghc(9,7,0)
            let
                err = renderWithContext defaultSDocContext
                    $ vcat
                    $ map formatBulleted
                    $ map (\psMessage -> diagnosticMessage (defaultDiagnosticOpts @PsMessage) psMessage)
                    $ map errMsgDiagnostic
                    $ sortMsgBag Nothing
                    $ getMessages $ errorMessages
            in Left (srcLoc, err)
#elif MIN_VERSION_ghc(9,6,0)
            let
                err = renderWithContext defaultSDocContext
                    $ vcat
                    $ map (formatBulleted defaultSDocContext)
                    $ map (\psMessage -> diagnosticMessage (defaultDiagnosticOpts @PsMessage) psMessage)
                    $ map errMsgDiagnostic
                    $ sortMsgBag Nothing
                    $ getMessages $ errorMessages
            in Left (srcLoc, err)
#elif MIN_VERSION_ghc(9,3,0)
            let
                err = renderWithContext defaultSDocContext
                    $ vcat
                    $ map (formatBulleted defaultSDocContext)
                    $ map diagnosticMessage
                    $ map errMsgDiagnostic
                    $ sortMsgBag Nothing
                    $ getMessages $ errorMessages
            in Left (srcLoc, err)
#elif MIN_VERSION_ghc(9,2,0)
            let
                psErrToString e = show $ ParserErrorPpr.pprError e
                err = concatMap psErrToString errorMessages
            in Left (srcLoc, err)
#elif MIN_VERSION_ghc(8,10,0)
            let -- TODO: do not ignore "warnMessages"
                -- I have no idea what they can be
                (_warnMessages, errorMessages) = msgs dynFlags
                err = concatMap show errorMessages
            in Left (srcLoc, err)
#else
            let err = showSDoc dynFlags doc
            in Left (srcLoc, err)
#endif

#if MIN_VERSION_ghc(8,10,0)
#elif MIN_VERSION_ghc(8,6,0)
    -- Only here to satisfy GHC checker which was not able to consider this as total with GHC <8.10
    PFailed _ _ _ -> error "The impossible happen: this case is not possible"
#endif
