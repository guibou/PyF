{-# LANGUAGE TemplateHaskell #-}
module PyF
  (f)
where

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified PyF.Internal.QQ as QQ
import qualified Formatting as F
import           Language.Haskell.TH

f :: QuasiQuoter
f = QuasiQuoter {
    quoteExp = \s -> do
        e <- QQ.toExp s
        pure (AppE (VarE 'F.format) e)
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("Data.String.Interpolate.i: This QuasiQuoter can not be used as a " ++ name ++ "!")
