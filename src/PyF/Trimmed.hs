{-# LANGUAGE TemplateHaskell #-}

-- | This modules behaves as 'PyF', but the formatters 'fmt' and
-- 'fmtWithDelimiters' do whitespace trimming as defined in the 'PyF.trimIndent'
-- function.
module PyF.Trimmed
  ( fmt,

    -- * With custom delimiters
    fmtWithDelimiters,
    module PyF.Class,

    -- * Whitespace utilities
    PyF.trimIndent,
  )
where

import Data.String
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified PyF
import qualified PyF.Class ()
import Language.Haskell.TH

trimQQ :: QuasiQuoter -> QuasiQuoter
trimQQ qq =
  qq
    { quoteExp = \s -> do
        exts <- extsEnabled
        if OverloadedStrings `elem` exts
          then [|Data.String.fromString $ PyF.trimIndent $(quoteExp qq s)|]
          else [|PyF.trimIndent $(quoteExp qq s)|]
    }

fmt :: QuasiQuoter
fmt = trimQQ PyF.fmt

fmtWithDelimiters :: (Char, Char) -> QuasiQuoter
fmtWithDelimiters delimiters = trimQQ (PyF.fmtWithDelimiters delimiters)
