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

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified PyF
import qualified PyF.Class ()

trimQQ qq = qq {quoteExp = \s -> [|trimIndent $(quoteExp qq s)|]}

fmt :: QuasiQuoter
fmt = trimQQ PyF.fmt

fmtWithDelimiters :: (Char, Char) -> QuasiQuoter
fmtWithDelimiters delimiters = trimQQ (PyF.fmtWithDelimiters delimiters)
