{-# LANGUAGE TemplateHaskell #-}

-- | This modules behaves as 'PyF', but the formatters 'fmt' and
-- do whitespace trimming as defined in the 'PyF.trimIndent'
-- function.
module PyF.Trimmed
  ( fmt,

    module PyF.Class,

    -- * Whitespace utilities
    PyF.trimIndent,
  )
where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified PyF
import qualified PyF.Class ()
import PyF.Internal.QQ
import PyF (defaultConfig, mkFormatter)

fmt :: QuasiQuoter
fmt = mkFormatter "fmt" trimConfig

trimConfig :: Config
trimConfig = defaultConfig {
  postProcess = \q -> wrapFromString [| PyF.trimIndent $(q) |]
                           }
