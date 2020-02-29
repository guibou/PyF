{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | A lot of quasiquoters to format and interpolate string expression
module PyF
  ( fmt,

    -- * With custom delimiters
    fmtWithDelimiters,
    module PyF.Class,
  )
where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import PyF.Class
import PyF.Internal.QQ (toExp)

templateF :: (Char, Char) -> String -> QuasiQuoter
templateF delimiters fName = QuasiQuoter
  { quoteExp = \s -> (toExp delimiters s),
    quotePat = err "pattern",
    quoteType = err "type",
    quoteDec = err "declaration"
  }
  where
    err name = error (fName ++ ": This QuasiQuoter can not be used as a " ++ name ++ "!")

-- | Generic formatter, can format an expression to any @t@ as long as
--   @t@ is an instance of 'IsString'.
fmt :: QuasiQuoter
fmt = templateF pythonDelimiters "fmt"

fmtWithDelimiters :: (Char, Char) -> QuasiQuoter
fmtWithDelimiters delimiters = templateF delimiters "fmtWithDelimiters"

pythonDelimiters :: (Char, Char)
pythonDelimiters = ('{', '}')
