{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{- | A lot of quasiquoters to format and interpolate string expression
-}
module PyF
  ( f
   -- * With custom delimiters
  , fWithDelimiters
  )
where

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified PyF.Internal.QQ as QQ

templateF :: (Char, Char) -> String -> QuasiQuoter
templateF delimiters fName = QuasiQuoter {
    quoteExp = \s -> (QQ.toExp delimiters s)
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error (fName ++ ": This QuasiQuoter can not be used as a " ++ name ++ "!")

-- | Generic formatter, can format an expression to any @t@ as long as
--   @t@ is an instance of 'IsString'.
f :: QuasiQuoter
f = templateF pythonDelimiters "f"

fWithDelimiters :: (Char, Char) -> QuasiQuoter
fWithDelimiters delimiters = templateF delimiters "fWithDelimiters"

pythonDelimiters :: (Char, Char)
pythonDelimiters = ('{', '}')