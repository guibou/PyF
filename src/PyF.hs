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

import           Language.Haskell.TH

import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text as SText
import qualified Data.Text.Lazy.Builder as Builder

templateF :: (Char, Char) -> String -> QuasiQuoter
templateF delimiters fName = QuasiQuoter {
    quoteExp = \s -> (AppE (VarE 'magicFormat)) <$> (QQ.toExp delimiters s)
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error (fName ++ ": This QuasiQuoter can not be used as a " ++ name ++ "!")

-- | Generic formatter, can format an expression to (lazy) Text, String, Builder and IO () depending on type inference
f :: QuasiQuoter
f = templateF pythonDelimiters "f"

fWithDelimiters :: (Char, Char) -> QuasiQuoter
fWithDelimiters delimiters = templateF delimiters "fWithDelimiters"

class MagicFormat t where
  magicFormat :: Builder.Builder -> t

instance MagicFormat (IO ()) where
  magicFormat = LText.putStrLn . Builder.toLazyText

instance MagicFormat [Char] where
  magicFormat = LText.unpack . Builder.toLazyText

instance MagicFormat SText.Text where
  magicFormat = LText.toStrict . Builder.toLazyText

instance MagicFormat LText.Text where
  magicFormat = Builder.toLazyText

instance MagicFormat Builder.Builder where
  magicFormat = id

pythonDelimiters :: (Char, Char)
pythonDelimiters = ('{', '}')