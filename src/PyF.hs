{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{- | A lot of quasiquoters to format and interpolate string expression
-}
module PyF
  (f,
   f',
   fIO,
   fString,
   fBuilder,
   fLazyText,
   fStrictText,

   -- * With custom delimiters
   fWithDelimiters,
   f'WithDelimiters,
   fIOWithDelimiters,
   fStringWithDelimiters,
   fBuilderWithDelimiters,
   fLazyTextWithDelimiters,
   fStrictTextWithDelimiters,

   -- * Formatting re-export
   runFormat,
   format,
   sformat,
   bprint,
   fprint,
   hprint)
where

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified PyF.Internal.QQ as QQ

import Formatting (runFormat, format, sformat, bprint, fprint, hprint)
import qualified Formatting as F
import           Language.Haskell.TH

import qualified Data.Text.Lazy as LText
import qualified Data.Text as SText
import qualified Data.Text.Lazy.Builder as Builder

templateF :: (Char, Char) -> String -> QuasiQuoter
templateF delimiters fName = QuasiQuoter {
    quoteExp = QQ.toExp delimiters
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error (fName ++ ": This QuasiQuoter can not be used as a " ++ name ++ "!")

-- | Returns an expression usable with Formatting.format (and similar functions)
f :: QuasiQuoter
f = templateF pythonDelimiters "f"

fWithDelimiters :: (Char, Char) -> QuasiQuoter
fWithDelimiters delimiters = templateF delimiters "fWithDelimiters"

-- | Generic formatter, can format an expression to (lazy) Text, String, Builder and IO () depending on type inference
f' :: QuasiQuoter
f' = wrapQQ (templateF pythonDelimiters "f'") (VarE 'magicFormat)

f'WithDelimiters :: (Char, Char) -> QuasiQuoter
f'WithDelimiters delimiters = templateF delimiters "f'WithDelimiters"

wrapQQ :: QuasiQuoter -> Exp -> QuasiQuoter
wrapQQ qq wrap = qq {
  quoteExp = \s -> do
      e <- quoteExp qq s
      pure (AppE wrap e)
  }

class MagicFormat t where
  magicFormat :: F.Format t t -> t

instance MagicFormat (IO ()) where
  magicFormat = F.fprint

instance MagicFormat [Char] where
  magicFormat = F.formatToString

instance MagicFormat SText.Text where
  magicFormat = F.sformat

instance MagicFormat LText.Text where
  magicFormat = F.format

instance MagicFormat Builder.Builder where
  magicFormat = F.bprint

-- Monomorphic formatters
fIO, fString, fStrictText, fLazyText, fBuilder :: QuasiQuoter
fIOWithDelimiters, fStringWithDelimiters, fStrictTextWithDelimiters, fLazyTextWithDelimiters, fBuilderWithDelimiters :: (Char, Char) -> QuasiQuoter

fIO = fIOWithDelimiters pythonDelimiters
fString = fStringWithDelimiters pythonDelimiters
fStrictText = fStrictTextWithDelimiters pythonDelimiters
fLazyText = fLazyTextWithDelimiters pythonDelimiters
fBuilder = fBuilderWithDelimiters pythonDelimiters

pythonDelimiters :: (Char, Char)
pythonDelimiters = ('{', '}')

-- | Format the format string and directly print it to stdout
fIOWithDelimiters delimiters = wrapQQ (templateF delimiters "fIO") (VarE 'F.fprint)

-- | Format the format string as a 'String'
fStringWithDelimiters delimiters = wrapQQ (templateF delimiters "fString") (VarE 'F.formatToString)

-- | Format the format string as a strict 'SText.Text'
fStrictTextWithDelimiters delimiters = wrapQQ (templateF delimiters "fStrictTeext") (VarE 'F.sformat)

-- | Format the format string as a Lazy 'LText.Text'
fLazyTextWithDelimiters delimiters = wrapQQ (templateF delimiters "fLazy") (VarE 'F.sformat)

-- | Format the format string as a 'Builder.Builder'
fBuilderWithDelimiters delimiters = wrapQQ (templateF delimiters "fBuilder") (VarE 'F.bprint)
