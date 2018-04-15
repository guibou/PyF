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

templateF :: String -> QuasiQuoter
templateF fName = QuasiQuoter {
    quoteExp = QQ.toExp
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error (fName ++ ": This QuasiQuoter can not be used as a " ++ name ++ "!")

-- | Returns an expression usable with Formatting.format (and similar functions)
f :: QuasiQuoter
f = templateF "f"

-- | Generic formatter, can format an expression to (lazy) Text, String, Builder and IO () depending on type inference
f' :: QuasiQuoter
f' = wrapQQ (templateF "f'") (VarE 'magicFormat)

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


-- | Format the format string and directly print it to stdout
fIO = wrapQQ (templateF "fIO") (VarE 'F.fprint)

-- | Format the format string as a 'String'
fString = wrapQQ (templateF "fString") (VarE 'F.formatToString)

-- | Format the format string as a strict 'SText.Text'
fStrictText = wrapQQ (templateF "fStrictTeext") (VarE 'F.sformat)

-- | Format the format string as a Lazy 'LText.Text'
fLazyText = wrapQQ (templateF "fLazy") (VarE 'F.sformat)

-- | Format the format string as a 'Builder.Builder'
fBuilder = wrapQQ (templateF "fBuilder") (VarE 'F.bprint)
