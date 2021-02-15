{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | A lot of quasiquoters to format and interpolate string expression
module PyF
  ( fmt,

    -- * With custom delimiters
    fmtWithDelimiters,
    module PyF.Class,

    -- * Whitespace utilities
    trimIndent,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import PyF.Class
import PyF.Internal.QQ (toExp)

templateF :: (Char, Char) -> String -> QuasiQuoter
templateF delimiters fName =
  QuasiQuoter
    { quoteExp = toExp delimiters,
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

-- | Removes the trailing whitespace of a string.
-- It follows https://www.python.org/dev/peps/pep-0257/#id18 meaning that:
--
-- - First line indentation is ignored
-- - All other line common indentation is removed
-- - White lines at beginning or end are discarded
--
-- >>> trimIndent "   hello\n   - a\n   - b\n    "
-- "hello\n- a\n- b"
--
-- See 'PyF.Trimmed.fmt' in the @PyF.Trimmed@ module for a quasiquoter with this behavior
trimIndent :: String -> String
trimIndent s =
  case lines s of
    [] -> ""
    -- Strip whitespace on the first line
    ((dropWhile isSpace -> firstLine) : others) ->
      -- Find the minimum indent common to all lines
      let biggestLines = map (length . takeWhile isSpace) (filter (not . all isSpace) others)
          stripLen = minimum (filter (/= 0) biggestLines)

          -- drop them
          trimmedLines = map (drop stripLen) others

          -- trim empty lines at beginning or end
          trimIndentLines = dropWhile (all isSpace) . dropWhileEnd (all isSpace) $ (firstLine : trimmedLines)
       in intercalate "\n" trimIndentLines
