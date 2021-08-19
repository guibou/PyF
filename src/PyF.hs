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
    raw,
  )
where

import Data.Char (isSpace)
import Data.List (intercalate)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import PyF.Class
import PyF.Internal.QQ (toExp)
import Language.Haskell.TH

expQQ :: String -> (String -> Q Exp) -> QuasiQuoter
expQQ fName qExp = QuasiQuoter
    { quoteExp = qExp,
      quotePat = err "pattern",
      quoteType = err "type",
      quoteDec = err "declaration"
    }
  where
    err name = error (fName ++ ": This QuasiQuoter can not be used as a " ++ name ++ "!")

-- | Generic formatter, can format an expression to any @t@ as long as
--   @t@ is an instance of 'IsString'.
fmt :: QuasiQuoter
fmt = expQQ "fmt" (toExp pythonDelimiters)

fmtWithDelimiters :: (Char, Char) -> QuasiQuoter
fmtWithDelimiters delimiters = expQQ "fmtWithDelimiters" (toExp delimiters)

pythonDelimiters :: (Char, Char)
pythonDelimiters = ('{', '}')

raw :: QuasiQuoter
raw = expQQ "raw" (\s -> [| s |])

-- | Removes the trailing whitespace of a string.
--
-- - First line is ignored if it only contains whitespaces
-- - All other line common indentation is removed, ignoring line with only whitespaces.
--
-- >>> trimIndent "\n   hello\n   - a\n   - b\n   "
-- "hello\n- a\n- b\n"
--
-- See 'PyF.Trimmed.fmt' in the @PyF.Trimmed@ module for a quasiquoter with this behavior
trimIndent :: String -> String
trimIndent s =
  case lines s of
    [] -> ""
    [_] -> s
    (firstLine : others) ->
      let
          -- Discard the first line if needed
          usedLines
            | all isSpace firstLine = others ++ trail
            | otherwise = firstLine : others ++ trail

          -- If the string ends with a newline, `lines` will discard it. We restore it.
          trail
            | last s == '\n' = [""]
            | otherwise = []
          -- Find the minimum indent common to all lines
          biggestLines = map (length . takeWhile isSpace) (filter (not . all isSpace) usedLines)

          stripLen = case biggestLines of
            [] -> 0
            _ -> minimum biggestLines

          -- drop them
          trimmedLines = map (drop stripLen) usedLines
       in intercalate "\n" trimmedLines
