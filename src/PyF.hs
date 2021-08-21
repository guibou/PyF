{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | A lot of quasiquoters to format and interpolate string expression
module PyF
  ( fmt,
    fmtTrim,
    str,
    strTrim,
    raw,
    module PyF.Class,

    -- * Whitespace utilities
    trimIndent,

    -- * Configuration
    mkFormatter,
    defaultConfig,
    fmtConfig,
    strConfig,
    addTrim,
    addFormatting,
  )
where

import Data.Char (isSpace)
import Data.List (intercalate)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import PyF.Class
import PyF.Internal.QQ (Config (..), expQQ, toExp, wrapFromString)

-- | Generic formatter, can format an expression to any @t@ as long as
--   @t@ is an instance of 'IsString'.
fmt :: QuasiQuoter
fmt = mkFormatter "fmt" fmtConfig

-- | Format with whitespace trimming.
fmtTrim :: QuasiQuoter
fmtTrim = mkFormatter "fmtTrim" (addTrim fmtConfig)

-- | multiline string, no interpolation.
str :: QuasiQuoter
str = mkFormatter "str" strConfig

-- | multiline string, no interpolation, but does indentation trimming.
strTrim :: QuasiQuoter
strTrim = mkFormatter "strTrim" (addTrim strConfig)

-- | Raw string, no interpolation neither escaping is performed.
raw :: QuasiQuoter
raw = expQQ "raw" (\s -> [|s|])

-- | Removes the trailing whitespace of a string.
--
-- - First line is ignored if it only contains whitespaces
-- - All other line common indentation is removed, ignoring line with only whitespaces.
--
-- >>> trimIndent "\n   hello\n   - a\n   - b\n   "
-- "hello\n- a\n- b\n"
--
-- See 'fmtTrim' for a quasiquoter with this behavior
trimIndent :: String -> String
trimIndent s =
  case lines s of
    [] -> ""
    [_] -> s
    (firstLine : others) ->
      let -- Discard the first line if needed
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

-- | This is an empty configuration. No formatting, no post processing
defaultConfig :: Config
defaultConfig =
  Config
    { delimiters = Nothing,
      postProcess = id
    }

-- | Configuration for 'str' it just wrap the multiline string with 'fromString'.
strConfig :: Config
strConfig =
  Config
    { delimiters = Nothing,
      postProcess = wrapFromString
    }

-- | The config for 'fmt'.
fmtConfig :: Config
fmtConfig = addFormatting ('{', '}') strConfig

-- | Add indentation trimming to a configuration.
addTrim :: Config -> Config
addTrim config =
  config
    { postProcess = \q -> postProcess config [|PyF.trimIndent $(q)|]
    }

-- | Enable formatting
addFormatting :: (Char, Char) -> Config -> Config
addFormatting delims c = c {delimiters = Just delims}

-- | Build a formatter. See the 'Config' type for details, as well as
-- 'fmtConfig' and 'strConfig' for examples.
mkFormatter :: String -> Config -> QuasiQuoter
mkFormatter name config = expQQ name (toExp config)
