{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | A lot of quasiquoters to format and interpolate string expression
module PyF
  ( fmt,
    fmtTrim,

    module PyF.Class,

    -- * Whitespace utilities
    trimIndent,
    raw,

    defaultConfig,
    mkFormatter,
  )
where

import Data.Char (isSpace)
import Data.List (intercalate)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import PyF.Class
import PyF.Internal.QQ (toExp, Config(..), wrapFromString, expQQ)

-- | Generic formatter, can format an expression to any @t@ as long as
--   @t@ is an instance of 'IsString'.
fmt :: QuasiQuoter
fmt = mkFormatter "fmt" defaultConfig

-- | Format with whitespace trimming.
fmtTrim :: QuasiQuoter
fmtTrim = mkFormatter "fmtTrim" trimConfig

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

-- | This is the config for 'fmt'
defaultConfig :: Config
defaultConfig =
  Config
    { delimiters = ('{', '}'),
      postProcess = wrapFromString
    }

trimConfig :: Config
trimConfig = defaultConfig {
  postProcess = \q -> wrapFromString [| PyF.trimIndent $(q) |]
                           }

-- | Build a formatter. See the 'Config' type for details.
mkFormatter :: String -> Config -> QuasiQuoter
mkFormatter name config = expQQ name (toExp config)
