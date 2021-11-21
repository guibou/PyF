{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module SpecUtils
  ( checkExample,
    checkExampleDiff,
  )
where

import Language.Haskell.TH
#ifdef PYTHON_TEST
import Language.Haskell.TH.Syntax
import System.Exit
import System.Process
#endif

import PyF (fmtConfig)
import PyF.Internal.QQ
import Test.Hspec

-- * Utils

#ifdef PYTHON_TEST
-- | Runs a python formatter example
--
-- For conveniance, it exports a few python symbols, `inf`, `nan` and pi.
--
-- >>> runPythonExample "{3.14159:.1f}
-- "3.1
runPythonExample :: String -> IO (Maybe String)
runPythonExample s = do
  let pythonPath = "python3"
      args = ["-c", "from math import pi;nan = float('NaN');inf = float('inf');print(f\'''" ++ s ++ "''', end='')"]
  (ecode, stdout, _stderr) <- readProcessWithExitCode pythonPath args ""
  pure $ case ecode of
    ExitSuccess -> Just stdout
    ExitFailure _ -> Nothing

-- | `pyCheck formatString reference` compares a format string against
-- a reference (if `Just`) and against the python implementation
--
-- This TH expression will return an expression compatible with `Hspec` `SpecM`.
--
-- This expression is a failure if python cannot format this formatString
-- or if the python result does not match the (provided) reference.
pyCheck :: String -> Maybe String -> Q Exp
pyCheck s exampleStr = do
  pythonRes <- Language.Haskell.TH.Syntax.runIO (runPythonExample s)
  case pythonRes of
    Nothing -> [|expectationFailure $ "Expression: `" ++ s ++ "` fails in python"|]
    Just res -> do
      let qexp = [|$(toExp fmtConfig s) `shouldBe` res|]
      case exampleStr of
        Nothing -> qexp
        Just e ->
          if res == e
            then qexp
            else [|expectationFailure $ "Provided result `" ++ e ++ "` does not match the python result `" ++ res ++ "`"|]

-- * Exported

-- | `checkExample formatString result` checks if, once formated,
--     `formatString` is equal to result. It also checks that the result is
--     the same as the one provided by python.
checkExample :: String -> String -> Q Exp
checkExample s res = pyCheck s (Just res)
#else
-- | Alias for checkExampleDiff
checkExample :: String -> String -> Q Exp
checkExample = checkExampleDiff
#endif

-- | `checkExampleDiff formatString result` checks if, once formated,
--     `formatString` is equal to result. It does not check the result
--     against the python implementation
checkExampleDiff :: String -> String -> Q Exp
checkExampleDiff s res = [|$(toExp fmtConfig s) `shouldBe` res|]
