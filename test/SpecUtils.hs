{-# LANGUAGE TemplateHaskell #-}
module SpecUtils
  ( checkExample
  , checkExampleDiff
  , check
  , checkCompile
  , CompilationStatus(..)
)
where

import Test.Hspec
import PyF.Internal.QQ

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import System.Process (readProcessWithExitCode)
import System.Exit

import System.IO.Temp
import qualified System.IO as IO

-- * Utils

{- | Runs a python formatter example

For conveniance, it exports a few python symbols, `inf`, `nan` and pi.

>>> runPythonExample "{3.14159:.1f}
"3.1
-}
runPythonExample :: String -> IO (Maybe String)
runPythonExample s = do
  let
    pythonPath = "python"
    args = ["-c", "from math import pi;nan = float('NaN');inf = float('inf');print(f\'''" ++ s ++ "''', end='')"]
  (ecode, stdout, _stderr) <- readProcessWithExitCode pythonPath args ""
  pure $ case ecode of
    ExitSuccess -> Just stdout
    ExitFailure _ -> Nothing

{- | `pyCheck formatString reference` compares a format string against
a reference (if `Just`) and against the python implementation

This TH expression will return an expression compatible with `Hspec` `SpecM`.

This expression is a failure if python cannot format this formatString
or if the python result does not match the (provided) reference.
-}

pyCheck :: String -> Maybe String -> Q Exp
pyCheck s exampleStr = do
  pythonRes <- Language.Haskell.TH.Syntax.runIO (runPythonExample s)

  case pythonRes of
    Nothing -> [| expectationFailure $ "Expression: `" ++ s ++ "` fails in python" |]
    Just res -> do
      let qexp = [| $(toExpPython s)  `shouldBe` res |]
      case exampleStr of
        Nothing -> qexp
        Just e -> if res == e
        then qexp
        else [| expectationFailure $ "Provided result `" ++ e ++ "` does not match the python result `" ++ res ++ "`" |]

-- * Exported

{- | `checkExample formatString result` checks if, once formated,
     `formatString` is equal to result. It also checks that the result is
     the same as the one provided by python.
-}
checkExample :: String -> String -> Q Exp
checkExample s res = pyCheck s (Just res)

{- | `checkExampleDiff formatString result` checks if, once formated,
     `formatString` is equal to result. It does not check the result
     against the python implementation
-}
checkExampleDiff :: String -> String -> Q Exp
checkExampleDiff s res = [| $(toExpPython s) `shouldBe` res |]

{- | `check formatString` checks only with the python implementation
-}
check :: String -> Q Exp
check s = pyCheck s Nothing

-- * Check compilation with external GHC (this is usefull to test compilation failure)

data CompilationStatus
  = CompileError String -- ^ Fails during compilation (with error)
  | RuntimeError String
  | Ok String
  deriving (Show, Eq)

{- | Compile a formatting string

>>> checkCompile "pi:x"
CompileError "Bla bla bla, Floating cannot be formatted as hexa (`x`)
-}
checkCompile :: String -> IO CompilationStatus
checkCompile s = withSystemTempFile "PyFTest.hs" $ \path fd -> do
  IO.hPutStr fd $ "{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}\nimport PyF\nmain :: IO ()\nmain = [f|{" ++ s ++ "}|]\n"
  IO.hFlush fd

  (ecode, _stdout, stderr) <- readProcessWithExitCode "ghc" [path] ""
  case ecode of
    ExitFailure _ -> pure (CompileError stderr)
    ExitSuccess -> do
      (ecode', stdout', stderr') <- readProcessWithExitCode (take (length path - 3) path) [] ""

      case ecode' of
        ExitFailure _ -> pure (RuntimeError stderr')
        ExitSuccess -> pure (Ok stdout')

-- * Quick-checking
