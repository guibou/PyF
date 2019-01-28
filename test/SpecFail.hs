{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

import System.Process (readProcessWithExitCode)
import System.Exit

import qualified Data.Text as Text
 
import System.IO.Temp
import qualified System.IO as IO

import Data.Hashable

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
  IO.hPutStr fd $ "{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, TypeApplications #-}\nimport PyF\ntruncate' = truncate @Float @Int\nhello = \"hello\"\nnumber = 3.14 :: Float\nmain :: IO ()\nmain = [f|" ++ s ++ "|]\n"
  IO.hFlush fd

  (ecode, _stdout, stderr) <- readProcessWithExitCode "ghc" [path, "-isrc"] ""
  case ecode of
    ExitFailure _ -> pure (CompileError (sanitize path stderr))
    ExitSuccess -> do
      (ecode', stdout', stderr') <- readProcessWithExitCode (take (length path - 3) path) [] ""

      case ecode' of
        ExitFailure _ -> pure (RuntimeError stderr')
        ExitSuccess -> pure (Ok stdout')

-- sanitize a compilation result by removing variables strings such as
-- temporary files name
sanitize :: FilePath -> String -> String
sanitize path s =
  -- strip the filename
  let
    t = Text.pack s
  in Text.unpack (Text.replace (Text.pack path) (Text.pack "INITIALPATH") t) 

-- if the compilation fails, runs a golden test on compilation output
-- else, fails the test
failCompile :: String -> Spec
failCompile s = do
  before (checkCompile s) $ it (s ++ " " ++ show (hash s)) $ \res -> case res of
    CompileError output -> defaultGolden (show $ hash s) output
    _ -> error (show res)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "error reporting" $ do
    describe "string" $ do
      describe "integral / fractional qualifiers" $ do
        failCompile "{hello:f}"
        failCompile "{hello:d}"
        failCompile "{hello:e}"
        failCompile "{hello:b}"
        failCompile "{hello:E}"
        failCompile "{hello:G}"
        failCompile "{hello:g}"
        failCompile "{hello:%}"
        failCompile "{hello:x}"
        failCompile "{hello:X}"
        failCompile "{hello:o}"

      describe "padding center" $ do
        failCompile "{hello:=100s}"
        failCompile "{hello:=100}"

      -- XXX: this are not failing for now, it should be fixed
      xdescribe "grouping" $ do
        failCompile "{hello:_s}"
        failCompile "{hello:,s}"

      describe "sign" $ do
        failCompile "{hello:+s}"
        failCompile "{hello: s}"
        failCompile "{hello:-s}"

    describe "number" $ do
      failCompile "{truncate' number:f}"
      failCompile "{truncate' number:g}"
      failCompile "{truncate' number:G}"
      failCompile "{truncate' number:e}"
      failCompile "{truncate' number:E}"
      failCompile "{truncate' number:%}"
      failCompile "{truncate number:s}"

    describe "number with precision" $ do
      failCompile "{truncate number:.3d}"
      failCompile "{truncate number:.3o}"
      failCompile "{truncate number:.3b}"
      failCompile "{truncate number:.3x}"

    describe "floats" $ do
      failCompile "{number:o}"
      failCompile "{number:b}"
      failCompile "{number:x}"
      failCompile "{number:X}"
      failCompile "{number:d}"
      failCompile "{number:s}"

    -- XXX: this are not failing for now, it should be fixed
    xdescribe "not specified" $ do
      failCompile "{truncate number:.3}"
      failCompile "{hello:#}"