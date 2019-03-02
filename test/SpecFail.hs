{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

import Test.Hspec
import Test.HUnit.Lang

import System.Process (readProcessWithExitCode)
import System.Exit

import qualified Data.Text as Text
 
import System.IO.Temp
import qualified System.IO as IO

import Data.Hashable
import System.FilePath
import Control.Exception
import Control.DeepSeq
import System.Directory

-- * Check compilation with external GHC (this is usefull to test compilation failure)

data CompilationStatus
  = CompileError String -- ^ Fails during compilation (with error)
  | RuntimeError String
  | Ok String
  deriving (Show, Eq)

makeTemplate :: String -> String
makeTemplate s = "{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, TypeApplications #-}\nimport PyF\ntruncate' = truncate @Float @Int\nhello = \"hello\"\nnumber = 3.14 :: Float\nmain :: IO ()\nmain = [f|" ++ s ++ "|]\n"

{- | Compile a formatting string

>>> checkCompile fileContent
CompileError "Bla bla bla, Floating cannot be formatted as hexa (`x`)
-}
checkCompile :: HasCallStack => String -> IO CompilationStatus
checkCompile content = withSystemTempFile "PyFTest.hs" $ \path fd -> do
  IO.hPutStr fd content
  IO.hFlush fd

  (ecode, _stdout, stderr) <- readProcessWithExitCode "ghc" [path,
                                                             -- Include all PyF files
                                                             "-isrc",
                                                             -- Disable the usage of the annoying .ghc environment file
                                                             "-package-env", "-",
                                                            -- Tests use a filename in a temporary directory which may have a long filename which triggers
                                                            -- line wrapping, reducing the reproducibility of error message
                                                            -- By setting the column size to a high value, we ensure reproducible error messages
                                                             "-dppr-cols=10000000000000"
                                                            ] ""
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

golden :: HasCallStack => String -> String -> IO ()
golden name output = do
  let
    goldenFile = ".golden" </> name </> "golden"
    actualFile = ".golden" </> name </> "actual"

  createDirectoryIfMissing True (".golden" </> name)

  -- It can fail if the golden file does not exists
  goldenContentE :: Either SomeException String <- try $ readFile goldenFile

  let
    -- if no golden file, the golden file is the content
    goldenContent = case goldenContentE of
      Right e -> e
      Left _ -> output

  -- Flush lazy IO
  _ <- evaluate (force goldenContent)

  if output /= goldenContent
    then do
      writeFile actualFile output
      (_, diffOutput, _) <- readProcessWithExitCode "diff" [goldenFile, actualFile] ""

      putStrLn diffOutput

      -- Update golden file
      writeFile goldenFile output

      assertFailure diffOutput
    else do
      writeFile goldenFile output


-- if the compilation fails, runs a golden test on compilation output
-- else, fails the test
fileFailCompile :: HasCallStack => FilePath -> Spec
fileFailCompile path = do
  fileContent <- runIO $ readFile path

  -- I'm using the hash of the path, considering that the file content can evolve
  failCompileContent (hash path) path fileContent

failCompile :: HasCallStack => String -> Spec
failCompile s = failCompileContent (hash s) s (makeTemplate s)

failCompileContent :: HasCallStack => Int -> String -> String -> Spec
failCompileContent h caption fileContent = do
  before (checkCompile fileContent) $ it (show caption) $ \res -> case res of
   CompileError output -> golden (show h) output
   _ -> assertFailure (show $ ".golden/" <> show h  <> "\n" <>show res)

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

      describe "grouping" $ do
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
      failCompile "{hello:+}"
      failCompile "{hello: }"
      failCompile "{hello:-}"
      failCompile "{hello:_}"
      failCompile "{hello:,}"

    describe "multiples lines" $ do
      failCompile "hello\n\n\n{pi:l}"

    describe "on haskell expression parsing" $ do
      describe "single line" $ do
        failCompile "{1 + - / lalalal}"
      describe "multiples lines" $ do
        failCompile "hello\n    {\nlet a = 5\n    b = 10\nin 1 + - / lalalal}"

    describe "non-doubled delimiters" $ do
      failCompile "hello } world"
      failCompile "hello { world"

    describe "fail is not enabled extension" $ do
      failCompile "{0b0001}"

    describe "lexical errors" $ do
      failCompile "foo\\Pbar"

    describe "fileFailures" $ do
      mapM_ fileFailCompile [
        "test/failureCases/bug18.hs"
        ]
