{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.DeepSeq
import Control.Exception
import Data.Bits (Bits (..))
import Data.Char (ord)
import qualified Data.Text as Text
import System.Exit
import System.FilePath
import qualified System.IO as IO
import System.IO.Temp
import System.Process (readProcessWithExitCode)
import Test.HUnit.Lang
import Test.Hspec

import PyF

-- * Check compilation with external GHC (this is usefull to test compilation failure)

data CompilationStatus
  = -- | Fails during compilation (with error)
    CompileError String
  | RuntimeError String
  | Ok String
  deriving (Show, Eq)


makeTemplate :: String -> String
makeTemplate s = [fmt|{{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules, TypeApplications #-}}
import PyF
truncate' = truncate @Float @Int
hello = "hello"
number = 3.14 :: Float
main :: IO ()
main = putStrLn [fmt|{s}|] <> "|]\n"

-- | Compile a formatting string
--
-- >>> checkCompile fileContent
-- CompileError "Bla bla bla, Floating cannot be formatted as hexa (`x`)
checkCompile :: HasCallStack => String -> IO CompilationStatus
checkCompile content = withSystemTempFile "PyFTest.hs" $ \path fd -> do
  IO.hPutStr fd content
  IO.hFlush fd
  (ecode, _stdout, stderr) <-
    readProcessWithExitCode
      "ghc"
      [ path,
        -- Include all PyF files
        "-isrc",
        -- Disable the usage of the annoying .ghc environment file
        "-package-env",
        "-",
        -- Tests use a filename in a temporary directory which may have a long filename which triggers
        -- line wrapping, reducing the reproducibility of error message
        -- By setting the column size to a high value, we ensure reproducible error messages
        "-dppr-cols=10000000000000",
        -- Clean package environment
        "-hide-all-packages",
        "-package base",
        "-package bytestring",
        "-package parsec",
        "-package text",
        "-package template-haskell",
        "-package ghc-boot",
        "-package mtl",
        "-package ghc",
        "-package time",
        "-package containers"
      ]
      ""
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
sanitize path =
  Text.unpack
    -- Strip the filename
    . Text.replace (Text.pack path) (Text.pack "INITIALPATH")
    -- GHC 9.0 replaces [Char] by String everywhere
    . Text.replace (Text.pack "[Char]") (Text.pack "String")
    . Text.pack

golden :: HasCallStack => String -> String -> IO ()
golden name output = do
  let
#if __GLASGOW_HASKELL__ >= 906
      goldenFile = "test/golden96" </> (name <> ".golden")
#else
      goldenFile = "test/golden" </> (name <> ".golden")
#endif
      actualFile = "test/golden" </> (name <> ".actual")
  -- It can fail if the golden file does not exists
  goldenContentE :: Either SomeException String <- try $ readFile goldenFile
  let -- if no golden file, the golden file is the content
      goldenContent = case goldenContentE of
        Right e -> e
        Left _ -> output
  -- Flush lazy IO
  _ <- evaluate (force goldenContent)
  -- Apparently the whitespaces in GHC 9.10 changed
  -- By stripping, we just keep the test working as expected, but we are
  -- compatible with different version of GHC.
  --
  -- TODO: use GHC API to build directly the example and gather errors in a
  -- more reproducible way.
  if Text.strip (Text.pack output) /= Text.strip (Text.pack goldenContent)
    then do
      writeFile actualFile output
      (_, diffOutput, _) <- readProcessWithExitCode "diff" ["-b", goldenFile, actualFile] ""
      putStrLn diffOutput
      -- Update golden file
      writeFile goldenFile (Text.unpack $ Text.strip (Text.pack output))
      assertFailure diffOutput
    else writeFile goldenFile (Text.unpack $ Text.strip (Text.pack output))

failCompile :: HasCallStack => String -> Spec
failCompile s = failCompileContent s s (makeTemplate s)

failCompileContent :: HasCallStack => String -> String -> String -> Spec
failCompileContent h caption fileContent =
  before (checkCompile fileContent) $ do
    let goldenName = concatMap cleanSpecialChars h
        -- Add an unique identifier, so golden files won't conflict on case
        -- insensitive systems
        -- See: bug #97.
        goldenPath = goldenName ++ "." ++ show (stableHash goldenName)
    it (show caption) $ \res -> case res of
      CompileError output -> golden goldenPath output
      _ -> assertFailure (show $ ".golden/" <> goldenPath <> "\n" <> show res)

-- | A stable hash from string, based on
-- https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function#FNV-1_hash
stableHash :: String -> Word
stableHash [] = 14695981039346656037
stableHash (x : xs) = fromIntegral (ord x) * stableHash xs `xor` 1099511628211

-- Remove chars which are not accepted in a path name
-- The encoding is rather approximative, I'm trying to avoid too long names.
cleanSpecialChars :: Char -> [Char]
cleanSpecialChars '/' = "SL"
cleanSpecialChars '\\' = "BS"
cleanSpecialChars ':' = "CL"
cleanSpecialChars '\n' = "NL"
cleanSpecialChars e = pure e

main :: IO ()
main = hspec spec

spec :: Spec
spec =
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
    -- XXX: this are not failing for now, it should be fixed
    xdescribe "not specified" $ do
      failCompile "{truncate number:.3}"
      failCompile "{hello:#}"
      failCompile "{hello:+}"
      failCompile "{hello: }"
      failCompile "{hello:-}"
      failCompile "{hello:_}"
      failCompile "{hello:,}"
    describe "multiples lines" $
      failCompile "hello\n\n\n{pi:l}"
    describe "on haskell expression parsing" $ do
      describe "single line" $
        failCompile "{1 + - / lalalal}"
      describe "empty expression" $
        failCompile "{}"
      describe "sub expression" $ do
        describe "simple failure" $
          failCompile "{pi:.{/}}"
        describe "empty failure" $
          failCompile "{pi:.{}}"
      describe "multiples lines" $
        failCompile "hello\n    {\nlet a = 5\n    b = 10\nin 1 + - / lalalal}"
    describe "non-doubled delimiters" $ do
      failCompile "hello } world"
      failCompile "hello { world"
    describe "lexical errors" $ do
      describe "single line" $
        failCompile "foo\\Pbar"
      describe "multiple line" $
        failCompile "foo\nbli\\Pbar"
    describe "Wrong type" $ do
      failCompile "{True}"
      failCompile "{True:f}"
      failCompile "{True:d}"
    describe "Missing variables" $ do
      failCompile "Hello {name}"
      failCompile "Hello {length name}"
      failCompile "Hello {pi:.{precision}}"
      failCompile "Hello {pi:.{truncate number + precision}}"
      failCompile "Hello {pi:.{precision}}"
      failCompile "Hello {pi:{width}}"
