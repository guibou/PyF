{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test where
import PyF
import Data.Text
import Language.Haskell.TH (pprint, Loc (..), runQ)
import PyF.Internal.QQ
import Language.Haskell.TH (stringE)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (All)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy.Builder.Int as Builder.Int


-- test =  do
--  let t = "abc" :: Text
--  [int|bac|] :: Text


-- foo = do
-- --   res <- runQ $ toExpPlain' Loc { loc_filename = "<interactive>", loc_package = "main", loc_module = "Main", loc_start = (1, 1), loc_end = (1, 1) } "abc" [] fmtConfig
--   res <- runQ @IO $ toFormatPlain (RawPlain "abc")
--   putStrLn (pprint $ res)

bar =  $(
    -- s <- stringE "abc"
    -- toFormatPlain (ReplacementPlain (undefined, s))
    nonEmptyE (NE.singleton [| "abc" |])

    ) :: Text

baz :: Builder
baz = do
  let t = 32 :: Int
  [int|abc{t}|]


instance Interpolate Text Builder where
    interpolateInto = Builder.fromText

instance Interpolate Int Builder where
    interpolateInto = Builder.Int.decimal
