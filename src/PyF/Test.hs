{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Yak.Test where

import Yak.PythonSyntax
import Prelude hiding (putStrLn)

import Data.Text.Lazy.IO
import Data.Text.Lazy

main = do
  let float = 12.234423
      string = "Hello" :: Text
      aIntValue = 12 :: Int

  -- A bit of string padding
  putStrLn [f|{string} my name is Guillaume|]
  putStrLn [f|{string:s} my name is Guillaume|]
  putStrLn [f|{string:-<10} my name is Guillaume|]
  putStrLn [f|{string:->10} my name is Guillaume|]
  putStrLn [f|{string:-^10} my name is Guillaume|]

  -- Floating poaIntValue
  -- putStrLn [f|The amount is {float}|]
  putStrLn [f|The amount is {float:f}|]
  putStrLn [f|The amount is {float:.3f}|]
  putStrLn [f|The amount is {float:-<10.3f}|]
  putStrLn [f|The amount is {float:-<10.3F}|]
  putStrLn [f|The amount is {float:-<10.3e}|]
  putStrLn [f|The amount is {float:-<10.3E}|]
  putStrLn [f|The amount is {float:-<10.3%}|]

  -- AIntValueeger
  putStrLn [f|The amount is {aIntValue}|]
  putStrLn [f|The amount is {aIntValue:.3d}|]
  putStrLn [f|The amount is {aIntValue:o}|]
  putStrLn [f|The amount is {aIntValue:x}|]
  putStrLn [f|The amount is {aIntValue:X}|]
