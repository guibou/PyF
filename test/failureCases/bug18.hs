{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS -Wall -Werror #-}

{-
  This test actually fails with -Wall because fieldA is not
  initialized. PyF is doing nothing wrong. But bug #18 shows that GHC
  error message leaks a lot of the PyF generated code which is not
  beautiful.

  This tests helps tracking the conciseness of the PyF generated code.
-}

import qualified Data.Text as T
import PyF

data Foo
  = Foo
      { fieldA :: (),
        fieldB :: T.Text
      }
  deriving (Show)

yolo :: Foo
yolo =
  Foo
    { fieldB = [fmt|hello what's up {x}|]
    }
  where
    x :: T.Text
    x = T.pack "hi"

main = print yolo
