module SpecCustomDelimiters where

import Language.Haskell.TH.Quote
import PyF
import PyF.Internal.QQ

myCustomFormatter :: QuasiQuoter
myCustomFormatter =
  mkFormatter
    "fmt"
    ( defaultConfig
        { delimiters = ('@', '!')
        }
    )
