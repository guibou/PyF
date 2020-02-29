module SpecCustomDelimiters where

import Language.Haskell.TH.Quote
import PyF

myCustomFormatter :: QuasiQuoter
myCustomFormatter = fmtWithDelimiters ('@', '!')
