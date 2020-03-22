module PyF.Internal.Extensions
  ( thExtToMetaExt,
  )
where

import qualified Language.Haskell.Exts.Extension as Exts
import qualified Language.Haskell.TH as TH

-- | Associate a template haskell extension to an haskell-src-ext extension
thExtToMetaExt :: TH.Extension -> Maybe Exts.Extension
thExtToMetaExt e = Exts.EnableExtension <$> case e of
  -- For some reason, the casing is different between TH and haskell-src-exts
  TH.Cpp -> Just Exts.CPP
  -- Hope for the best, if haskell-src-ext knows the extension or not
  _ -> case Exts.parseExtension (show e) of
    Exts.EnableExtension ext -> Just ext
    Exts.DisableExtension ext -> Just ext
    Exts.UnknownExtension _ -> Nothing
