{-# LANGUAGE CPP #-}

-- | TODO XXX Document this like Graphics.BothGL.Internal

module Graphics.BothGL.Types (
  ShaderType
  , Shader
  , Program
  ) where

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
 import Graphics.BothGL.Internal.GHCJS
#else
  import Graphics.BothGL.Internal.GHC
#endif
