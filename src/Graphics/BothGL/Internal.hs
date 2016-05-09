{-# LANGUAGE CPP #-}

-- | The constructors exported from this module will be different
-- depending on whether the library is compiled with GHC or GHCJS.  If
-- you only need the types, without the constructors, import
-- 'Graphics.BothGL.Types' instead.  If you need this module, you
-- probably want to guard uses with CPP, as this module itself does,
-- so that your code can still compile on both targets.
--
-- If you are reading this on Hackage, also note that the types shown
-- reflect GHC, not GHCJS.  There are currently no HTML docs of the
-- GHCJS internals.  For now, read the source of
-- 'Graphics.BothGL.Internal.GHCJS'.

module Graphics.BothGL.Internal (
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
  module Graphics.BothGL.Internal.GHCJS
#else
  module Graphics.BothGL.Internal.GHC
#endif
  ) where
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
 import Graphics.BothGL.Internal.GHCJS
#else
  import Graphics.BothGL.Internal.GHC
#endif
