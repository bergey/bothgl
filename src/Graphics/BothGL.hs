-- | This package provides an OpenGL abstraction that targets either
-- WebGL (with GHCJS) or native OpenGL bindings (with GHC).  Only the
-- context setup code differs; the same shader & rendering code can be
-- used in both cases.
--
-- The library is deliberately low-level, staying as close to the
-- original APIs as possible while hiding their differences.  If you
-- need access to features not available on both targets, see
-- 'Graphics.BothGL.Internal'

module Graphics.BothGL (

  module Graphics.BothGL.GL
  , module Graphics.BothGL.Types
  ) where

import Graphics.BothGL.Types
import Graphics.BothGL.GL
