{-# LANGUAGE CPP #-}

-- | This package provides an OpenGL abstraction that target either
-- WebGL (with GHCJS) or native OpenGL bindings (with GHC).  Only the
-- context setup code differs; the same shader & rendering code can be
-- used in both cases.
--
-- The library is deliberately low-level, staying as close to the
-- original APIs as possible while hiding their differences.  If you
-- need access to features not available on both targets, see
-- 'Graphics.BothGL.Internal'

module Graphics.BothGL where

import Control.Monad.Reader.Class
import Graphics.BothGL.Internal

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
import qualified GHCJS.DOM.WebGLRenderingContextBase as W
#endif

-- Both
class MonadIO => GL gl where
  createShader :: ShaderType -> gl (Maybe Shader)
  -- getShaderSource :: Shader -> gl
  shaderSource :: Shader -> Lazy.ByteString -> gl ()
  compileShader :: Shader -> gl ()
  createProgram :: gl Program
  attachShader :: Program -> Shader -> gl ()
  linkProgram :: Program -> gl ()
  useProgram :: Program -> gl ()
  getAttribLocation :: Program -> String -> gl (Maybe AttributeLocation)
  enableVertexAttribArray :: AttributeLocation -> gl ()
  vertexAttribPointer :: GLuint ->
    GLint -> GLenum -> GLboolean -> GLsizei -> OffsetPtr -> gl ()
  drawArrays :: GLenum -> GLint -> GLsizei -> gl ()
  clear :: GLbitfield -> gl ()
  createBuffer :: gl (Maybe Buffer)
  bindBuffer :: GLenum -> Maybe Buffer -> gl ()
  bufferData :: BufferData d => GLenum -> d -> gl ()

-- TODO Instances
-- 1. GHC & OpenGL package
-- 2. GHCJS & ghcjs-dom package
-- 3. GHC & gl package (Check if Edward wants this.)

-- The type class is general enough to allow other instances, eg,
-- WriterT logging OpenGL commands, or even a pure serialization.  I
-- don't know if there's any demand.  The deciding factor for putting
-- all the functions in the type class is the option to support both
-- OpenGL and gl, and possibly some other WebGL binding also.

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
instance (MonadIO gl, MonadReader WebGL2RenderingContext gl) => GL gl where
  createShader = ask1 W.createShader
  shaderSource (Shader s) = ask2 W.shaderSource s
  compileShader (Shader s) = ask1 W.compileShader s
  createProgram = Program <$> ask1 W.createProgram
  attachShader (Program p) (Shader s) = ask1 W.attachShader p s
  linkProgram (Program p) = ask1 W.linkProgram (Just p)
  useProgram (Program p) = ask1 W.useProgram (Just p)
  getAttribLocation (Program p) = ask1 (Just p)
  enableVertexAttribArray (AttributeLocation loc) =
    ask1 W.enableVertexAttribArray (fromIntegral loc)
  vertexAttribPointer = ask6 W.vertexAttribPointer
  drawArrays = ask3 W.drawArrays
  clear = ask1 W.clear
  createBuffer = ask >>= W.createBuffer
  bindBuffer t b = ask2 W.bindBuffer t (unBuffer <$> b)
#else
import Graphics.BothGL.Instances.GHC
#endif
