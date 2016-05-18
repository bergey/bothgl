{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

module Graphics.BothGL.GL where

import Graphics.BothGL.Internal
import Graphics.BothGL.Util

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as Lazy
import Data.Maybe

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
import Control.Monad.Reader.Class
import qualified GHCJS.DOM.WebGLRenderingContextBase as W
#else
import Graphics.GL
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Unsafe as Strict
import qualified Data.ByteString.Internal as Strict
import Data.Coerce
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
#endif

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
type GL m = (MonadIO m, MonadReader WebGL2RenderingContext m)
#else
type GL m = MonadIO m
#endif

createShader :: GL gl => ShaderType -> gl (Maybe Shader)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
createShader = ask1 W.createShader
#else
createShader = fmap (fmap Shader . zeroToNothing) . glCreateShader
#endif

shaderSource :: GL gl => Shader -> Lazy.ByteString -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
shaderSource (Shader s) = ask2 W.shaderSource s
#else
shaderSource (Shader s) bs = liftIO $
  allocaArray (length chunks) $ \ps ->
  allocaArray (length chunks) $ \pl ->
  go 0 chunks ps pl
    where
        chunks = Lazy.toChunks bs
        go :: Int -> [Strict.ByteString] -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()
        go i (Strict.PS fp o l : cs) ps pl = do
            pokeElemOff pl i (fromIntegral l)
            withForeignPtr fp $ \p -> do
                pokeElemOff ps i (castPtr p `plusPtr` o)
                go (i+1) cs ps pl

        go i [] ps pl = glShaderSource s (fromIntegral i) ps pl
#endif

compileShader :: GL gl => Shader -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
compileShader (Shader s) = ask1 W.compileShader s
#else
compileShader (Shader s) = glCompileShader s
#endif

createProgram :: GL gl => gl Program
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
createProgram = Program <$> ask1 W.createProgram
#else
createProgram = Program <$> glCreateProgram
#endif

attachShader :: GL gl => Program -> Shader -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
attachShader (Program p) (Shader s) = ask1 W.attachShader p s
#else
attachShader (Program p) (Shader s) = glAttachShader p s
#endif

linkProgram :: GL gl => Program -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
linkProgram (Program p) = ask1 W.linkProgram (Just p)
#else
linkProgram (Program p) = glLinkProgram p
#endif

useProgram :: GL gl => Program -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
useProgram (Program p) = ask1 W.useProgram (Just p)
#else
useProgram (Program p) = glUseProgram p
#endif

getAttribLocation :: GL gl => Program -> String -> gl (Maybe AttributeLocation)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
getAttribLocation (Program p) s = (fmap . fmap) AttributeLocation $
  ask1 getAttribLocation (Just p) s
#else
getAttribLocation (Program p) s =
  liftIO $ fmap AttributeLocation . zeroToNothing <$> withCString s (fmap fromIntegral . glGetAttribLocation p . castPtr)
#endif

enableVertexAttribArray :: GL gl => AttributeLocation -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
enableVertexAttribArray (AttributeLocation loc) =
  ask1 W.enableVertexAttribArray (fromIntegral loc)
#else
enableVertexAttribArray (AttributeLocation loc) = glEnableVertexAttribArray loc
#endif

vertexAttribPointer :: GL gl => GLuint ->
  GLint -> GLenum -> Bool -> GLsizei -> OffsetPtr -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
vertexAttribPointer = ask6 W.vertexAttribPointer
#else
vertexAttribPointer loc  comp ty toNorm stride (OffsetPtr offPtr) =
  liftIO $ glVertexAttribPointer loc (fromIntegral comp) ty (if toNorm then GL_TRUE else GL_FALSE) (fromIntegral stride) offPtr
#endif

drawArrays :: GL gl => GLenum -> GLint -> GLsizei -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
drawArrays = ask3 W.drawArrays
#else
drawArrays mode p size = liftIO $ glDrawArrays mode p size
#endif

clear :: GL gl => GLbitfield -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
clear = ask1 W.clear
#else
clear = glClear
#endif

createBuffer :: GL gl => gl (Maybe (Buffer a))
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
createBuffer = ask >>= W.createBuffer
#else
createBuffer = liftIO $ allocaArray 1 $ \p -> do
  glGenBuffers (fromIntegral 1) p
  fmap Buffer . listToMaybe <$> peekArray 1 p
#endif

bindBuffer :: GL gl => GLenum -> Maybe (Buffer a) -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
bindBuffer t b = ask2 W.bindBuffer t (unBuffer <$> b)
#else
bindBuffer target buf = glBindBuffer target (maybe 0 (\(Buffer b) -> b) buf)
#endif

bufferData :: GL gl => BufferData d => GLenum -> d -> BufferUsage -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
bufferData t v u = withRawData v $ \arr -> ask3 W.bufferData t arr u
#else
bufferData t v u = withRawData v $ \(RawData ptr) ->
  glBufferData t (fromIntegral $ sizeOfData v) ptr (coerce u)
#endif

clearColor :: GL gl => GLclampf -> GLclampf -> GLclampf -> GLclampf -> gl ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
clearColor = ask4 W.clearColor
#else
clearColor = glClearColor
#endif

-- TODO patterns for shader types
