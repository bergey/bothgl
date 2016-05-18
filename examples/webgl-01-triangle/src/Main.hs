{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Graphics.BothGL

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import           Data.FileEmbed (embedFile)
import           Data.Coerce
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                 hiding (sequence_)
import           Data.Foldable                 (minimumBy)
import           Data.Ord
import Control.Monad.Trans.Maybe

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventTarget
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.Types                hiding (Event)
import           GHCJS.DOM.UIEvent
import           GHCJS.DOM.WebGLRenderingContextBase
import           GHCJS.DOM.HTMLCanvasElement
import qualified JavaScript.Web.Canvas.Internal as C
import           GHCJS.Foreign
import           GHCJS.Types
#else
import Control.Exception
import Control.Exception.Lens
import System.Exit
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import Graphics.GL.Core41
import Graphics.GL
import SDL.Raw
import Data.StateVar
import Foreign
import Foreign.C
import Control.Lens
#endif

main :: IO ()
main = do
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
  initHTML
  context <- initGL "webgl0"
  runReaderT context render
#else
  initSDL render

initSDL :: IO () -> IO ()
initSDL render =  runInBoundThread $ withCString "gl01" $ \windowName -> do
  SDL.Raw.init SDL_INIT_EVERYTHING >>= err
  -- contextMajorVersion $= 3
  -- contextMinorVersion $= 3
  -- contextProfileMask  $= SDL_GL_CONTEXT_PROFILE_CORE
  -- redSize   $= 5
  -- greenSize $= 5
  -- blueSize  $= 5
  -- depthSize $= 16
  -- doubleBuffer $= True
  let w = 640
      h = 480
      flags = SDL_WINDOW_OPENGL
          .|. SDL_WINDOW_SHOWN
          .|. SDL_WINDOW_RESIZABLE
          .|. SDL_WINDOW_FULLSCREEN
  window <- createWindow windowName SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED w h flags >>= errOnNull

  -- start OpenGL
  cxt <- glCreateContext window >>= errOnNull
  glMakeCurrent window cxt >>= err
  handling id print render `finally` do
    glDeleteContext cxt
    destroyWindow window
    quit
    exitSuccess

    -- | Treat negative return codes as prompting an error check.
err :: MonadIO m => CInt -> m ()
err e
  | e < 0 = liftIO $ do
    msg <- getError >>= peekCString
    clearError
    when (msg /= "") $ error $ msg
  | otherwise = return ()


-- | get\/set @SDL_GL_CONTEXT_MAJOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details
contextMajorVersion :: StateVar Int
contextMajorVersion = attr SDL_GL_CONTEXT_MAJOR_VERSION

-- | get\/set @SDL_GL_CONTEXT_MINOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details
contextMinorVersion :: StateVar Int
contextMinorVersion = attr SDL_GL_CONTEXT_MINOR_VERSION

contextProfileMask :: StateVar Int
contextProfileMask  = attr SDL_GL_CONTEXT_PROFILE_MASK

errOnNull :: MonadIO m => Ptr a -> m (Ptr a)
errOnNull p
  | p == nullPtr = liftIO $ do
    msg <- getError >>= peekCString
    clearError
    error $ if null msg then "Something went wrong, but SDL won't tell me what." else msg
  | otherwise = return p

-- | Use a GLattr as a variable
attr :: GLattr -> StateVar Int
attr a = StateVar (getAttr a) (setAttr a)

boolAttr :: GLattr -> StateVar Bool
boolAttr = mapStateVar fromEnum toEnum . attr

getAttr :: GLattr -> IO Int
getAttr a = alloca $ \p -> do
 glGetAttribute a p >>= err
 fromIntegral <$> peek p

setAttr :: GLattr -> Int -> IO ()
setAttr a i = glSetAttribute a (fromIntegral i) >>= err

#endif

render :: GL gl => gl ()
render = do
    Just shaderProg <- initShaders
    inputs <- initShaderInputs shaderProg
    buffers <- initBuffers
    clearColor 0 0 0 1
    -- enable context DEPTH_TEST
    drawScene shaderProg inputs buffers
    return ()

data Inputs = Inputs
  { vertexPosition :: AttributeLocation
  }

data BufferInfo = BufferInfo
  { buffer :: Buffer
  , itemSize :: GLint
  , attrType :: GLenum
                -- skip normalized, stride, offset
  , mode :: GLenum
  , firstI :: GLint
  , numItems :: GLsizei
  }

data Buffers = Buffers
  { triangles :: BufferInfo
  , squares :: BufferInfo
  }

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
initHTML :: IO ()
initHTML = do
  Just doc <- currentDocument
  Just body <- getBody doc
  setInnerHTML body . Just $
    "<canvas id=\"webgl0\" width=\"200\" height=\"200\" style=\"border: 1px solid\"></canvas>"

initGL :: String -> IO WebGL2RenderingContext
initGL name = do
  Just doc <- currentDocument
  Just canvas' <- getElementById doc name
  let canvas = coerce canvas'
  context <- coerce <$> getContext canvas "webgl"
  -- TODO error handling
  w <- getWidth canvas
  h <- getHeight canvas
  viewport context 0 0 (fromIntegral w) (fromIntegral h)
  return context
#endif

initShaders :: GL gl => gl (Maybe Program)
initShaders = do
  Just fragmentShader <- createShader GL_FRAGMENT_SHADER
  shaderSource fragmentShader fragmentShaderSource
  Just compileShader fragmentShader
  -- TODO better error handling

  vertexShader <- createShader GL_VERTEX_SHADER
  shaderSource vertexShader vertexShaderSource
  compileShader vertexShader

  shaderProgram <- createProgram
  attachShader shaderProgram fragmentShader
  attachShader shaderProgram vertexShader
  linkProgram shaderProgram
  return shaderProgram

vertexShaderSource :: ByteString -- TODO Lazy vs Strict
vertexShaderSource = $(embedFile "src/triangle.vert")

fragmentShaderSource :: ByteString
fragmentShaderSource = unpack $(embedFile "src/triangle.frag")

initShaderInputs :: GL gl => Program -> gl Inputs
initShaderInputs shaderProgram = do
  let shaderProgram' = Just shaderProgram
  useProgram shaderProgram
  Just pos <- getAttribLocation shaderProgram' "aVertexPosition"
  enableVertexAttribArray pos
  return $ Inputs (fromIntegral pos)

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI))
foreign import javascript unsafe "new Float32Array([-0.5, 0.4, 0, -0.9, -0.4, 0, -0.1, -0.4, 0])" triangleArray :: JSVal

foreign import javascript unsafe "new Float32Array([0.9, 0.4, 0, 0.1, 0.4, 0, 0.9, -0.4, 0, 0.1, -0.4, 0])" squareArray :: JSVal
-- TODO XXX FIXME Vector?
#endif


initBuffers :: GL gl => gl Buffers
initBuffers = do
  Just triBuf <- createBuffer
  bindBuffer ARRAY_BUFFER (Just triBuf)
  bufferData ARRAY_BUFFER  triangleArray STATIC_DRAW
  let tri = BufferInfo triBuf 3 FLOAT TRIANGLES 0 3
  Just sqBuf <- createBuffer
  bindBuffer ARRAY_BUFFER (Just sqBuf)
  bufferData ARRAY_BUFFER (Just $ ArrayBuffer squareArray) STATIC_DRAW
  let sq = BufferInfo sqBuf 3 FLOAT TRIANGLE_STRIP 0 4
  return $ Buffers tri sq

-- TODO move attr location into BufferInfo
drawBuffer :: GL gl => Program -> GLuint -> BufferInfo -> gl ()
drawBuffer prog attr BufferInfo{..} = do
  bindBuffer ARRAY_BUFFER (Just buffer)
  vertexAttribPointer attr itemSize attrType False 0 0
  drawArrays mode firstI numItems

drawScene :: GL gl => Program -> Inputs -> Buffers -> gl ()
drawScene prog is bs = do
  clear $ COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT
  drawBuffer prog (vertexPosition is) (triangles bs)
  drawBuffer prog (vertexPosition is) (squares bs)
