-- |

module Graphics.BothGL.Internal.GHCJS where

import           GHCJS.DOM.WebGLRenderingContextBase

type ShaderType = GLenum

newtype Shader = Shader W.WebGLShader
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype Program = Program WebGLProgram
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype AttributeLocation = AttributeLocation GLint

newtype OffsetPtr  = OffsetPtr GLintptr -- Int64

newtype Buffer a = Buffer WebGLBuffer

unBuffer :: Buffer a -> WebGLBuffer
unBuffer (Buffer b) = b

instance TypedArray a => BufferData a where
  withRawData v m = _someFFI

instance BufferData (V.Vector a) where
  withRawData v m = _unsafeVectorStuffb
