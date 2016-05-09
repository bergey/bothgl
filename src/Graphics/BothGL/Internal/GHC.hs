-- |

module Graphics.BothGL.Internal.GHC where

import Graphics.Rendering.OpenGL

import GHC.Generics
import Data.Data

type ShaderType = GLenum

newtype Shader = Shader GLuint
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype Program = Program GLuint
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype AttributeLocation = AttributeLocation GLuint

newtype OffsetPtr  = OffsetPtr (Ptr ())

-- | A 'Buffer' is the generic OpenGL storage object for multiple possible kind of data
--
-- For ArrayBuffer it storages vertex attributes like position, normal or color an provides
-- the MD (Multiple Data) in SIMD (Single Instruction, Multiple Data)
newtype Buffer a = Buffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)
