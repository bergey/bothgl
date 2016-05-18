-- |

module Graphics.BothGL.Internal.GHC where

import Graphics.BothGL.Class

import Graphics.GL

import GHC.Generics
import Data.Data

type ShaderType = GLenum

newtype Shader = Shader GLuint
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype Program = Program GLuint
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype AttributeLocation = AttributeLocation GLuint

newtype OffsetPtr  = OffsetPtr (Ptr ())

newtype BufferUsage = BufferUsage GLenum
  deriving (Eq,Num,Show,Typeable,Data,Generic)

-- | A 'Buffer' is the generic OpenGL storage object for multiple possible kind of data
--
-- For ArrayBuffer it storages vertex attributes like position, normal or color an provides
-- the MD (Multiple Data) in SIMD (Single Instruction, Multiple Data)
newtype Buffer a = Buffer GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

-- | This instance writes the data interleaved because the 'Vector' structure is already interleaved.
-- If you want an different layout use a newtype wrapper or an own data structure.
instance Storable a => BufferData (V.Vector a) where
  withRawData v m = liftIO $ V.unsafeWith v $ m . castPtr
  fromRawData bytes ptr = liftIO $ do
    fp <- newForeignPtr_ $ castPtr ptr
    return $ V.unsafeFromForeignPtr0 fp (bytes `div` sizeOf (undefined::a))
  sizeOfData v = V.length v * sizeOf (undefined::a)

instance Storable a => BufferData [a] where
  withRawData v m = liftIO . withArray v $ m . castPtr
  fromRawData bytes = liftIO . peekArray (bytes `div` sizeOf (undefined::a)) . castPtr
  sizeOfData v = length v * sizeOf (undefined::a)
