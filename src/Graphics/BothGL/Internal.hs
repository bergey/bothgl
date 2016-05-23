{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}


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

module Graphics.BothGL.Internal where

import           Data.Foldable

#if defined(ghcjs_HOST_OS)
import           Control.Monad.IO.Class
import           Data.Data
import qualified Data.Vector.Storable                as V
import           GHC.Generics
import           GHCJS.DOM.Types
import           GHCJS.DOM.WebGLRenderingContextBase
import qualified JavaScript.TypedArray               as TA
import qualified JavaScript.TypedArray.ArrayBuffer   as AB
#else
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Data
import qualified Data.Vector.Storable                as V
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics
import           Graphics.GL
#endif

class BufferData a where
  -- | perfom a monadic action with the pointer to the raw content and the number of elements
  withRawData :: MonadIO m => a -> (RawData -> IO b) -> m b
  -- | reads 'a' from a pointer and the given size of a in bytes
  fromRawData :: MonadIO m => Int -> RawData -> m a
  -- | size of the complete data in bytes
  sizeOfData :: a -> Int

#if defined(ghcjs_HOST_OS)

type ShaderType = GLenum

newtype Shader = Shader WebGLShader
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype Program = Program WebGLProgram
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

newtype AttributeLocation = AttributeLocation GLint

newtype OffsetPtr  = OffsetPtr GLintptr -- Int64

newtype BufferUsage = BufferUsage GLenum
  deriving (Eq,Num,Show,Typeable,Data,Generic)

newtype Buffer = Buffer WebGLBuffer

unBuffer :: Buffer a -> WebGLBuffer
unBuffer (Buffer b) = b

newtype RawData = RawData ArrayBuffer

-- instance TypedArray a => BufferData a where
--   withRawData v m = _someFFI

instance BufferData ArrayBuffer where
  withRawData v m = liftIO $  m (RawData v)
  fromRawData end (RawData arr) = return . RawData $ AB.slice 0 (Just end) arr
  sizeOfData = AB.byteLength

-- instance BufferData (V.Vector a) where
--   withRawData v m =

instance TA.TypedArray a => BufferData [Elem a] where
  withRawData v m = liftIO $ do
    let l = length v
    arr <- TA.create l
    let set (i, el) = TA.unsafeSetIndex i el arr
    traverse_ set $ zip [1..] v
    m (RawData (TA.buffer arr))

  fromRawData end (RawData arr) = liftIO $ do
    mut <- AB.thaw arr -- TODO is unsafeThaw safe here?
    let l = AB.byteLength arr `div` TA.elemSize (undefined :: a)
    arr <- TA.fromArrayBuffer mut 0 (Just l)
    traverse (flip TA.unsafeIndex arr) [ 0.. l ]

  sizeOfData arr = length arr * elemSize arr
#else

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

unBuffer :: Buffer a -> GLuint
unBuffer (Buffer b) = b

newtype RawData = RawData (Ptr ())

-- | This instance writes the data interleaved because the 'Vector' structure is already interleaved.
-- If you want an different layout use a newtype wrapper or an own data structure.
instance Storable a => BufferData (V.Vector a) where
  withRawData v m = liftIO $ V.unsafeWith v $ m . RawData . castPtr
  fromRawData bytes (RawData ptr) = liftIO $ do
    fp <- newForeignPtr_ $ castPtr ptr
    return $ V.unsafeFromForeignPtr0 fp (bytes `div` sizeOf (undefined::a))
  sizeOfData v = V.length v * sizeOf (undefined::a)

instance Storable a => BufferData [a] where
  withRawData v m = liftIO . withArray v $ m . RawData . castPtr
  fromRawData bytes (RawData p) = liftIO . peekArray (bytes `div` sizeOf (undefined::a)) . castPtr $ p
  sizeOfData v = length v * sizeOf (undefined::a)

#endif
