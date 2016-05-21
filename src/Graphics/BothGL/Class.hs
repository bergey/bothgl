{-# LANGUAGE CPP #-}

-- |

module Graphics.BothGL.Class where

#if defined(ghcjs_HOST_OS)
#else
#endif

class BufferData a where
  -- | perfom a monadic action with the pointer to the raw content and the number of elements
  withRawData :: MonadIO m => a -> (RawData -> IO b) -> m b
  -- | reads 'a' from a pointer and the given size of a in bytes
  fromRawData :: MonadIO m => Int -> RawData -> m a
  -- | size of the complete data in bytes
  sizeOfData :: a -> Int
