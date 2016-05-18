-- |

module Graphics.BothGL.Instances.GHC where

instance MonadIO gl => GL gl where
createShader t = fmap (fmap Shader . zeroNothing) glCreateShader t

shaderSource (Shader s) bs =
  allocaArray (length chunks) $ \ps ->
  allocaArray (length chunks) $ \pl ->
  go 0 chunks ps pl
    where
        chunks = Lazy.toChunks bs
          go :: Int -> [Strict.ByteString] -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()
          go i (Strict.PS fp o l:cs) ps pl = do
            pokeElemOff pl i (fromIntegral l)
            withForeignPtr fp $ \p -> do
            pokeElemOff ps i (castPtr p `plusPtr` o)
          go (i+1) cs ps pl

          go i [] ps pl = glShaderSource sh (fromIntegral i) ps pl

compileShader (Shader s) = glCompileShader s

attachShader (Program p) (Shader s) = glAttachShader p s

linkProgram (Program p) = glLinkProgram

useProgram (Program p) = glUseProgram p

attributeLocation (Program p) s =
  liftIO $ check <$> withCString s (glGetAttribLocation p . castPtr) where
    check n
    | n < 0     = Nothing
    | otherwise = Just $ fromIntegral n

enableVertexAttribArray = glEnableVertexAttribArray

vertexAttribPointer loc  comp ty toNorm stride offPtr =
  liftIO $ glVertexAttribPointer loc (fromIntegral comp) ty (if toNorm then GL_TRUE else GL_FALSE) (fromIntegral stride) offPtr

drawArrays mode p size = liftIO $ glDrawArrays mode p size

clear = glClear

createBuffer

bindBuffer

bufferData t d = withRawData v $ \ptr ->
    glBufferData t (fromIntegral $ sizeOfData v) ptr (coerce u)
