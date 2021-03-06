-- |

module Graphics.BothGL.Util where

import Control.Monad.Reader.Class

zeroToNothing :: (Num i, Eq i) => i -> Maybe i
zeroToNothing 0 = Nothing
zeroToNothing i = Just i

nothingToZero :: (Num i, Eq i) => Maybe i -> i
nothingToZero Nothing = 0
nothingToZero (Just 0) = error "0 is not a valid buffer ID"
nothingToZero (Just i) = i

ask1 :: MonadReader r m => (r -> a -> m b) -> a -> m b
ask1 f  a = ask >>= \r -> f r a

ask2 :: MonadReader r m => (r -> a -> b -> m c) -> a -> b -> m c
ask2 f a b = ask >>= \r -> f r a b

ask3 :: MonadReader r m => (r -> a -> b -> c -> m d)
  -> a -> b -> c -> m d
ask3 f a b c = ask >>= \r -> f r a b c

ask4 :: MonadReader r m => (r -> a -> b -> c -> d -> m e)
  -> a -> b -> c -> d -> m e
ask4 f a b c d = ask >>= \r -> f r a b c d

ask5 :: MonadReader r m => (r -> a -> b -> c -> d -> e -> m f)
  -> a -> b -> c -> d -> e -> m f
ask5 f a b c d e = ask >>= \r -> f r a b c d e

ask6 :: MonadReader r m => (r -> a -> b -> c -> d -> e -> f -> m g)
  -> a -> b -> c -> d -> e -> f -> m g
ask6 fn a b c d e f = ask >>= \r -> fn r a b c d e f
