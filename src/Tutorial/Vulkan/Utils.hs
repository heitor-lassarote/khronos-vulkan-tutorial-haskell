module Tutorial.Vulkan.Utils
  ( iFindIndexM
  , iFindIndex
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Vector            (Vector)
import Data.Vector            qualified as Vector

iFindIndexM :: (Monad m) => (Int -> a -> m Bool) -> Vector a -> m (Maybe Int)
iFindIndexM predicate v = go 0
 where
  len = Vector.length v
  go i
    | i == len  = pure Nothing
    | otherwise = predicate i (Vector.unsafeIndex v i) >>= \case
      False -> go $ i + 1
      True  -> pure $ Just i

iFindIndex :: (Int -> a -> Bool) -> Vector a -> Maybe Int
iFindIndex predicate v = runIdentity $ iFindIndexM (\i x -> Identity $ predicate i x) v
