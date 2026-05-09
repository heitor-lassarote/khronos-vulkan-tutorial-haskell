module Tutorial.Vulkan.Utils
  ( iFindIndexM
  , iFindIndex
  , findM
  , perspectiveVulkan
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Vector            (Vector)
import Data.Vector            qualified as Vector
import Linear                 qualified as Linear

-- | Monad variant of 'iFindIndex' that accepts the index in the predicate.
iFindIndexM :: (Monad m) => (Int -> a -> m Bool) -> Vector a -> m (Maybe Int)
iFindIndexM predicate v = go 0
 where
  len = Vector.length v
  go i
    | i == len  = pure Nothing
    | otherwise = predicate i (Vector.unsafeIndex v i) >>= \case
      False -> go $ i + 1
      True  -> pure $ Just i

-- | Variant of 'Vector.findIndex' that accepts the index in the predicate.
iFindIndex :: (Int -> a -> Bool) -> Vector a -> Maybe Int
iFindIndex predicate v = runIdentity $ iFindIndexM (\i x -> Identity $ predicate i x) v

-- | Monadic variant of 'Vector.find'.
findM :: (Monad m) => (a -> m Bool) -> Vector a -> m (Maybe a)
findM predicate v = go 0
 where
  len = Vector.length v
  go i =
    let val = Vector.unsafeIndex v i in
    if
      | i == len  -> pure Nothing
      | otherwise -> predicate val >>= \case
        False -> go $ i + 1
        True  -> pure $ Just val

-- | Like 'Linear.perspective', but negates the y coordinate's scaling factor to match Vulkan's y axis pointing down.
--
-- Moreover, maps the depth from [-0.5, 0.5] (OpenGL-style) to [0, 1] (Vulkan-style).
perspectiveVulkan
  :: Floating a
  => a -- ^ FOV (y direction, in radians)
  -> a -- ^ Aspect ratio
  -> a -- ^ Near plane
  -> a -- ^ Far plane
  -> Linear.M44 a
perspectiveVulkan fovy aspect near far =
  Linear.V4
    (Linear.V4 x 0  0 0)
    (Linear.V4 0 y  0 0)
    (Linear.V4 0 0  z w)
    (Linear.V4 0 0 -1 0)
 where
  nmf = near - far
  f = recip (tan (fovy / 2))
  x = f / aspect
  y = -f
  z = far / nmf
  w = far * near / nmf
