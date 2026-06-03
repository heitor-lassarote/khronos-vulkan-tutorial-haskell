module Tutorial.Vulkan.Utils
  ( iFindIndexM
  , iFindIndex
  , findM
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Vector            (Vector)
import Data.Vector            qualified as Vector

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
