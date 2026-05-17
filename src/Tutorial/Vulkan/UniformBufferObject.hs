module Tutorial.Vulkan.UniformBufferObject
  ( UniformBufferObject (..)
  ) where

import Data.Kind        (Type)
import Linear           qualified as Linear
import UnliftIO.Foreign (Storable (..), castPtr)

type UniformBufferObject :: Type
data UniformBufferObject = UniformBufferObject
  { model, view, proj :: Linear.M44 Float
  }

instance Storable UniformBufferObject where
  sizeOf _ = 3 * sizeOf (undefined :: Linear.M44 Float)

  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    let p = castPtr ptr
    model <- peek p
    view' <- peekByteOff p (sizeOf (undefined :: Linear.M44 Float))
    proj <- peekByteOff p (2 * sizeOf (undefined :: Linear.M44 Float))
    pure UniformBufferObject{view = view', ..}

  poke ptr UniformBufferObject{..} = do
    let p = castPtr ptr
    poke p model
    pokeByteOff p (sizeOf (undefined :: Linear.M44 Float)) view
    pokeByteOff p (2 * sizeOf (undefined :: Linear.M44 Float)) proj
