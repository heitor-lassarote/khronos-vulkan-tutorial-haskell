module Tutorial.Vulkan.UniformBufferObject
  ( UniformBufferObject (..)
  ) where

import Data.Kind        (Type)
import Geomancy         qualified as Geomancy
import UnliftIO.Foreign (Storable (..), castPtr)

type UniformBufferObject :: Type
data UniformBufferObject = UniformBufferObject
  { model, view, proj :: Geomancy.Transform
  }

instance Storable UniformBufferObject where
  sizeOf _ = 3 * sizeOf (undefined :: Geomancy.Mat4)

  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    let p = castPtr ptr
    model <- peek p
    view' <- peekByteOff p (sizeOf (undefined :: Geomancy.Mat4))
    proj <- peekByteOff p (2 * sizeOf (undefined :: Geomancy.Mat4))
    pure UniformBufferObject{view = view', ..}

  poke ptr UniformBufferObject{..} = do
    let p = castPtr ptr
    poke p model
    pokeByteOff p (sizeOf (undefined :: Geomancy.Mat4)) view
    pokeByteOff p (2 * sizeOf (undefined :: Geomancy.Mat4)) proj
