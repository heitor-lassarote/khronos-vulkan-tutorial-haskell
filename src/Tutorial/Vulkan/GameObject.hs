module Tutorial.Vulkan.GameObject
  ( GameObject (..)
  , modelMatrix
  ) where

import Control.Lens                        ((&), (.~), (^.))
import Data.Kind                           (Type)
import Data.Vector                         (Vector)
import Linear                              qualified as Linear
import UnliftIO.Foreign                    (Ptr)
import Vulkan                              qualified as Vk

import Tutorial.Vulkan.UniformBufferObject (UniformBufferObject (..))

type GameObject :: Type
data GameObject = GameObject
  { position, rotation, scale :: Linear.V3 Float
  , uniformBuffers            :: Vector Vk.Buffer
  , uniformBuffersMemory      :: Vector Vk.DeviceMemory
  , uniformBuffersMapped      :: Vector (Ptr UniformBufferObject)
  , descriptorSets            :: Vector Vk.DescriptorSet
  }

modelMatrix :: GameObject -> Linear.M44 Float
modelMatrix GameObject{position, rotation, scale} =
  Linear.mkTransformation rotation' position
  & (Linear.!*! scale')
  & Linear.transpose
 where
  rotation' =
      Linear.axisAngle (Linear.V3 0 1 0) (rotation ^. Linear._y)
    * Linear.axisAngle (Linear.V3 1 0 0) (rotation ^. Linear._x)
    * Linear.axisAngle (Linear.V3 0 0 1) (rotation ^. Linear._z)
  scale' = Linear.scaled (Linear.vector scale & Linear._w .~ 1)
