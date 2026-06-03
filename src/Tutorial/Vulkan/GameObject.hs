module Tutorial.Vulkan.GameObject
  ( GameObject (..)
  , modelMatrix
  ) where

import Data.Kind                           (Type)
import Data.Vector                         (Vector)
import Geomancy                            qualified as Geomancy
import Geomancy.Transform                  qualified as Geomancy
import UnliftIO.Foreign                    (Ptr)
import Vulkan                              qualified as Vk

import Tutorial.Vulkan.UniformBufferObject (UniformBufferObject (..))

type GameObject :: Type
data GameObject = GameObject
  { position, rotation, scale :: Geomancy.Vec3
  , uniformBuffers            :: Vector Vk.Buffer
  , uniformBuffersMemory      :: Vector Vk.DeviceMemory
  , uniformBuffersMapped      :: Vector (Ptr UniformBufferObject)
  , descriptorSets            :: Vector Vk.DescriptorSet
  }

modelMatrix :: GameObject -> Geomancy.Transform
modelMatrix GameObject{position, rotation, scale} =
  Geomancy.translateV position <> rotation' <> Geomancy.withVec3 scale Geomancy.scale3
 where
  rotation' =
    Geomancy.withVec3 rotation \x y z ->
       Geomancy.rotateY y
    <> Geomancy.rotateX x
    <> Geomancy.rotateZ z
