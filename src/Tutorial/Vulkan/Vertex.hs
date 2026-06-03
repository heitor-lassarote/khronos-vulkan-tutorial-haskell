module Tutorial.Vulkan.Vertex
  ( Vertex (..)
  , Index (..)
  , indexType
  , getBindingDescription
  , getAttributeDescriptions
  ) where

import Data.Kind        (Type)
import Data.Vector      (Vector)
import Data.Vector      qualified as Vector
import Data.Word        (Word32)
import Geomancy         qualified as Geomancy
import UnliftIO.Foreign (Storable (..), castPtr)
import Vulkan           qualified as Vk

type Vertex :: Type
data Vertex = Vertex
  { pos      :: Geomancy.Vec3
  , color    :: Geomancy.Vec3
  , texCoord :: Geomancy.Vec2
  }

type Index :: Type
newtype Index = Index{index :: Word32}
  deriving newtype (Num, Storable)

indexType :: Vk.IndexType
indexType = Vk.INDEX_TYPE_UINT32

instance Storable Vertex where
  sizeOf _ =
    2 * sizeOf (undefined :: Geomancy.Vec3) + sizeOf (undefined :: Geomancy.Vec2)

  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    let p = castPtr ptr
    pos <- peek p
    color <- peekByteOff p (sizeOf (undefined :: Geomancy.Vec3))
    texCoord <- peekByteOff p (2 * sizeOf (undefined :: Geomancy.Vec3))
    pure Vertex{..}

  poke ptr Vertex{..} = do
    let p = castPtr ptr
    poke p pos
    pokeByteOff p (sizeOf (undefined :: Geomancy.Vec3)) color
    pokeByteOff p (2 * sizeOf (undefined :: Geomancy.Vec3)) texCoord

getBindingDescription :: Vk.VertexInputBindingDescription
getBindingDescription = Vk.VertexInputBindingDescription
  { Vk.binding = 0
  , Vk.stride = fromIntegral $ sizeOf (undefined :: Vertex)
  , Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
  }

getAttributeDescriptions :: Vector Vk.VertexInputAttributeDescription
getAttributeDescriptions = Vector.fromList
  [ Vk.VertexInputAttributeDescription
    { Vk.location = 0, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32B32_SFLOAT
    , Vk.offset = 0
    }
  , Vk.VertexInputAttributeDescription
    { Vk.location = 1, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32B32_SFLOAT
    , Vk.offset = fromIntegral $ sizeOf (undefined :: Geomancy.Vec3)
    }
  , Vk.VertexInputAttributeDescription
    { Vk.location = 2, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32_SFLOAT
    , Vk.offset = fromIntegral $ 2 * sizeOf (undefined :: Geomancy.Vec3)
    }
  ]
