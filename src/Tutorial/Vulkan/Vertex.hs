module Tutorial.Vulkan.Vertex
  ( Vertex (..)
  , getBindingDescription
  , getAttributeDescriptions
  ) where

import Data.Kind        (Type)
import Data.Vector      (Vector)
import Data.Vector      qualified as Vector
import Linear           qualified as Linear
import UnliftIO.Foreign (Storable (..), castPtr)
import Vulkan           qualified as Vk

type Vertex :: Type
data Vertex = Vertex
  { pos   :: Linear.V2 Float
  , color :: Linear.V3 Float
  }

instance Storable Vertex where
  sizeOf _ =
    sizeOf (undefined :: Linear.V2 Float) + sizeOf (undefined :: Linear.V3 Float)

  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    let p = castPtr ptr
    pos <- peek p
    color <- peekByteOff p (sizeOf (undefined :: Linear.V2 Float))
    pure Vertex{..}

  poke ptr Vertex{..} = do
    let p = castPtr ptr
    poke p pos
    pokeByteOff p (sizeOf (undefined :: Linear.V2 Float)) color

getBindingDescription :: Vk.VertexInputBindingDescription
getBindingDescription = Vk.VertexInputBindingDescription
  { Vk.binding = 0
  , Vk.stride = fromIntegral $ sizeOf (undefined :: Vertex)
  , Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
  }

getAttributeDescriptions :: Vector Vk.VertexInputAttributeDescription
getAttributeDescriptions = Vector.fromList
  [ Vk.VertexInputAttributeDescription
    { Vk.location = 0, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32_SFLOAT
    , Vk.offset = 0
    }
  , Vk.VertexInputAttributeDescription
    { Vk.location = 1, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32B32_SFLOAT
    , Vk.offset = fromIntegral $ sizeOf (undefined :: Linear.V2 Float)
    }
  ]
