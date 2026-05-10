module Tutorial.Vulkan.Vertex
  ( Vertex (..)
  , Index (..)
  , indexType
  , getBindingDescription
  , getAttributeDescriptions
  ) where

import Data.Hashable    (Hashable (..))
import Data.Kind        (Type)
import Data.Vector      (Vector)
import Data.Vector      qualified as Vector
import Data.Word        (Word32)
import Linear           qualified as Linear
import UnliftIO.Foreign (Storable (..), castPtr)
import Vulkan           qualified as Vk

type Vertex :: Type
data Vertex = Vertex
  { pos      :: Linear.V3 Float
  , color    :: Linear.V3 Float
  , texCoord :: Linear.V2 Float
  }
  deriving stock (Eq)

instance Hashable Vertex where
  hashWithSalt salt Vertex{..} =
    salt `hashWithSalt` pos `hashWithSalt` color `hashWithSalt` texCoord

type Index :: Type
newtype Index = Index{index :: Word32}
  deriving newtype (Num, Storable)

indexType :: Vk.IndexType
indexType = Vk.INDEX_TYPE_UINT32

instance Storable Vertex where
  sizeOf _ =
    2 * sizeOf (undefined :: Linear.V3 Float) + sizeOf (undefined :: Linear.V2 Float)

  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    let p = castPtr ptr
    pos <- peek p
    color <- peekByteOff p (sizeOf (undefined :: Linear.V3 Float))
    texCoord <- peekByteOff p (2 * sizeOf (undefined :: Linear.V3 Float))
    pure Vertex{..}

  poke ptr Vertex{..} = do
    let p = castPtr ptr
    poke p pos
    pokeByteOff p (sizeOf (undefined :: Linear.V3 Float)) color
    pokeByteOff p (2 * sizeOf (undefined :: Linear.V3 Float)) texCoord

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
    , Vk.offset = fromIntegral $ sizeOf (undefined :: Linear.V3 Float)
    }
  , Vk.VertexInputAttributeDescription
    { Vk.location = 2, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32_SFLOAT
    , Vk.offset = fromIntegral $ 2 * sizeOf (undefined :: Linear.V3 Float)
    }
  ]
