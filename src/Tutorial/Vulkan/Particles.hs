module Tutorial.Vulkan.Particles (defaultMain) where

import Control.Lens                 (_1, _3, view, (&), (^.))
import Control.Monad                (when)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Loops          (whileM_)
import Control.Monad.Trans.Resource (release)
import Data.Bits                    (Bits (..))
import Data.ByteString.Char8        qualified as BS
import Data.Foldable                (for_)
import Data.Functor                 ((<&>))
import Data.Kind                    (Type)
import Data.Time                    qualified as Time
import Data.Vector                  (Vector)
import Data.Vector                  qualified as Vector
import Data.Vector.Storable         qualified as SVector
import Data.Word                    (Word32, Word64)
import Graphics.UI.GLFW             qualified as GLFW
import Linear                       qualified as Linear
import System.FilePath              ((<.>), (</>))
import System.Random.Stateful       (StatefulGen)
import System.Random.Stateful       qualified as Random
import UnliftIO.Exception           (assert, catch, throwIO)
import UnliftIO.Foreign             (Ptr, Storable (..), castPtr)
import UnliftIO.IORef               (newIORef, readIORef, writeIORef)
import Vulkan                       qualified as Vk
import Vulkan.CStruct.Extends       (SomeStruct (..), pattern (:&))
import Vulkan.Exception             (VulkanException (..))
import Vulkan.Zero                  (zero)

import Tutorial.Vulkan.Common

type Particles :: Type
data Particles

data instance FrameExtra Particles = FrameExtraParticles
  { computeCommandBuffer :: Vk.CommandBuffer
  , shaderStorageBuffer  :: Vk.Buffer
  , computeDescriptorSet :: Vk.DescriptorSet
  , computeUniformBuffer :: (Vk.Buffer, Vk.DeviceMemory, Ptr UniformBufferObject)
  }

data instance SwapchainExtra Particles = SwapchainExtraParticles

data instance ApplicationExtra Particles = ApplicationExtraParticles
  { semaphore                  :: Vk.Semaphore
  , computeDescriptorSetLayout :: Vk.DescriptorSetLayout
  , computePipelineLayout      :: Vk.PipelineLayout
  , computePipeline            :: Vk.Pipeline
  }

-- | The default number of particles to compute and render.
particleCount :: Int
particleCount = 8192
-- | Creates a descriptor set layout with two layouts:
--
--     1. UBO binding at 0, visible to the vertex stage.
--     2. Combined image sampler binding at 1, visible to the fragment stage.
createDescriptorSetLayout :: (MonadScopedAllocator r m) => Vk.Device -> m Vk.DescriptorSetLayout
createDescriptorSetLayout device = do
  let
    uboLayoutBinding = Vk.DescriptorSetLayoutBinding
      { Vk.binding = 0
      , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , Vk.descriptorCount = 1
      , Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
      , Vk.immutableSamplers = Vector.empty
      }
    combinedImageSamplerBinding = Vk.DescriptorSetLayoutBinding
      { Vk.binding = 1
      , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , Vk.descriptorCount = 1
      , Vk.stageFlags = Vk.SHADER_STAGE_FRAGMENT_BIT
      , Vk.immutableSamplers = Vector.empty
      }
    layoutInfo = (zero :: Vk.DescriptorSetLayoutCreateInfo '[])
      { Vk.bindings = Vector.fromList
        [ uboLayoutBinding
        , combinedImageSamplerBinding
        ]
      }
  Vk.withDescriptorSetLayout device layoutInfo Nothing (allocate' GlobalAllocatorScope)

-- | Creates the full graphics pipeline, rendering the pipeline layout and pipeline:
--
--     * Loads the shader.
--     * Configures the vertices from 'Vertex'.
--     * Configures the rasterizer:
--         * Back-face culling.
--         * Counter-clockwise winding.
--         * Disabled blending.
--         * Dynamic viewport and scissor.
--     * Enables multisampling with the provided sample counts and a value of 0.2 for sample shading.
--     * Sets dynamic rendering.
createGraphicsPipeline
  :: (MonadScopedAllocator r m)
  => Vk.Device -> Vk.DescriptorSetLayout -> Vk.SurfaceFormatKHR
  -> m (Vk.PipelineLayout, Vk.Pipeline)
createGraphicsPipeline device descriptorSetLayout swapchainSurfaceFormat = do
  shaderCode <- liftIO $ BS.readFile ("shaders" </> "particle" <.> "spv")
  (shaderModuleKey, shaderModule) <- createShaderModule device shaderCode
  let
    vertShaderStageInfo = (zero :: Vk.PipelineShaderStageCreateInfo '[])
      { Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
      , Vk.module' = shaderModule
      , Vk.name = "vertMain"
      }
    fragShaderStageInfo = (zero :: Vk.PipelineShaderStageCreateInfo '[])
      { Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
      , Vk.module' = shaderModule
      , Vk.name = "fragMain"
      }
    shaderStages = Vector.fromList [vertShaderStageInfo, fragShaderStageInfo]

    dynamicStates = Vector.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR]
    dynamicState = (zero :: Vk.PipelineDynamicStateCreateInfo){Vk.dynamicStates}

    vertexInputInfo = (zero :: Vk.PipelineVertexInputStateCreateInfo '[])
      { Vk.vertexBindingDescriptions = Vector.singleton getBindingDescription
      , Vk.vertexAttributeDescriptions = getAttributeDescriptions
      }

    inputAssemblyState = (zero :: Vk.PipelineInputAssemblyStateCreateInfo)
      { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_POINT_LIST
      }

    viewportState = (zero :: Vk.PipelineViewportStateCreateInfo '[])
      { Vk.viewportCount = 1
      , Vk.scissorCount = 1
      }

    rasterizer = (zero :: Vk.PipelineRasterizationStateCreateInfo '[])
      { Vk.depthClampEnable = False
      , Vk.rasterizerDiscardEnable = False
      , Vk.polygonMode = Vk.POLYGON_MODE_FILL
      , Vk.cullMode = Vk.CULL_MODE_BACK_BIT
      , Vk.frontFace = Vk.FRONT_FACE_COUNTER_CLOCKWISE
      , Vk.depthBiasEnable = False
      , Vk.lineWidth = 1
      }
    multisampling = (zero :: Vk.PipelineMultisampleStateCreateInfo '[])
      { Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
      , Vk.sampleShadingEnable = False
      }
    colorBlendAttachment = (zero :: Vk.PipelineColorBlendAttachmentState)
      { Vk.blendEnable = True
      , Vk.srcColorBlendFactor = Vk.BLEND_FACTOR_SRC_ALPHA
      , Vk.dstColorBlendFactor = Vk.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
      , Vk.colorBlendOp = Vk.BLEND_OP_ADD
      , Vk.srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
      , Vk.dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
      , Vk.alphaBlendOp = Vk.BLEND_OP_ADD
      , Vk.colorWriteMask =
        Vk.COLOR_COMPONENT_R_BIT
        .|. Vk.COLOR_COMPONENT_G_BIT
        .|. Vk.COLOR_COMPONENT_B_BIT
        .|. Vk.COLOR_COMPONENT_A_BIT
      }
    colorBlending = (zero :: Vk.PipelineColorBlendStateCreateInfo '[])
      { Vk.logicOpEnable = False
      , Vk.logicOp = Vk.LOGIC_OP_COPY
      , Vk.attachmentCount = 1
      , Vk.attachments = Vector.singleton colorBlendAttachment
      }

    pipelineLayoutInfo = (zero :: Vk.PipelineLayoutCreateInfo)
      { Vk.setLayouts = Vector.singleton descriptorSetLayout
      , Vk.pushConstantRanges = Vector.empty
      }
  pipelineLayout <- Vk.withPipelineLayout device pipelineLayoutInfo Nothing (allocate' GlobalAllocatorScope)

  let
    pipelineCreateInfoChain = SomeStruct (zero :: Vk.GraphicsPipelineCreateInfo '[])
      { Vk.stageCount = 2
      , Vk.stages = SomeStruct <$> shaderStages
      , Vk.vertexInputState = Just $ SomeStruct vertexInputInfo
      , Vk.inputAssemblyState = Just inputAssemblyState
      , Vk.viewportState = Just $ SomeStruct viewportState
      , Vk.rasterizationState = Just $ SomeStruct rasterizer
      , Vk.multisampleState = Just $ SomeStruct multisampling
      , Vk.colorBlendState = Just $ SomeStruct colorBlending
      , Vk.dynamicState = Just dynamicState
      , Vk.layout = pipelineLayout
      , Vk.next = (zero :: Vk.PipelineRenderingCreateInfo)
        { Vk.colorAttachmentFormats = Vector.singleton swapchainSurfaceFormat.format
        }
        :& ()
      }

  graphicsPipelines <-
    withResultCheck "Failed to create graphics pipeline" $
      Vk.withGraphicsPipelines
        device
        zero
        (Vector.singleton pipelineCreateInfoChain)
        Nothing
        (allocate' GlobalAllocatorScope)
  let graphicsPipeline = assert (Vector.length graphicsPipelines == 1) (Vector.head graphicsPipelines)

  release shaderModuleKey

  pure (pipelineLayout, graphicsPipeline)

type Particle :: Type
data Particle = Particle
  { position, velocity :: Linear.V2 Float
  , color              :: Linear.V4 Float
  }

getBindingDescription :: Vk.VertexInputBindingDescription
getBindingDescription = Vk.VertexInputBindingDescription
  { Vk.binding = 0
  , Vk.stride = fromIntegral $ sizeOf (undefined :: Particle)
  , Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
  }

getAttributeDescriptions :: Vector Vk.VertexInputAttributeDescription
getAttributeDescriptions = Vector.fromList
  [ Vk.VertexInputAttributeDescription
    { Vk.location = 0, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32_SFLOAT
    , Vk.offset = 0
    }
  , Vk.VertexInputAttributeDescription
    { Vk.location = 1, Vk.binding = 0, Vk.format = Vk.FORMAT_R32G32B32A32_SFLOAT
    , Vk.offset = fromIntegral $ 2 * sizeOf (undefined :: Linear.V2 Float)
    }
  ]

instance Storable Particle where
  sizeOf _ = 2 * sizeOf (undefined :: Linear.V2 Float) + sizeOf (undefined :: Linear.V4 Float)

  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    position <- peek (castPtr ptr)
    velocity <- peekByteOff (castPtr ptr) (sizeOf (undefined :: Linear.V2 Float))
    color <- peekByteOff (castPtr ptr) (2 * sizeOf (undefined :: Linear.V2 Float))
    pure Particle{..}

  poke ptr Particle{..} = do
    poke (castPtr ptr) position
    pokeByteOff (castPtr ptr) (sizeOf (undefined :: Linear.V2 Float)) velocity
    pokeByteOff (castPtr ptr) (2 * sizeOf (undefined :: Linear.V2 Float)) color

type UniformBufferObject :: Type
newtype UniformBufferObject = UniformBufferObject
  { deltaTime :: Float
  }
  deriving newtype (Storable)

updateUniformBuffer :: (MonadApplication Particles r m) => Frame Particles -> Float -> m ()
updateUniformBuffer frame lastFrameTime = do
  let ubo = UniformBufferObject{deltaTime = lastFrameTime * 2}
  liftIO $ poke (frame.extra.computeUniformBuffer & view _3) ubo

createShaderStorageBuffers
  :: (MonadScopedAllocator r m, StatefulGen g m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> Int -> Int -> g
  -> m (Vector Vk.Buffer)
createShaderStorageBuffers physicalDevice device commandPool queue height width random = do
  particles <- SVector.replicateM particleCount do
    rndR <- Random.uniformRM (0, 1) random
    theta <- Random.uniformRM (0, 2 * pi) random
    let
      r = 0.25 * sqrt rndR
      x = r * cos theta * fromIntegral height / fromIntegral width
      y = r * sin theta
    color <- Random.uniformRM (Linear.V3 0 0 0, Linear.V3 1 1 1) random
    pure Particle
      { position = Linear.V2 x y
      , velocity = Linear.normalize (Linear.V2 x y) Linear.^* 0.00025
      , color = Linear.V4 (color ^. Linear._x) (color ^. Linear._y) (color ^. Linear._z) 1
      }
  Vector.replicateM maxFramesInFlight do
    fst <$> createBuffer'
      (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT)
      physicalDevice
      device
      commandPool
      queue
      particles

-- | Creates a descriptor set layout with two layouts visible to the compute shader.
--
-- These layouts represent the last particle binding (read) and current particle binding (write).
createComputeDescriptorSetLayout :: (MonadScopedAllocator r m) => Vk.Device -> m Vk.DescriptorSetLayout
createComputeDescriptorSetLayout device = do
  let
    computeParticleBinding = Vk.DescriptorSetLayoutBinding
      { Vk.binding = 0
      , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , Vk.descriptorCount = 1
      , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
      , Vk.immutableSamplers = Vector.empty
      }
    computeLastParticleBinding = Vk.DescriptorSetLayoutBinding
      { Vk.binding = 1
      , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
      , Vk.descriptorCount = 1
      , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
      , Vk.immutableSamplers = Vector.empty
      }
    computeCurrentParticleBinding = Vk.DescriptorSetLayoutBinding
      { Vk.binding = 2
      , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
      , Vk.descriptorCount = 1
      , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
      , Vk.immutableSamplers = Vector.empty
      }
    layoutInfo = (zero :: Vk.DescriptorSetLayoutCreateInfo '[])
      { Vk.bindings = Vector.fromList
        [ computeParticleBinding
        , computeLastParticleBinding
        , computeCurrentParticleBinding
        ]
      }
  Vk.withDescriptorSetLayout device layoutInfo Nothing (allocate' GlobalAllocatorScope)

createComputeDescriptorSets
  :: (MonadScopedAllocator r m)
  => Vk.Device -> Vk.DescriptorPool -> Vk.DescriptorSetLayout -> Vector Vk.Buffer
  -> Vector Vk.Buffer
  -> m (Vector Vk.DescriptorSet)
createComputeDescriptorSets device descriptorPool descriptorSetLayout uniformBuffers shaderStorageBuffers = do
  let
    layouts = Vector.replicate maxFramesInFlight descriptorSetLayout
    allocInfo = (zero :: Vk.DescriptorSetAllocateInfo '[])
      { Vk.descriptorPool
      , Vk.setLayouts = layouts
      }
  computeDescriptorSets <- Vk.withDescriptorSets device allocInfo (allocate' GlobalAllocatorScope)

  for_ [0 .. maxFramesInFlight - 1] \i -> do
    let
      bufferInfo = Vk.DescriptorBufferInfo
        { Vk.buffer = uniformBuffers Vector.! i
        , Vk.offset = 0
        , Vk.range = fromIntegral $ sizeOf (undefined :: UniformBufferObject)
        }
      storageBufferInfoLastFrame = Vk.DescriptorBufferInfo
        { Vk.buffer = shaderStorageBuffers Vector.! ((i - 1) `mod` maxFramesInFlight)
        , Vk.offset = 0
        , Vk.range = fromIntegral $ sizeOf (undefined :: Particle) * particleCount
        }
      storageBufferInfoCurrentFrame = Vk.DescriptorBufferInfo
        { Vk.buffer = shaderStorageBuffers Vector.! i
        , Vk.offset = 0
        , Vk.range = fromIntegral $ sizeOf (undefined :: Particle) * particleCount
        }
      descriptorWrites = Vector.fromList
        [ SomeStruct (zero :: Vk.WriteDescriptorSet '[])
          { Vk.dstSet = computeDescriptorSets Vector.! i
          , Vk.dstBinding = 0
          , Vk.dstArrayElement = 0
          , Vk.descriptorCount = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , Vk.bufferInfo = Vector.singleton bufferInfo
          }
        , SomeStruct (zero :: Vk.WriteDescriptorSet '[])
          { Vk.dstSet = computeDescriptorSets Vector.! i
          , Vk.dstBinding = 1
          , Vk.dstArrayElement = 0
          , Vk.descriptorCount = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
          , Vk.bufferInfo = Vector.singleton storageBufferInfoLastFrame
          }
        , SomeStruct (zero :: Vk.WriteDescriptorSet '[])
          { Vk.dstSet = computeDescriptorSets Vector.! i
          , Vk.dstBinding = 2
          , Vk.dstArrayElement = 0
          , Vk.descriptorCount = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
          , Vk.bufferInfo = Vector.singleton storageBufferInfoCurrentFrame
          }
        ]
    Vk.updateDescriptorSets device descriptorWrites Vector.empty

  pure computeDescriptorSets

-- | Creates a pool sized to hold 'numObjects' UBO descriptors per in-flight frame.
createComputeDescriptorPool
  :: (MonadScopedAllocator r m) => Vk.Device -> m Vk.DescriptorPool
createComputeDescriptorPool device = do
  let
    uboPoolSize = Vk.DescriptorPoolSize
      { Vk.type' = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , Vk.descriptorCount = fromIntegral maxFramesInFlight
      }
    computePoolSize = Vk.DescriptorPoolSize
      { Vk.type' = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
      , Vk.descriptorCount = fromIntegral maxFramesInFlight * 2
      }
    poolInfo = (zero :: Vk.DescriptorPoolCreateInfo '[])
      { Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
      , Vk.maxSets = fromIntegral maxFramesInFlight
      , Vk.poolSizes = Vector.fromList
        [ uboPoolSize
        , computePoolSize
        ]
      }
  Vk.withDescriptorPool device poolInfo Nothing (allocate' GlobalAllocatorScope)

createComputePipeline
  :: (MonadScopedAllocator r m) => Vk.Device -> Vk.DescriptorSetLayout
  -> m (Vk.PipelineLayout, Vk.Pipeline)
createComputePipeline device computeDescriptorSetLayout = do
  let
    pipelineLayoutInfo = Vk.PipelineLayoutCreateInfo
      { Vk.flags = zeroBits
      , Vk.setLayouts = Vector.singleton computeDescriptorSetLayout
      , Vk.pushConstantRanges = Vector.empty
      }
  computePipelineLayout <-
    Vk.withPipelineLayout device pipelineLayoutInfo Nothing (allocate' GlobalAllocatorScope)
  shaderCode <- liftIO $ BS.readFile ("shaders" </> "particle" <.> "spv")
  (shaderModuleKey, shaderModule) <- createShaderModule device shaderCode
  let
    compShaderStageInfo = (zero :: Vk.PipelineShaderStageCreateInfo '[])
      { Vk.stage = Vk.SHADER_STAGE_COMPUTE_BIT
      , Vk.module' = shaderModule
      , Vk.name = "compMain"
      }
    pipelineInfo = (zero :: Vk.ComputePipelineCreateInfo '[])
      { Vk.flags = zeroBits
      , Vk.stage = SomeStruct compShaderStageInfo
      , Vk.layout = computePipelineLayout
      }
  computePipelines <-
    withResultCheck "Failed to create compute pipeline" $
      Vk.withComputePipelines
        device
        zero
        (Vector.singleton $ SomeStruct pipelineInfo)
        Nothing
        (allocate' GlobalAllocatorScope)

  release shaderModuleKey

  (computePipelineLayout,) <$> Vector.headM computePipelines

recordComputeCommandBuffer
  :: (MonadApplication Particles r m) => Frame Particles -> m ()
recordComputeCommandBuffer frame = do
  ApplicationEnv{extra = ApplicationExtraParticles{computePipeline, computePipelineLayout}} <-
    view applicationEnvL
  let commandBuffer = frame.extra.computeCommandBuffer

  Vk.resetCommandBuffer commandBuffer zero
  Vk.beginCommandBuffer commandBuffer zero

  Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_COMPUTE computePipeline
  Vk.cmdBindDescriptorSets
    commandBuffer
    Vk.PIPELINE_BIND_POINT_COMPUTE
    computePipelineLayout
    0
    (Vector.singleton frame.extra.computeDescriptorSet)
    Vector.empty
  Vk.cmdDispatch commandBuffer (fromIntegral particleCount `div` 256) 1 1

  Vk.endCommandBuffer commandBuffer

-- | Creates the following synchronization objects:
--
--     * A timeline semaphore for the following purposes:
--       * To signal that an image has been acquired from the swapchain and is ready for rendering.
--       * To signal that rendering has finished and presentation can happen.
--       * To signal that that the compute shader has finished and the particles can be read.
--     * Per-frame fences to ensure only one frame is rendered at a time.
createSyncObjects
  :: (MonadScopedAllocator r m) => Vk.Device -> m (Vk.Semaphore, Vector Vk.Fence)
createSyncObjects device = do
  let
    semaphoreType = Vk.SemaphoreTypeCreateInfo
      { Vk.semaphoreType = Vk.SEMAPHORE_TYPE_TIMELINE
      , Vk.initialValue = 0
      }
  semaphore <- Vk.withSemaphore device zero{Vk.next = semaphoreType :& ()} Nothing (allocate' GlobalAllocatorScope)
  inFlightFences <- Vector.replicateM
    maxFramesInFlight
    (Vk.withFence device zero Nothing (allocate' GlobalAllocatorScope))
  pure (semaphore, inFlightFences)

-- | Initializes GLFW, Vulkan, and creates all necessary objects for 'Application'.
initVulkan :: (MonadScopedAllocator r m) => Int -> Int -> m (ApplicationEnv Particles)
initVulkan width height = do
  startTime <- liftIO Time.getCurrentTime
  framebufferResizedRef <- newIORef False
  random <- Random.newIOGenM $ Random.mkStdGen $ floor $ Time.utctDayTime startTime
  window <- initWindow width height framebufferResizedRef
  inst <- createInstance
  _dbgMsgsMb <- setupDebugMessenger inst
  surface <- createSurface inst window
  (physicalDevice, dynamicRenderingSupported) <- pickPhysicalDevice inst
  (device, queue, queueIndex) <- createLogicalDevice physicalDevice surface dynamicRenderingSupported
  (swapchain, swapchainSurfaceFormat, swapchainImages, swapchainExtent) <-
    createSwapchain device physicalDevice surface window
  swapchainImageViews <- createImageViews device swapchainSurfaceFormat swapchainImages
  computeDescriptorSetLayout <- createComputeDescriptorSetLayout device
  descriptorSetLayout <- createDescriptorSetLayout device
  (pipelineLayout, graphicsPipeline) <-
    createGraphicsPipeline device descriptorSetLayout swapchainSurfaceFormat
  (computePipelineLayout, computePipeline) <- createComputePipeline device computeDescriptorSetLayout
  commandPool <- createCommandPool device queueIndex
  shaderStorageBuffers <- createShaderStorageBuffers physicalDevice device commandPool queue width height random
  descriptorPool <- createComputeDescriptorPool device
  computeUniformBuffers <- createUniformBuffers @UniformBufferObject physicalDevice device
  computeDescriptorSets <- do
    let uniformBuffers = Vector.map (view _1) computeUniformBuffers
    createComputeDescriptorSets device descriptorPool computeDescriptorSetLayout uniformBuffers shaderStorageBuffers
  commandBuffers <- createCommandBuffers device commandPool
  computeCommandBuffers <- createCommandBuffers device commandPool
  (semaphore, inFlightFences) <- createSyncObjects device
  let
    frames = Vector.zipWith6
      (\inFlightFence
        commandBuffer
        computeCommandBuffer
        shaderStorageBuffer
        computeDescriptorSet
        computeUniformBuffer -> Frame{extra = FrameExtraParticles{..}, ..})
      inFlightFences
      commandBuffers
      computeCommandBuffers
      shaderStorageBuffers
      computeDescriptorSets
      computeUniformBuffers
  swapchainRef <- newIORef Swapchain
    { swapchain
    , surfaceFormat = swapchainSurfaceFormat
    , images = swapchainImages
    , extent = swapchainExtent
    , imageViews = swapchainImageViews
    , extra = SwapchainExtraParticles
    }
  allocations <- view allocatorEnvL
  pure ApplicationEnv{extra = ApplicationExtraParticles{..}, ..}

-- | Records a full frame.
--
--     * Transitions the image to color attachment.
--     * Begins dynamic rendering (clears to black).
--     * Binds the pipeline.
--     * Sets dynamic viewport/scissor.
--     * Binds vertex/input buffers and the descriptor set.
--     * Draws each game object.
--     * Ends rendering.
--     * Transitions the image to present layout.
recordCommandBuffer :: (MonadApplication Particles r m) => Word32 -> Frame Particles -> m ()
recordCommandBuffer imageIndex frame = do
  ApplicationEnv{..} <- view $ applicationEnvL @Particles
  swapchain <- readIORef swapchainRef
  let image = swapchain.images Vector.! fromIntegral imageIndex

  Vk.resetCommandBuffer frame.commandBuffer zero
  Vk.beginCommandBuffer frame.commandBuffer zero

  transitionImageLayout
    image
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    zero
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.IMAGE_ASPECT_COLOR_BIT
    frame

  let
    clearColor = Vk.Color $ Vk.Float32 0 0 0 1
    renderArea = Vk.Rect2D {Vk.offset = Vk.Offset2D 0 0, Vk.extent = swapchain.extent}
    colorAttachmentInfo = (zero :: Vk.RenderingAttachmentInfo)
      { Vk.imageView = swapchain.imageViews Vector.! fromIntegral imageIndex
      , Vk.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
      , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
      , Vk.clearValue = clearColor
      }
    renderingInfo = (zero :: Vk.RenderingInfo '[])
      { Vk.renderArea
      , Vk.layerCount = 1
      , Vk.colorAttachments = Vector.singleton colorAttachmentInfo
      }
  Vk.cmdBeginRendering frame.commandBuffer renderingInfo

  Vk.cmdBindPipeline frame.commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
  Vk.cmdSetViewport
    frame.commandBuffer
    0
    (Vector.singleton $
      Vk.Viewport 0 0 (fromIntegral swapchain.extent.width) (fromIntegral swapchain.extent.height) 0 1)
  Vk.cmdSetScissor frame.commandBuffer 0 (Vector.singleton (Vk.Rect2D (Vk.Offset2D 0 0) swapchain.extent))
  Vk.cmdBindVertexBuffers frame.commandBuffer 0 (Vector.singleton frame.extra.shaderStorageBuffer) (Vector.singleton 0)
  Vk.cmdDraw frame.commandBuffer (fromIntegral particleCount) 1 0 0

  Vk.cmdEndRendering frame.commandBuffer

  -- Transition swapchain image to present layout
  transitionImageLayout
    image
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    zero
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT
    Vk.IMAGE_ASPECT_COLOR_BIT
    frame

  Vk.endCommandBuffer frame.commandBuffer

-- | Orchestrates one frame:
--
--     * Waits for the in-flight fence.
--     * Acquires a swapchain image.
--     * Resets the fence.
--     * Calls 'updateUniformBuffer' and 'recordCommandBuffer'.
--     * Submits to the queue.
--     * Presents.
--
-- If the swapchain or framebuffer is stale (because the viewport is resized),
-- 'recreateSwapchain' is called and the function returns 'False' to indicate that the frame index should not be advanced.
drawFrame :: (MonadApplication Particles r m) => Int -> Word64 -> Float -> m Bool
drawFrame frameIndex timelineValue lastFrameTime = do
  ApplicationEnv{..} <- view applicationEnvL
  let frame = frames Vector.! frameIndex

  swapchain <- readIORef swapchainRef
  (imageIndexResult, imageIndex) <-
    Vk.acquireNextImageKHR device swapchain.swapchain maxBound zero frame.inFlightFence
      `catch` \exn@(VulkanException r) ->
        if r == Vk.ERROR_OUT_OF_DATE_KHR
        then pure (r, 0)
        else throwIO exn

  Vk.waitForFences device (Vector.singleton frame.inFlightFence) True maxBound
    >>= checkResult "Failed to wait for fence!"
  Vk.resetFences device (Vector.singleton frame.inFlightFence)

  case imageIndexResult of
    Vk.SUCCESS -> continue imageIndex frame swapchain
    Vk.SUBOPTIMAL_KHR -> continue imageIndex frame swapchain
    Vk.ERROR_OUT_OF_DATE_KHR -> False <$ recreateSwapchain
    _ -> do
      checkResult "Failed to acquire swap chain image!" $
        assert (imageIndexResult == Vk.TIMEOUT || imageIndexResult == Vk.NOT_READY) imageIndexResult
      pure False
 where
  continue :: (MonadApplication Particles r m) => Word32 -> Frame Particles -> Swapchain Particles -> m Bool
  continue imageIndex frame swapchain = do
    ApplicationEnv{extra = ApplicationExtraParticles{..}, ..} <- view applicationEnvL

    let
      computeWaitValue = timelineValue
      computeSignalValue = timelineValue + 1
      graphicsWaitValue = computeSignalValue
      graphicsSignalValue = timelineValue + 2

    updateUniformBuffer frame lastFrameTime

    let
      computeTimelineInfo = Vk.TimelineSemaphoreSubmitInfo
        { Vk.waitSemaphoreValueCount = 1
        , Vk.waitSemaphoreValues = Vector.singleton computeWaitValue
        , Vk.signalSemaphoreValueCount = 1
        , Vk.signalSemaphoreValues = Vector.singleton computeSignalValue
        }
      computeSubmitInfo = Vk.SubmitInfo
        { Vk.next = computeTimelineInfo :& ()
        , Vk.waitSemaphores = Vector.singleton semaphore
        , Vk.waitDstStageMask = Vector.singleton Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
        , Vk.commandBuffers = Vector.singleton $ Vk.commandBufferHandle frame.extra.computeCommandBuffer
        , Vk.signalSemaphores = Vector.singleton semaphore
        }
    recordComputeCommandBuffer frame
    Vk.queueSubmit queue (Vector.singleton $ SomeStruct computeSubmitInfo) zero

    let
      graphicsTimelineInfo = Vk.TimelineSemaphoreSubmitInfo
        { Vk.waitSemaphoreValueCount = 1
        , Vk.waitSemaphoreValues = Vector.singleton graphicsWaitValue
        , Vk.signalSemaphoreValueCount = 1
        , Vk.signalSemaphoreValues = Vector.singleton graphicsSignalValue
        }
      graphicsSubmitInfo = Vk.SubmitInfo
        { Vk.next = graphicsTimelineInfo :& ()
        , Vk.waitSemaphores = Vector.singleton semaphore
        , Vk.waitDstStageMask = Vector.singleton Vk.PIPELINE_STAGE_VERTEX_INPUT_BIT
        , Vk.commandBuffers = Vector.singleton $ Vk.commandBufferHandle frame.commandBuffer
        , Vk.signalSemaphores = Vector.singleton semaphore
        }
    recordCommandBuffer imageIndex frame
    Vk.queueSubmit queue (Vector.singleton $ SomeStruct graphicsSubmitInfo) zero

    let
      waitInfo = Vk.SemaphoreWaitInfo
        { Vk.semaphores = Vector.singleton semaphore
        , Vk.values = Vector.singleton graphicsSignalValue
        , Vk.flags = zeroBits
        }
    Vk.waitSemaphoresSafe device waitInfo maxBound >>=
      checkResult "Failed to wait for semaphore!"

    let
      presentInfoKHR = (zero :: Vk.PresentInfoKHR '[])
        { Vk.swapchains = Vector.singleton swapchain.swapchain
        , Vk.imageIndices = Vector.singleton imageIndex
        }

    framebufferResized <- readIORef framebufferResizedRef
    catchOutOfDate (Vk.queuePresentKHR queue presentInfoKHR) >>= \case
      Vk.SUBOPTIMAL_KHR -> do
        writeIORef framebufferResizedRef False
        recreateSwapchain
      Vk.ERROR_OUT_OF_DATE_KHR -> do
        writeIORef framebufferResizedRef False
        recreateSwapchain
      result -> do
        if framebufferResized then do
          writeIORef framebufferResizedRef False
          recreateSwapchain
        else
          pure $ assert (result == Vk.SUCCESS) ()

    pure True

-- | Calls 'cleanupSwapchain' and recreates them, updating the swapchain reference.
--
-- This function should be called whenever the application is resized.
--
-- If the application is minimized, the application is paused.
recreateSwapchain :: (MonadApplication Particles r m) => m ()
recreateSwapchain = do
  ApplicationEnv{..} <- view applicationEnvL

  -- Pause while minimized
  liftIO $ whileM_
    (GLFW.getFramebufferSize window <&> \(width, height) -> width == 0 || height == 0)
    GLFW.waitEvents

  Vk.deviceWaitIdle device

  cleanupSwapchain
  (swapchain, surfaceFormat, images, extent) <- createSwapchain device physicalDevice surface window
  imageViews <- createImageViews device surfaceFormat images
  writeIORef swapchainRef Swapchain{extra = SwapchainExtraParticles{..}, ..}

-- | Helper to get the current time from GLFW. Throws if it couldn't.
getTime :: (MonadIO m) => m Double
getTime =
  liftIO GLFW.getTime >>= \case
    Nothing -> throwIO $ RuntimeError "Failed to get GLFW time"
    Just time -> pure time

-- | While the window should not close, pools events and renders frames.
mainLoop :: (MonadApplication Particles r m) => m ()
mainLoop = do
  ApplicationEnv{window} <- view $ applicationEnvL @Particles
  frameIndexRef <- newIORef 0
  timelineValueRef <- newIORef 0
  lastTimeRef <- newIORef =<< getTime
  lastFrameTimeRef <- newIORef 0
  whileM_ (liftIO $ not <$> GLFW.windowShouldClose window) do
    frameIndex <- readIORef frameIndexRef
    timelineValue <- readIORef timelineValueRef
    lastFrameTime <- readIORef lastFrameTimeRef
    lastTime <- readIORef lastTimeRef
    liftIO GLFW.pollEvents
    shouldIncrementFrameCounter <- drawFrame frameIndex timelineValue lastFrameTime
    writeIORef timelineValueRef (timelineValue + 2)
    -- Animate the particle system using the last frames to get smooth, frame-rate independent animation
    currentTime <- getTime
    writeIORef lastFrameTimeRef (realToFrac $ (currentTime - lastTime) * 1000)
    writeIORef lastTimeRef currentTime
    when shouldIncrementFrameCounter do
      writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

-- | Creates a window and renders the contents from the Vulkan compute tutorial.
defaultMain :: IO ()
defaultMain = mkDefaultMain initVulkan mainLoop
