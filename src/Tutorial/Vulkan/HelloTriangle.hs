module Tutorial.Vulkan.HelloTriangle (defaultMain) where

import Codec.Ktx2                          qualified as Ktx2
import Codec.Ktx2.Header                   qualified as Ktx2
import Codec.Ktx2.Read                     qualified as Ktx2
import Control.Lens                        (view, (&), (+~))
import Control.Monad                       (unless, when)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Control.Monad.Loops                 (whileM_)
import Control.Monad.Trans.Resource        (release)
import Data.Bits                           (Bits (..))
import Data.ByteString.Char8               qualified as BS
import Data.ByteString.Unsafe              (unsafeUseAsCStringLen)
import Data.Foldable                       (for_)
import Data.Functor                        ((<&>))
import Data.Kind                           (Type)
import Data.Maybe                          (isNothing)
import Data.Time                           (UTCTime)
import Data.Time                           qualified as Time
import Data.Traversable                    (for, forAccumM)
import Data.Vector                         (Vector)
import Data.Vector                         qualified as Vector
import Data.Vector.Storable                qualified as SVector
import Data.Word                           (Word32)
import Graphics.UI.GLFW                    qualified as GLFW
import Linear                              qualified as Linear
import Math.NumberTheory.Logarithms        (integerLog2)
import System.FilePath                     ((<.>), (</>))
import Text.GLTF.Loader                    qualified as GLTF
import UnliftIO                            (MonadUnliftIO)
import UnliftIO.Exception                  (assert, catch, throwIO)
import UnliftIO.Foreign                    (Storable (..), castPtr, copyBytes)
import UnliftIO.IORef                      (newIORef, readIORef, writeIORef)
import Vulkan                              qualified as Vk
import Vulkan.CStruct.Extends              (SomeStruct (..), pattern (:&))
import Vulkan.Exception                    (VulkanException (..))
import Vulkan.Zero                         (zero)

import Tutorial.Vulkan.Common
import Tutorial.Vulkan.GameObject          (GameObject (..), modelMatrix)
import Tutorial.Vulkan.UniformBufferObject (UniformBufferObject (..))
import Tutorial.Vulkan.Utils               (findM, perspectiveVulkan)
import Tutorial.Vulkan.Vertex              (Index (..), Vertex (..))
import Tutorial.Vulkan.Vertex              qualified as Vertex

type HelloTriangle :: Type
data HelloTriangle

data instance FrameExtra HelloTriangle = FrameExtraHelloTriangle
  { presentCompleteSemaphore :: Vk.Semaphore
  , frameIndex               :: Word32
  }

data instance SwapchainExtra HelloTriangle = SwapchainExtraHelloTriangle
  { depthImage     :: Vk.Image
  , depthImageView :: Vk.ImageView
  , colorImage     :: Vk.Image
  , colorImageView :: Vk.ImageView
  , framebuffers   :: Vector Vk.Framebuffer
  , renderPassM    :: Maybe Vk.RenderPass
  }

data instance ApplicationExtra HelloTriangle = ApplicationExtraHelloTriangle
  { renderFinishedSemaphores :: Vector Vk.Semaphore
  , vertexBuffer             :: Vk.Buffer
  , vertexBufferMemory       :: Vk.DeviceMemory
  , indexBuffer              :: Vk.Buffer
  , indexBufferMemory        :: Vk.DeviceMemory
  , startTime                :: UTCTime
  , vertices                 :: SVector.Vector Vertex
  , indices                  :: SVector.Vector Index
  , msaaSamples              :: Vk.SampleCountFlagBits
  , gameObjects              :: Vector GameObject
  }

modelPath :: FilePath
modelPath = "khronos-vulkan-tutorial-cpp" </> "attachments" </> "assets" </> "viking_room" <.> "glb"

texturePath :: FilePath
texturePath = "khronos-vulkan-tutorial-cpp" </> "attachments" </> "assets" </> "viking_room" <.> "ktx2"

-- | Creates a render pass, if dynamic rendering is unsupported.
createRenderPass
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.Format -> Vk.SampleCountFlagBits -> Bool
  -> m (Maybe Vk.RenderPass)
createRenderPass physicalDevice device swapchainImageFormat msaaSamples dynamicRenderingSupported
  | dynamicRenderingSupported = pure Nothing
  | otherwise = do
    depthFormat <- findDepthFormat physicalDevice
    let
      colorAttachment = Vk.AttachmentDescription
        { Vk.format = swapchainImageFormat
        , Vk.samples = msaaSamples
        , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        , Vk.flags = zeroBits
        }
      depthAttachment = Vk.AttachmentDescription
        { Vk.format = depthFormat
        , Vk.samples = msaaSamples
        , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        , Vk.flags = zeroBits
        }
      colorAttachmentResolve = Vk.AttachmentDescription
        { Vk.format = swapchainImageFormat
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        , Vk.flags = zeroBits
        }
      -- Subpass reference to the color and depth attachments
      colorAttachmentRef = Vk.AttachmentReference
        { Vk.attachment = 0
        , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      depthAttachmentRef = Vk.AttachmentReference
        { Vk.attachment = 1
        , Vk.layout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        }
      colorAttachmentResolveRef = Vk.AttachmentReference
        { Vk.attachment = 2
        , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
      subpass = (zero :: Vk.SubpassDescription)
        { Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.colorAttachments = Vector.singleton colorAttachmentRef
        , Vk.resolveAttachments = Vector.singleton colorAttachmentResolveRef
        , Vk.depthStencilAttachment = Just depthAttachmentRef
        }
      -- Dependency to ensure proper image layout transitions
      dependency = Vk.SubpassDependency
        { Vk.srcSubpass = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass = zero
        , Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
        , Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
        , Vk.srcAccessMask = Vk.ACCESS_NONE
        , Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT .|. Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
        , Vk.dependencyFlags = zeroBits
        }
      renderPassInfo = (zero :: Vk.RenderPassCreateInfo '[])
        { Vk.attachments = Vector.fromList [colorAttachment, depthAttachment, colorAttachmentResolve]
        , Vk.subpasses = Vector.singleton subpass
        , Vk.dependencies = Vector.singleton dependency
        }
    Just <$> Vk.withRenderPass device renderPassInfo Nothing (allocate' SwapchainAllocatorScope)

-- | If the render pass is provided, creates a swapchain framebuffer.
--
-- Returns an empty vector otherwise.
createFramebuffers
  :: (MonadScopedAllocator r m)
  => Vk.Device -> Vk.ImageView -> Vk.ImageView -> Vector Vk.ImageView
  -> Vk.Extent2D -> Maybe Vk.RenderPass -> m (Vector Vk.Framebuffer)
createFramebuffers device colorImageView depthImageView swapchainImageViews swapchainExtent = \case
  Nothing -> pure Vector.empty
  Just renderPass -> for swapchainImageViews \imageView -> do
    let
      framebufferInfo = (zero :: Vk.FramebufferCreateInfo '[])
        { Vk.renderPass
        , Vk.attachments = Vector.fromList [colorImageView, depthImageView, imageView]
        , Vk.width = swapchainExtent.width
        , Vk.height = swapchainExtent.height
        , Vk.layers = 1
        }
    Vk.withFramebuffer device framebufferInfo Nothing (allocate' SwapchainAllocatorScope)

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
--     * Sets the depth stencil.
createGraphicsPipeline
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.DescriptorSetLayout -> Vk.SurfaceFormatKHR
  -> Vk.SampleCountFlagBits -> Maybe Vk.RenderPass
  -> m (Vk.PipelineLayout, Vk.Pipeline)
createGraphicsPipeline physicalDevice device descriptorSetLayout swapchainSurfaceFormat msaaSamples renderPassM = do
  shaderCode <- liftIO $ BS.readFile ("shaders" </> "triangle" <.> "spv")
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
      { Vk.vertexBindingDescriptions = Vector.singleton Vertex.getBindingDescription
      , Vk.vertexAttributeDescriptions = Vertex.getAttributeDescriptions
      }

    inputAssemblyState = (zero :: Vk.PipelineInputAssemblyStateCreateInfo)
      { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
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
      { Vk.rasterizationSamples = msaaSamples
      , Vk.sampleShadingEnable = True
      , Vk.minSampleShading = 0.2 -- min fraction for sample shading; closer to one is smoother
      }
    colorBlendAttachment = (zero :: Vk.PipelineColorBlendAttachmentState)
      { Vk.blendEnable = False
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

    depthStencil = (zero :: Vk.PipelineDepthStencilStateCreateInfo)
      { Vk.depthTestEnable = True
      , Vk.depthWriteEnable = True
      , Vk.depthCompareOp = Vk.COMPARE_OP_LESS
      , Vk.depthBoundsTestEnable = False
      , Vk.stencilTestEnable = False
      }

    pipelineLayoutInfo = (zero :: Vk.PipelineLayoutCreateInfo)
      { Vk.setLayouts = Vector.singleton descriptorSetLayout
      , Vk.pushConstantRanges = Vector.empty
      }
  pipelineLayout <- Vk.withPipelineLayout device pipelineLayoutInfo Nothing (allocate' GlobalAllocatorScope)

  depthFormat <- findDepthFormat physicalDevice
  let
    pipelineCreateInfoChain = case renderPassM of
      Nothing -> SomeStruct (zero :: Vk.GraphicsPipelineCreateInfo '[])
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
        , Vk.depthStencilState = Just depthStencil
        , Vk.next = (zero :: Vk.PipelineRenderingCreateInfo)
          { Vk.colorAttachmentFormats = Vector.singleton swapchainSurfaceFormat.format
          , Vk.depthAttachmentFormat = depthFormat
          }
          :& ()
        }
      Just renderPass -> SomeStruct (zero :: Vk.GraphicsPipelineCreateInfo '[])
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
        , Vk.renderPass
        , Vk.subpass = 0
        , Vk.depthStencilState = Just depthStencil
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

-- | Creates a color image for use with the color buffer (for multisampling) as well as its image view.
createColorResources
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.Extent2D -> Vk.SurfaceFormatKHR
  -> Vk.SampleCountFlagBits
  -> m (Vk.Image, Vk.ImageView)
createColorResources physicalDevice device swapchainExtent swapchainSurfaceFormat msaaSamples = do
  let colorFormat = swapchainSurfaceFormat.format
  (colorImage, _colorImageMemory) <- createImage
    physicalDevice
    device
    swapchainExtent.width
    swapchainExtent.height
    1
    colorFormat
    Vk.IMAGE_TILING_OPTIMAL
    (Vk.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    msaaSamples
    SwapchainAllocatorScope
  colorImageView <-
    createImageView device colorImage 1 colorFormat Vk.IMAGE_ASPECT_COLOR_BIT SwapchainAllocatorScope
  pure (colorImage, colorImageView)

-- | Creates a depth image for use with a depth stencil as well as its image view.
createDepthResources
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.Extent2D
  -> Vk.SampleCountFlagBits
  -> m (Vk.Image, Vk.ImageView)
createDepthResources physicalDevice device swapchainExtent msaaSamples = do
  depthFormat <- findDepthFormat physicalDevice
  let mipLevels = 1
  (depthImage, _depthImageMemory) <- createImage
    physicalDevice
    device
    swapchainExtent.width
    swapchainExtent.height
    mipLevels
    depthFormat
    Vk.IMAGE_TILING_OPTIMAL
    Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    msaaSamples
    SwapchainAllocatorScope
  depthImageView <-
    createImageView device depthImage mipLevels depthFormat Vk.IMAGE_ASPECT_DEPTH_BIT SwapchainAllocatorScope
  pure (depthImage, depthImageView)

-- | Finds a format among the candidate formats satisfying the image tiling or throws an error if none are supported.
--
-- The only supported formats are linear and optimal.
findSupportedFormat
  :: (MonadIO m)
  => Vk.PhysicalDevice -> Vector Vk.Format -> Vk.ImageTiling -> Vk.FormatFeatureFlags
  -> m Vk.Format
findSupportedFormat physicalDevice candidates tiling features = do
  formatM <- findM
    (\format -> do
      props <- Vk.getPhysicalDeviceFormatProperties physicalDevice format
      pure
        $  tiling == Vk.IMAGE_TILING_LINEAR && (props.linearTilingFeatures .&. features) == features
        || tiling == Vk.IMAGE_TILING_OPTIMAL && (props.optimalTilingFeatures .&. features) == features)
    candidates
  maybe
    (throwIO $ RuntimeError "Failed to find supported format!")
    pure
    formatM

-- | Finds a format that is optimal for the depth stencil.
--
-- See 'findSupportedFormat'.
findDepthFormat :: (MonadIO m) => Vk.PhysicalDevice -> m Vk.Format
findDepthFormat physicalDevice = findSupportedFormat
  physicalDevice
  (Vector.fromList [Vk.FORMAT_D32_SFLOAT, Vk.FORMAT_D32_SFLOAT_S8_UINT, Vk.FORMAT_D24_UNORM_S8_UINT])
  Vk.IMAGE_TILING_OPTIMAL
  Vk.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

-- | Loads a texture from the disk ('texturePath').
--
-- The image is optimized for transfer destination and sampled,
-- created with 'createImage', using optimal tiling on the local device.
--
-- Returns the allocated image, its format, and mip levels.
createTextureImage
  :: (MonadScopedAllocator r m) => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> m (Vk.Image, Vk.Format, Word32)
createTextureImage physicalDevice device commandPool graphicsQueue = do
  ktx2 <- Ktx2.fromFile texturePath
  level <- case ktx2.levels of
    (_, lvl) : _ -> pure lvl
    _            -> throwIO $ RuntimeError "Texture has no levels"
  let
    width = ktx2.header.pixelWidth
    height = ktx2.header.pixelHeight
    imageSize = fromIntegral $ BS.length level
    -- TODO: Proper handling of mip levels.
    -- We currently regenerate mip maps, but the texture may have its own.
    mipLevels = fromIntegral $ integerLog2 (fromIntegral $ max width height) + 1
    format = Vk.Format $ fromIntegral $ ktx2.header.vkFormat
  (stagingBufferReleaseKey, stagingBufferMemoryReleaseKey, stagingBuffer, stagingBufferMemory) <- createBuffer
    physicalDevice
    device
    imageSize
    Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
    (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
  liftIO $ unsafeUseAsCStringLen level \(ptr, len) -> do
    data' <- Vk.mapMemory device stagingBufferMemory 0 imageSize zero
    copyBytes (castPtr data') ptr len
    Vk.unmapMemory device stagingBufferMemory

  (textureImage, _imageMemory) <- createImage
    physicalDevice
    device
    width
    height
    mipLevels
    format
    Vk.IMAGE_TILING_OPTIMAL
    (Vk.IMAGE_USAGE_TRANSFER_SRC_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT)
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    Vk.SAMPLE_COUNT_1_BIT
    GlobalAllocatorScope

  transitionImageLayout'
    device
    commandPool
    graphicsQueue
    textureImage
    mipLevels
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  copyBufferToImage
    device
    commandPool
    graphicsQueue
    stagingBuffer
    textureImage
    width
    height
  -- Transitioned to Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL while generating mipmaps.
  generateMipmaps
    physicalDevice
    device
    commandPool
    graphicsQueue
    textureImage
    format
    width
    height
    mipLevels

  release stagingBufferMemoryReleaseKey
  release stagingBufferReleaseKey

  pure (textureImage, format, mipLevels)

-- | Generates mimaps from the provided image.
--
-- TODO: Implement resizing in software and load multiple levels from a file.
generateMipmaps
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> Vk.Image -> Vk.Format -> Word32 -> Word32 -> Word32
  -> m ()
generateMipmaps physicalDevice device commandPool graphicsQueue image imageFormat texWidth texHeight mipLevels = do
  formatProperties <- Vk.getPhysicalDeviceFormatProperties physicalDevice imageFormat
  unless (formatProperties.optimalTilingFeatures .&. Vk.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT /= zeroBits) do
    throwIO $ RuntimeError "Texture image format does not support linear blitting!"

  let
    barrier = (zero :: Vk.ImageMemoryBarrier '[])
      { Vk.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT
      , Vk.dstAccessMask = Vk.ACCESS_TRANSFER_READ_BIT
      , Vk.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      , Vk.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      , Vk.image
      , Vk.subresourceRange = Vk.ImageSubresourceRange
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = mipLevels - 1
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }
      , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      }

  withSingleTimeCommands device commandPool graphicsQueue \commandBuffer -> do
    let s = (fromIntegral texWidth, fromIntegral texHeight)
    () <$ forAccumM s [1 .. mipLevels - 1] \(mipWidth, mipHeight) i -> do
      let
        barrier' = (barrier :: Vk.ImageMemoryBarrier '[])
          { Vk.subresourceRange = barrier.subresourceRange
            { Vk.baseMipLevel = i - 1
            }
          }
      Vk.cmdPipelineBarrier
        commandBuffer
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        zeroBits
        Vector.empty
        Vector.empty
        (Vector.singleton $ SomeStruct barrier')

      let
        offsets = (Vk.Offset3D 0 0 0, Vk.Offset3D mipWidth mipHeight 1)
        dstOffsets =
          ( Vk.Offset3D 0 0 0
          , Vk.Offset3D
            (if mipWidth > 1 then mipWidth `div` 2 else 1)
            (if mipHeight > 1 then mipHeight `div` 2 else 1)
            1
          )
        blit = Vk.ImageBlit
          { Vk.srcSubresource = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT (i - 1) 0 1
          , Vk.srcOffsets = offsets
          , Vk.dstSubresource = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT i 0 1
          , Vk.dstOffsets = dstOffsets
          }
      Vk.cmdBlitImage
        commandBuffer
        image
        Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        image
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        (Vector.singleton blit)
        Vk.FILTER_LINEAR

      let
        barrier'' = (barrier' :: Vk.ImageMemoryBarrier '[])
          { Vk.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          , Vk.newLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          , Vk.srcAccessMask = Vk.ACCESS_TRANSFER_READ_BIT
          , Vk.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT
          }
      Vk.cmdPipelineBarrier
        commandBuffer
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
        zeroBits
        Vector.empty
        Vector.empty
        (Vector.singleton $ SomeStruct barrier'')

      pure
        ( ( if mipWidth > 1 then mipWidth `div` 2 else mipWidth
          , if mipHeight > 1 then mipHeight `div` 2 else mipHeight
          )
        , ()
        )

    let
      barrier' = (barrier :: Vk.ImageMemoryBarrier '[])
        { Vk.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        , Vk.newLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , Vk.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT
        , Vk.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT
        }
    Vk.cmdPipelineBarrier
      commandBuffer
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
      zeroBits
      Vector.empty
      Vector.empty
      (Vector.singleton $ SomeStruct barrier')

-- | Creates an image view for a texture.
--
-- See 'createImageView'.
createTextureImageView
  :: (MonadScopedAllocator r m) => Vk.Device -> Vk.Image -> Vk.Format -> Word32
  -> m Vk.ImageView
createTextureImageView device textureImage format mipLevels =
  createImageView device textureImage mipLevels format Vk.IMAGE_ASPECT_COLOR_BIT GlobalAllocatorScope

-- | Creates a texture sampler with linear filtering and repeat address mode.
--
-- The anisotropy is enabled with the maximum supported sampler anisotropy.
createTextureSampler :: (MonadScopedAllocator r m) => Vk.Device -> Vk.PhysicalDevice -> m Vk.Sampler
createTextureSampler device physicalDevice = do
  properties <- Vk.getPhysicalDeviceProperties physicalDevice
  let
    samplerInfo = (zero :: Vk.SamplerCreateInfo '[])
      { Vk.magFilter = Vk.FILTER_LINEAR
      , Vk.minFilter = Vk.FILTER_LINEAR
      , Vk.mipmapMode = Vk.SAMPLER_MIPMAP_MODE_LINEAR
      , Vk.addressModeU = Vk.SAMPLER_ADDRESS_MODE_REPEAT
      , Vk.addressModeV = Vk.SAMPLER_ADDRESS_MODE_REPEAT
      , Vk.addressModeW = Vk.SAMPLER_ADDRESS_MODE_REPEAT
      , Vk.anisotropyEnable = True
      , Vk.maxAnisotropy = properties.limits.maxSamplerAnisotropy
      , Vk.compareEnable = False
      , Vk.compareOp = Vk.COMPARE_OP_ALWAYS
      , Vk.borderColor = Vk.BORDER_COLOR_INT_OPAQUE_BLACK
      , Vk.unnormalizedCoordinates = False
      , Vk.mipLodBias = 0
      , Vk.minLod = 0
      , Vk.maxLod = Vk.LOD_CLAMP_NONE
      }
  Vk.withSampler device samplerInfo Nothing (allocate' GlobalAllocatorScope)

-- | Loads 'modelPath' from the disk.
--
-- See 'processGltf'.
loadModel :: (MonadUnliftIO m) => m (SVector.Vector Vertex, SVector.Vector Index)
loadModel =
  GLTF.fromBinaryFile modelPath >>= \case
    Left err -> throwIO $ RuntimeError $ show err
    Right model -> pure $ processGltf model.unGltf

-- | Converts a glTF file into the vertices and indices vectors expected by Vulkan.
processGltf :: GLTF.Gltf -> (SVector.Vector Vertex, SVector.Vector Index)
processGltf GLTF.Gltf{..} =
  let
    (vertices', indices') = Vector.foldl'
      (\acc GLTF.Mesh{..} -> Vector.foldl'
        (\(vs, is) GLTF.MeshPrimitive{..} ->
          let
            baseVertex = fromIntegral $ Vector.length vs
            -- glTF uses a right-handed coordinate system with Y-up, while Vulkan has Y-down.
            vertices = Vector.map
              (\(pos, texCoord) -> Vertex{pos, color, texCoord})
              (Vector.zip meshPrimitivePositions meshPrimitiveTexCoords)
            indices = Vector.map
              (\index -> fromIntegral index + baseVertex)
              meshPrimitiveIndices
          in
          (vs <> vertices, is <> indices))
        acc
        meshPrimitives)
      (Vector.empty, Vector.empty)
      gltfMeshes
  in
  (SVector.fromList $ Vector.toList vertices', SVector.fromList $ Vector.toList indices')
 where
  color = Linear.V3 1 1 1

-- | Creates 3 game objects.
setupGameObjects
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.DescriptorPool -> Vk.DescriptorSetLayout
  -> Vk.Sampler -> Vk.ImageView
  -> m (Vector GameObject)
setupGameObjects physicalDevice device descriptorPool descriptorSetLayout textureSampler textureImageView =
  for transforms \(position, rotation, scale) -> do
    (Vector.unzip3 -> (uniformBuffers, uniformBuffersMemory, uniformBuffersMapped)) <-
      createUniformBuffers physicalDevice device
    descriptorSets <-
      createDescriptorSets device descriptorPool descriptorSetLayout uniformBuffers textureSampler textureImageView
    pure GameObject{..}
 where
  transforms = Vector.fromList
    [ -- Center
      (Linear.V3 0 0 0, Linear.V3 0 0 0, Linear.V3 1 1 1)
    , -- Left
      (Linear.V3 -2 0 -1, Linear.V3 0 (pi / 4) 0, Linear.V3 0.75 0.75 0.75)
    , -- Right
      (Linear.V3 2 0 -1, Linear.V3 0 (pi / -4) 0, Linear.V3 0.75 0.75 0.75)
    ]

-- | Creates a vertex buffer for the input.
createVertexBuffer
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> SVector.Vector Vertex
  -> m (Vk.Buffer, Vk.DeviceMemory)
createVertexBuffer = createBuffer' Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT

-- | Creates an index buffer for the input.
createIndexBuffer
  :: (MonadScopedAllocator r m)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> SVector.Vector Index
  -> m (Vk.Buffer, Vk.DeviceMemory)
createIndexBuffer = createBuffer' Vk.BUFFER_USAGE_INDEX_BUFFER_BIT

-- | Creates a pool sized to hold 'numObjects' UBO descriptors per in-flight frame.
createDescriptorPool
  :: (MonadScopedAllocator r m) => Vk.Device -> Word32 -> m Vk.DescriptorPool
createDescriptorPool device numObjects = do
  let
    uboPoolSize = Vk.DescriptorPoolSize
      { Vk.type' = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , Vk.descriptorCount = fromIntegral maxFramesInFlight * numObjects
      }
    combinedImageSamplerPoolSize = Vk.DescriptorPoolSize
      { Vk.type' = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , Vk.descriptorCount = fromIntegral maxFramesInFlight * numObjects
      }
    poolInfo = (zero :: Vk.DescriptorPoolCreateInfo '[])
      { Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
      , Vk.maxSets = fromIntegral maxFramesInFlight * numObjects
      , Vk.poolSizes = Vector.fromList
        [ uboPoolSize
        , combinedImageSamplerPoolSize
        ]
      }
  Vk.withDescriptorPool device poolInfo Nothing (allocate' GlobalAllocatorScope)

-- | Allocates one descriptor set per in-flight frame.
--
--
-- The following are written into each set's binding:
--
--     1. The uniform buffer at binding 0.
--     2. The texture sampler and image view at binding 1.
createDescriptorSets
  :: (MonadScopedAllocator r m)
  => Vk.Device -> Vk.DescriptorPool -> Vk.DescriptorSetLayout -> Vector Vk.Buffer
  -> Vk.Sampler -> Vk.ImageView
  -> m (Vector Vk.DescriptorSet)
createDescriptorSets device descriptorPool descriptorSetLayout uniformBuffers textureSampler textureImageView = do
  let
    layouts = Vector.replicate maxFramesInFlight descriptorSetLayout
    allocInfo = (zero :: Vk.DescriptorSetAllocateInfo '[])
      { Vk.descriptorPool
      , Vk.setLayouts = layouts
      }
  descriptorSets <- Vk.withDescriptorSets device allocInfo (allocate' GlobalAllocatorScope)

  for_ (Vector.zip descriptorSets uniformBuffers) \(descriptorSet, uniformBuffer) -> do
    let
      bufferInfo = Vk.DescriptorBufferInfo
        { Vk.buffer = uniformBuffer
        , Vk.offset = 0
        , Vk.range = fromIntegral $ sizeOf (undefined :: UniformBufferObject)
        }
      imageInfo = Vk.DescriptorImageInfo
        { Vk.sampler = textureSampler
        , Vk.imageView = textureImageView
        , Vk.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }
      descriptorWrites = Vector.fromList
        [ SomeStruct (zero :: Vk.WriteDescriptorSet '[])
          { Vk.dstSet = descriptorSet
          , Vk.dstBinding = 0
          , Vk.dstArrayElement = 0
          , Vk.descriptorCount = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , Vk.bufferInfo = Vector.singleton bufferInfo
          }
        , SomeStruct (zero :: Vk.WriteDescriptorSet '[])
          { Vk.dstSet = descriptorSet
          , Vk.dstBinding = 1
          , Vk.dstArrayElement = 0
          , Vk.descriptorCount = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , Vk.imageInfo = Vector.singleton imageInfo
          }
        ]
    Vk.updateDescriptorSets device descriptorWrites Vector.empty

  pure descriptorSets

-- | Creates the following synchronization objects:
--
--     * Per-frame semaphores to signal that an image has been acquired from the swapchain and is ready for rendering.
--     * Per-swapchain-image semaphores to signal that rendering has finished and presentation can happen.
--     * Per-frame fences to ensure only one frame is rendered at a time.
createSyncObjects
  :: (MonadScopedAllocator r m)
  => Vk.Device -> Vector Vk.Image
  -> m (Vector Vk.Semaphore, Vector Vk.Semaphore, Vector Vk.Fence)
createSyncObjects device swapchainImages = do
  presentCompleteSemaphores <- Vector.replicateM
    maxFramesInFlight
    (Vk.withSemaphore device zero Nothing (allocate' GlobalAllocatorScope))
  renderFinishedSemaphores <- Vector.replicateM
    (Vector.length swapchainImages)
    (Vk.withSemaphore device zero Nothing (allocate' GlobalAllocatorScope))
  inFlightFences <- Vector.replicateM
    maxFramesInFlight
    (Vk.withFence device zero{Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT} Nothing (allocate' GlobalAllocatorScope))
  pure (presentCompleteSemaphores, renderFinishedSemaphores, inFlightFences)

-- | Initializes GLFW, Vulkan, and creates all necessary objects for 'Application'.
initVulkan :: (MonadScopedAllocator r m) => Int -> Int -> m (ApplicationEnv HelloTriangle)
initVulkan width height = do
  startTime <- liftIO Time.getCurrentTime
  framebufferResizedRef <- newIORef False
  window <- initWindow width height framebufferResizedRef
  inst <- createInstance
  _dbgMsgsMb <- setupDebugMessenger inst
  surface <- createSurface inst window
  (physicalDevice, dynamicRenderingSupported) <- pickPhysicalDevice inst
  msaaSamples <- getMaxUsableSampleCount physicalDevice
  (device, queue, queueIndex) <- createLogicalDevice physicalDevice surface dynamicRenderingSupported
  (swapchain, swapchainSurfaceFormat, swapchainImages, swapchainExtent) <-
    createSwapchain device physicalDevice surface window
  swapchainImageViews <- createImageViews device swapchainSurfaceFormat swapchainImages
  renderPassM <- createRenderPass physicalDevice device swapchainSurfaceFormat.format msaaSamples dynamicRenderingSupported
  descriptorSetLayout <- createDescriptorSetLayout device
  (pipelineLayout, graphicsPipeline) <-
    createGraphicsPipeline physicalDevice device descriptorSetLayout swapchainSurfaceFormat msaaSamples renderPassM
  commandPool <- createCommandPool device queueIndex
  (textureImage, format, mipLevels) <- createTextureImage physicalDevice device commandPool queue
  (colorImage, colorImageView) <-
    createColorResources physicalDevice device swapchainExtent swapchainSurfaceFormat msaaSamples
  (depthImage, depthImageView) <- createDepthResources physicalDevice device swapchainExtent msaaSamples
  framebuffers <-
    createFramebuffers device colorImageView depthImageView swapchainImageViews swapchainExtent renderPassM
  textureImageView <- createTextureImageView device textureImage format mipLevels
  textureSampler <- createTextureSampler device physicalDevice
  (vertices, indices) <- loadModel
  descriptorPool <- createDescriptorPool device 3
  gameObjects <-
    setupGameObjects physicalDevice device descriptorPool descriptorSetLayout textureSampler textureImageView
  (vertexBuffer, vertexBufferMemory) <-
    createVertexBuffer physicalDevice device commandPool queue vertices
  (indexBuffer, indexBufferMemory) <-
    createIndexBuffer physicalDevice device commandPool queue indices
  commandBuffers <- createCommandBuffers device commandPool
  (presentCompleteSemaphores, renderFinishedSemaphores, inFlightFences) <-
    createSyncObjects device swapchainImages
  let
    frames = Vector.zipWith4
      (\presentCompleteSemaphore
        inFlightFence
        commandBuffer
        frameIndex -> Frame{extra = FrameExtraHelloTriangle{..}, ..})
      presentCompleteSemaphores
      inFlightFences
      commandBuffers
      (Vector.generate maxFramesInFlight fromIntegral)
  swapchainRef <- newIORef Swapchain
    { swapchain
    , surfaceFormat = swapchainSurfaceFormat
    , images = swapchainImages
    , extent = swapchainExtent
    , imageViews = swapchainImageViews
    , extra = SwapchainExtraHelloTriangle
      { depthImage
      , depthImageView
      , colorImage
      , colorImageView
      , framebuffers
      , renderPassM
      }
    }
  allocations <- view allocatorEnvL
  pure ApplicationEnv{extra = ApplicationExtraHelloTriangle{..}, ..}

-- | Records a full frame.
--
--     * Transitions the image to color attachment.
--     * Transitions the depth image to depth attachment.
--     * Begins dynamic rendering (clears to black, also clear the depth stencil).
--     * Binds the pipeline.
--     * Sets dynamic viewport/scissor.
--     * Binds vertex/input buffers and the descriptor set.
--     * Draws each game object.
--     * Ends rendering.
--     * Transitions the image to present layout.
recordCommandBuffer
  :: (MonadApplication HelloTriangle r m) => Word32 -> Frame HelloTriangle -> m ()
recordCommandBuffer imageIndex frame = do
  ApplicationEnv{extra = ApplicationExtraHelloTriangle{..}, ..} <- view applicationEnvL
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
  -- Transition the multisampled color image to COLOR_ATTACHMENT_OPTIMAL
  transitionImageLayout
    swapchain.extra.colorImage
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.IMAGE_ASPECT_COLOR_BIT
    frame
  -- Transition the depth image to DEPTH_ATTACHMENT_OPTIMAL
  transitionImageLayout
    swapchain.extra.depthImage
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    Vk.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    Vk.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    (Vk.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT)
    (Vk.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT)
    Vk.IMAGE_ASPECT_DEPTH_BIT
    frame

  let
    clearColor = Vk.Color $ Vk.Float32 0 0 0 1
    clearDepth = Vk.DepthStencil $ Vk.ClearDepthStencilValue 1 0
    renderArea = Vk.Rect2D {Vk.offset = Vk.Offset2D 0 0, Vk.extent = swapchain.extent}

  case swapchain.extra.renderPassM of
    Nothing -> do
      let
        colorAttachmentInfo = (zero :: Vk.RenderingAttachmentInfo)
          { Vk.imageView = swapchain.extra.colorImageView
          , Vk.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , Vk.resolveMode = Vk.RESOLVE_MODE_AVERAGE_BIT
          , Vk.resolveImageView = swapchain.imageViews Vector.! fromIntegral imageIndex
          , Vk.resolveImageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
          , Vk.clearValue = clearColor
          }
        depthAttachmentInfo = (zero :: Vk.RenderingAttachmentInfo)
          { Vk.imageView = swapchain.extra.depthImageView
          , Vk.imageLayout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
          , Vk.clearValue = clearDepth
          }
        renderingInfo = (zero :: Vk.RenderingInfo '[])
          { Vk.renderArea
          , Vk.layerCount = 1
          , Vk.colorAttachments = Vector.singleton colorAttachmentInfo
          , Vk.depthAttachment = Just depthAttachmentInfo
          }
      Vk.cmdBeginRendering frame.commandBuffer renderingInfo
    Just renderPass -> do
      let
        renderPassInfo = (zero :: Vk.RenderPassBeginInfo '[])
          { Vk.renderPass
          , Vk.framebuffer = swapchain.extra.framebuffers Vector.! fromIntegral imageIndex
          , Vk.renderArea
          , Vk.clearValues = Vector.fromList [clearColor, clearDepth]
          }
      Vk.cmdBeginRenderPass frame.commandBuffer renderPassInfo Vk.SUBPASS_CONTENTS_INLINE

  Vk.cmdBindPipeline frame.commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
  Vk.cmdSetViewport
    frame.commandBuffer
    0
    (Vector.singleton $
      Vk.Viewport 0 0 (fromIntegral swapchain.extent.width) (fromIntegral swapchain.extent.height) 0 1)
  Vk.cmdSetScissor frame.commandBuffer 0 (Vector.singleton (Vk.Rect2D (Vk.Offset2D 0 0) swapchain.extent))
  Vk.cmdBindVertexBuffers frame.commandBuffer 0 (Vector.singleton vertexBuffer) (Vector.singleton 0)
  Vk.cmdBindIndexBuffer frame.commandBuffer indexBuffer 0 Vertex.indexType

  for_ gameObjects \GameObject{descriptorSets} -> do
    Vk.cmdBindDescriptorSets
      frame.commandBuffer
      Vk.PIPELINE_BIND_POINT_GRAPHICS
      pipelineLayout
      0
      (Vector.singleton (descriptorSets Vector.! fromIntegral frame.extra.frameIndex))
      Vector.empty
    Vk.cmdDrawIndexed frame.commandBuffer (fromIntegral $ SVector.length indices) 1 0 0 0

  case swapchain.extra.renderPassM of
    Nothing -> do
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
    Just _renderPass -> Vk.cmdEndRenderPass frame.commandBuffer

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
drawFrame :: (MonadApplication HelloTriangle r m) => Int -> m Bool
drawFrame frameIndex = do
  ApplicationEnv{..} <- view applicationEnvL
  let frame = frames Vector.! frameIndex
  let drawFences = Vector.singleton frame.inFlightFence
  Vk.waitForFences device drawFences True maxBound
    >>= checkResult "Failed to wait for fence!"

  swapchain <- readIORef swapchainRef
  (imageIndexResult, imageIndex) <-
    Vk.acquireNextImageKHR device swapchain.swapchain maxBound frame.extra.presentCompleteSemaphore zero
      `catch` \exn@(VulkanException r) ->
        if r == Vk.ERROR_OUT_OF_DATE_KHR
        then pure (r, 0)
        else throwIO exn
  case imageIndexResult of
    Vk.SUCCESS -> continue drawFences imageIndex frame swapchain
    Vk.SUBOPTIMAL_KHR -> continue drawFences imageIndex frame swapchain
    Vk.ERROR_OUT_OF_DATE_KHR -> False <$ recreateSwapchain
    _ -> do
      checkResult "Failed to acquire swap chain image!" $
        assert (imageIndexResult == Vk.TIMEOUT || imageIndexResult == Vk.NOT_READY) imageIndexResult
      pure False
 where
  continue
    :: (MonadApplication HelloTriangle r m)
    => Vector Vk.Fence -> Word32 -> Frame HelloTriangle -> Swapchain HelloTriangle
    -> m Bool
  continue drawFences imageIndex frame swapchain = do
    ApplicationEnv{extra = ApplicationExtraHelloTriangle{..}, ..} <- view applicationEnvL

    updateUniformBuffers frame

    -- Only reset the fence if we are submitting work
    Vk.resetFences device drawFences

    recordCommandBuffer imageIndex frame

    let
      waitDestinationStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      renderFinishedSemaphore = Vector.singleton $ renderFinishedSemaphores Vector.! fromIntegral imageIndex
      submitInfo = (zero :: Vk.SubmitInfo '[])
        { Vk.waitSemaphores = Vector.singleton frame.extra.presentCompleteSemaphore
        , Vk.waitDstStageMask = Vector.singleton waitDestinationStageMask
        , Vk.commandBuffers = Vector.singleton $ Vk.commandBufferHandle frame.commandBuffer
        , Vk.signalSemaphores = renderFinishedSemaphore
        }
    Vk.queueSubmit queue (Vector.singleton $ SomeStruct submitInfo) frame.inFlightFence

    let
      presentInfoKHR = (zero :: Vk.PresentInfoKHR '[])
        { Vk.waitSemaphores = renderFinishedSemaphore
        , Vk.swapchains = Vector.singleton swapchain.swapchain
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

-- | Computes the MVP matrices for each game object and writes the result into the object's 'uniformBufferMapped'.
--
-- The models are rotated around the Y axis at 0.1 radians per second.
--
-- The camera is located at (2, 2, 6), and looks towards the origin.
--
-- The FOV is set at 45 degrees and y-flipped for Vulkan.
updateUniformBuffers :: (MonadApplication HelloTriangle r m) => Frame HelloTriangle -> m ()
updateUniformBuffers frame = do
  ApplicationEnv{swapchainRef, extra = ApplicationExtraHelloTriangle{startTime, gameObjects}} <-
    view applicationEnvL
  currentTime <- liftIO Time.getCurrentTime
  swapchain <- readIORef swapchainRef
  let
    time = realToFrac $ Time.diffUTCTime currentTime startTime
    -- Need to transpose the matrices to column-major.
    initialRotation =
      Linear.transpose $ Linear.m33_to_m44 $ Linear.fromQuaternion $ Linear.axisAngle (Linear.V3 1 0 0) (pi / 2)
    view' = Linear.transpose $ Linear.lookAt (Linear.V3 2 2 6) (Linear.V3 0 0 0) (Linear.V3 0 0 1)
    proj = Linear.transpose $ perspectiveVulkan
      (pi / 4)
      (realToFrac swapchain.extent.width / realToFrac swapchain.extent.height)
      0.1
      20
  for_ gameObjects \gameObject -> do
    let
      rotationY = gameObject.rotation & Linear._y +~ 0.1 * time
      continuousRotation = modelMatrix gameObject{rotation = rotationY}
      model = continuousRotation Linear.!*! initialRotation
      ubo = UniformBufferObject{view = view', ..}
    liftIO $ poke (gameObject.uniformBuffersMapped Vector.! fromIntegral frame.extra.frameIndex) ubo

-- | Calls 'cleanupSwapchain' and recreates them, updating the swapchain reference.
--
-- This function should be called whenever the application is resized.
--
-- If the application is minimized, the application is paused.
recreateSwapchain :: (MonadApplication HelloTriangle r m) => m ()
recreateSwapchain = do
  ApplicationEnv{extra = ApplicationExtraHelloTriangle{..}, ..} <- view applicationEnvL
  oldSwapchain <- readIORef swapchainRef

  -- Pause while minimized
  liftIO $ whileM_
    (GLFW.getFramebufferSize window <&> \(width, height) -> width == 0 || height == 0)
    GLFW.waitEvents

  Vk.deviceWaitIdle device

  cleanupSwapchain
  (swapchain, surfaceFormat, images, extent) <- createSwapchain device physicalDevice surface window
  imageViews <- createImageViews device surfaceFormat images
  renderPassM <-
    createRenderPass physicalDevice device surfaceFormat.format msaaSamples (isNothing oldSwapchain.extra.renderPassM)
  (colorImage, colorImageView) <- createColorResources physicalDevice device extent surfaceFormat msaaSamples
  (depthImage, depthImageView) <- createDepthResources physicalDevice device extent msaaSamples
  framebuffers <- createFramebuffers device colorImageView depthImageView imageViews extent renderPassM
  writeIORef swapchainRef Swapchain{extra = SwapchainExtraHelloTriangle{..}, ..}

-- | While the window should not close, pools events and renders frames.
mainLoop :: (MonadApplication HelloTriangle r m) => m ()
mainLoop = do
  ApplicationEnv{window} <- view $ applicationEnvL @HelloTriangle
  frameIndexRef <- newIORef 0
  whileM_ (liftIO $ not <$> GLFW.windowShouldClose window) do
    frameIndex <- readIORef frameIndexRef
    liftIO GLFW.pollEvents
    shouldIncrementFrameCounter <- drawFrame frameIndex
    when shouldIncrementFrameCounter do
      writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

-- | Creates a window and renders the contents from the Vulkan tutorial.
defaultMain :: IO ()
defaultMain = mkDefaultMain initVulkan mainLoop
