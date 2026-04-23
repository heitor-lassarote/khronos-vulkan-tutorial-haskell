module Tutorial.Vulkan.HelloTriangle (defaultMain) where

import Control.Monad                (unless, when)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Loops          (whileM_)
import Control.Monad.Reader         (MonadReader (..), ReaderT (..))
import Control.Monad.Trans          (lift)
import Control.Monad.Trans.Resource
  (MonadResource, ReleaseKey, ResIO, allocate, allocate_, release, runResourceT)
import Data.Bits                    (Bits (..))
import Data.ByteString.Char8        (ByteString)
import Data.ByteString.Char8        qualified as BS
import Data.Foldable                (find, for_, traverse_)
import Data.Functor                 ((<&>))
import Data.Kind                    (Type)
import Data.Maybe                   (fromJust, fromMaybe, isJust)
import Data.Ord                     (clamp)
import Data.Vector                  (Vector)
import Data.Vector                  qualified as Vector
import Data.Vector.Storable         qualified as SVector
import Data.Word                    (Word32)
import Foreign                      (castPtr)
import Graphics.UI.GLFW             qualified as GLFW
import Linear                       qualified as Linear
import System.FilePath              ((<.>), (</>))
import System.IO                    (hPutStrLn, stderr)
import UnliftIO                     (IORef, MonadUnliftIO)
import UnliftIO.Exception
  (Exception (displayException), SomeException, assert, catch, finally, throwIO)
import UnliftIO.Foreign
  (Storable (..), alloca, nullPtr, peekCString)
import UnliftIO.IORef               (newIORef, readIORef, writeIORef)
import Vulkan                       qualified as Vk
import Vulkan.CStruct.Extends       (SomeStruct (..), pattern (:&))
import Vulkan.Exception             (VulkanException (..))
import Vulkan.Zero                  (zero)

import Tutorial.Vulkan.Utils        (iFindIndex, iFindIndexM)
import Tutorial.Vulkan.Vertex       (Vertex (..))
import Tutorial.Vulkan.Vertex       qualified as Vertex
import UnliftIO.Foreign             (copyBytes)

foreign import ccall unsafe "debug_callback.c &debug_callback"
  debugCallbackPtr :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT

type Frame :: Type
data Frame = Frame
  { presentCompleteSemaphore :: Vk.Semaphore
  , inFlightFence            :: Vk.Fence
  , commandBuffer            :: Vk.CommandBuffer
  }

type Swapchain :: Type
data Swapchain = Swapchain
  { swapchain             :: Vk.SwapchainKHR
  , surfaceFormat         :: Vk.SurfaceFormatKHR
  , releaseKey            :: ReleaseKey
  , images                :: Vector Vk.Image
  , extent                :: Vk.Extent2D
  , imageViews            :: Vector Vk.ImageView
  , imageViewsReleaseKeys :: Vector ReleaseKey
  }

type Application :: Type
data Application = Application
  { window                   :: GLFW.Window
  , frames                   :: Vector Frame
  , surface                  :: Vk.SurfaceKHR
  , physicalDevice           :: Vk.PhysicalDevice
  , device                   :: Vk.Device
  , queue                    :: Vk.Queue
  , swapchainRef             :: IORef Swapchain
  , renderFinishedSemaphores :: Vector Vk.Semaphore
  , graphicsPipeline         :: Vk.Pipeline
  , framebufferResizedRef    :: IORef Bool
  , vertexBuffer             :: Vk.Buffer
  , vertexBufferMemory       :: Vk.DeviceMemory
  }

type MonadApplication :: Type -> Type
newtype MonadApplication a = MonadApplication
  { runMonadApplication :: ReaderT Application ResIO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadUnliftIO)

instance MonadReader Application MonadApplication where
  ask = MonadApplication ask
  local f = MonadApplication . local f . (.runMonadApplication)

type RuntimeError :: Type
newtype RuntimeError = RuntimeError String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

checkResult :: (MonadIO io) => String -> Vk.Result -> io ()
checkResult _ Vk.SUCCESS = pure ()
checkResult message result =
  throwIO $ RuntimeError $ "[" <> show result <> "] " <> message

withResultCheck :: (MonadIO io) => String -> io (Vk.Result, a) -> io a
withResultCheck msg action = do
  (result, ret) <- action
  checkResult msg result
  pure ret

defaultWidth, defaultHeight :: Int
defaultWidth = 800
defaultHeight = 600

maxFramesInFlight :: Int
maxFramesInFlight = 2

allocate' :: IO a -> (a -> IO ()) -> ResIO a
allocate' create destroy = snd <$> allocate create destroy

initWindow :: Int -> Int -> IORef Bool -> ResIO GLFW.Window
initWindow width height framebufferResizedRef = do
  _glfwKeyReleaseKey <- allocate_ GLFW.init GLFW.terminate

  liftIO $ GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  liftIO $ GLFW.windowHint $ GLFW.WindowHint'Resizable True
  window <- allocate'
    (GLFW.createWindow width height "Vulkan" Nothing Nothing >>= \case
      Nothing -> throwIO $ RuntimeError "Could not create window"
      Just window -> pure window)
    GLFW.destroyWindow

  liftIO $ GLFW.setFramebufferSizeCallback window $ Just \_window _width _height ->
    writeIORef framebufferResizedRef True

  pure window

makeVersion :: Word32 -> Word32 -> Word32 -> Word32
makeVersion major minor patch = major `shift` 22 .|. minor `shift` 12 .|. patch

getRequiredInstanceExtensions :: Bool -> IO (Vector ByteString)
getRequiredInstanceExtensions enableValidationLayers = do
  glfwExtensions <-
    traverse (fmap BS.pack . peekCString) . Vector.fromList =<< GLFW.getRequiredInstanceExtensions
  extensionProperties <-
    withResultCheck "Error enumerating instance extension properties" $
      Vk.enumerateInstanceExtensionProperties Nothing

  for_ glfwExtensions \glfwExtension ->
    unless (any ((== glfwExtension) . Vk.extensionName) extensionProperties) do
      throwIO $ RuntimeError $ "Unsupported GLFW extension: " <> BS.unpack glfwExtension

  pure $ (if enableValidationLayers then Vector.cons Vk.EXT_DEBUG_UTILS_EXTENSION_NAME else id) glfwExtensions

createInstance :: Bool -> ResIO Vk.Instance
createInstance enableValidationLayers = do
  when enableValidationLayers do
    layerNames <-
      withResultCheck "Error enumerating instance layer properties" $
        (fmap . fmap . fmap) (.layerName) Vk.enumerateInstanceLayerProperties
    case find (`notElem` layerNames) requiredLayers of
      Nothing -> pure ()
      Just layerName -> throwIO $ RuntimeError $ "Unsupported required layer: " <> BS.unpack layerName

  glfwExtensions <- liftIO $ getRequiredInstanceExtensions enableValidationLayers

  let
    appInfo = zero
      { Vk.applicationInfo = Just Vk.ApplicationInfo
        { Vk.applicationName = Just "Hello Triangle"
        , Vk.applicationVersion = makeVersion 1 0 0
        , Vk.engineName = Just "No Engine"
        , Vk.engineVersion = makeVersion 1 0 0
        , Vk.apiVersion = Vk.API_VERSION_1_3
        }
      , Vk.enabledExtensionNames = glfwExtensions
      , Vk.enabledLayerNames = requiredLayers
      }

  Vk.withInstance appInfo Nothing allocate'
 where
  requiredLayers
    | enableValidationLayers = Vector.singleton "VK_LAYER_KHRONOS_validation"
    | otherwise              = Vector.empty

setupDebugMessenger :: Bool -> Vk.Instance -> ResIO (Maybe Vk.DebugUtilsMessengerEXT)
setupDebugMessenger enableValidationLayers inst =
  if enableValidationLayers then do
    let
      createInfo = zero
        { Vk.messageSeverity = severityFlags
        , Vk.messageType = messageTypeFlags
        , Vk.pfnUserCallback = debugCallbackPtr
        }
    Just <$> Vk.withDebugUtilsMessengerEXT inst createInfo Nothing allocate'
  else
    pure Nothing
 where
  severityFlags =
    Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT

  messageTypeFlags =
    Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
    .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT

pickPhysicalDevice :: Vk.Instance -> IO Vk.PhysicalDevice
pickPhysicalDevice inst = do
  physicalDevices <-
    withResultCheck "Error enumerating physical devices" $
      Vk.enumeratePhysicalDevices inst
  when (Vector.null physicalDevices) do
    throwIO $ RuntimeError "Failed to find GPUs with Vulkan support"
  devices <- flip Vector.filterM physicalDevices \physicalDevice -> do
    properties <- Vk.getPhysicalDeviceProperties physicalDevice
    features <- Vk.getPhysicalDeviceFeatures2
      @'[ Vk.PhysicalDeviceVulkan11Features
        , Vk.PhysicalDeviceVulkan13Features
        , Vk.PhysicalDeviceExtendedDynamicStateFeaturesEXT
        ]
      physicalDevice
    queueFamilies <- Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice

    availableDeviceExtensions <-
      withResultCheck "Error enumerating device extension properties" $
        Vk.enumerateDeviceExtensionProperties physicalDevice Nothing
    let
      supportsVulkan1_3 = properties.apiVersion >= Vk.API_VERSION_1_3
      supportsGraphics = isJust $ find
        (\qfp -> qfp.queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits)
        queueFamilies
      requiredDeviceExtensions = Vector.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME
      supportsAllRequiredExtensions = all
        (\requiredDeviceExtension -> isJust $ find
          (\availableDeviceExtension -> availableDeviceExtension.extensionName == requiredDeviceExtension)
          availableDeviceExtensions)
        requiredDeviceExtensions
      (physicalDeviceVulkan11Features
        :& physicalDeviceVulkan13Features
        :& physicalDeviceExtendedDynamicStateFeatures
        :& ()) = features.next
      supportsRequiredFeatures =
        physicalDeviceVulkan11Features.shaderDrawParameters
        && physicalDeviceVulkan13Features.dynamicRendering
        && physicalDeviceVulkan13Features.synchronization2
        && physicalDeviceExtendedDynamicStateFeatures.extendedDynamicState

    pure $ supportsVulkan1_3 && supportsGraphics && supportsAllRequiredExtensions && supportsRequiredFeatures

  when (Vector.null devices) do
    throwIO $ RuntimeError "failed to find a suitable GPU!"
  Vector.headM devices

createLogicalDevice :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> ResIO (Vk.Device, Vk.Queue, Word32)
createLogicalDevice physicalDevice surface = do
  queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice

  -- Find queue family property that supports both graphics and present
  queueIndexMb <- iFindIndexM
    (\i qfp -> do
      hasSurfaceSupport <- Vk.getPhysicalDeviceSurfaceSupportKHR physicalDevice (fromIntegral i) surface
      pure $ (qfp.queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits) && hasSurfaceSupport)
    queueFamilyProperties
  queueIndex <- maybe
    (throwIO $ RuntimeError "Could not find a queue for graphics and present -> terminating")
    (pure . fromIntegral)
    queueIndexMb

  let
    graphicsIndex = fromJust $ Vector.findIndex
      (\qfp -> qfp.queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits)
      queueFamilyProperties
    queuePriority = 0.5
    deviceQueueCreateInfo = (zero :: Vk.DeviceQueueCreateInfo '[])
      { Vk.queueFamilyIndex = fromIntegral graphicsIndex
      , Vk.queuePriorities = Vector.singleton queuePriority
      }
    featureChain =
      (zero :: Vk.PhysicalDeviceFeatures2 '[])
      :& (zero :: Vk.PhysicalDeviceVulkan11Features){Vk.shaderDrawParameters = True}
      :& (zero :: Vk.PhysicalDeviceVulkan13Features)
        { Vk.dynamicRendering = True
        , Vk.synchronization2 = True
        }
      :& (zero :: Vk.PhysicalDeviceExtendedDynamicStateFeaturesEXT){Vk.extendedDynamicState = True}
      :& ()
    requiredDeviceExtensions = Vector.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME
    deviceCreateInfo = (zero :: Vk.DeviceCreateInfo '[])
      { Vk.next = featureChain
      , Vk.queueCreateInfos = Vector.singleton $ SomeStruct deviceQueueCreateInfo
      , Vk.enabledExtensionNames = requiredDeviceExtensions
      }
  device <- Vk.withDevice physicalDevice deviceCreateInfo Nothing allocate'
  graphicsQueue <- Vk.getDeviceQueue device (fromIntegral graphicsIndex) queueIndex
  pure (device, graphicsQueue, queueIndex)

createSurface :: Vk.Instance -> GLFW.Window -> ResIO Vk.SurfaceKHR
createSurface inst window = do
  allocate'
    (alloca \surfPtr ->
      GLFW.createWindowSurface @Int (Vk.instanceHandle inst) window nullPtr surfPtr >>= \case
        0 -> peek surfPtr
        r -> throwIO $ RuntimeError $ "Failed to create window surface: " <> show r)
    (flip (Vk.destroySurfaceKHR inst) Nothing)

chooseSwapSurfaceFormat :: Vector Vk.SurfaceFormatKHR -> Vk.SurfaceFormatKHR
chooseSwapSurfaceFormat availableFormats = assert
  (not $ Vector.null availableFormats)
  (fromMaybe (Vector.unsafeHead availableFormats) srgb)
 where
  srgb = find
    (\format -> format.format == Vk.FORMAT_B8G8R8A8_SRGB && format.colorSpace == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR)
    availableFormats

chooseSwapPresentMode :: Vector Vk.PresentModeKHR -> Vk.PresentModeKHR
chooseSwapPresentMode availablePresentModes = assert
  (fifo `elem` availablePresentModes)
  (if mailbox `elem` availablePresentModes then mailbox else fifo)
 where
  mailbox = Vk.PRESENT_MODE_MAILBOX_KHR
  fifo = Vk.PRESENT_MODE_FIFO_KHR

chooseSwapExtent :: (MonadIO io) => Vk.SurfaceCapabilitiesKHR -> GLFW.Window -> io Vk.Extent2D
chooseSwapExtent capabilities window
  | capabilities.currentExtent.width /= maxBound = pure capabilities.currentExtent
  | otherwise = do
    (fromIntegral -> width, fromIntegral -> height) <- liftIO $ GLFW.getFramebufferSize window
    pure Vk.Extent2D
      { width = clamp (capabilities.minImageExtent.width, capabilities.maxImageExtent.width) width
      , height = clamp (capabilities.minImageExtent.height, capabilities.maxImageExtent.height) height
      }

chooseSwapMinImageCount :: Vk.SurfaceCapabilitiesKHR -> Word32
chooseSwapMinImageCount capabilities
  -- 0 means that there is no maximum
  | 0 < capabilities.maxImageCount && capabilities.maxImageCount < minImageCount = capabilities.maxImageCount
  | otherwise = minImageCount
 where
  minImageCount = max 3 capabilities.minImageCount

createSwapchain
  :: Vk.Device
  -> Vk.PhysicalDevice
  -> Vk.SurfaceKHR
  -> GLFW.Window
  -> ResIO (ReleaseKey, Vk.SwapchainKHR, Vk.SurfaceFormatKHR, Vector Vk.Image, Vk.Extent2D)
createSwapchain device physicalDevice surface window = do
  surfaceCapabilities <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  swapchainExtent <- chooseSwapExtent surfaceCapabilities window
  let minImageCount = chooseSwapMinImageCount surfaceCapabilities
  availableFormats <-
    withResultCheck "Failed to get physical device surface formats" $
      Vk.getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
  availablePresentModes <-
    withResultCheck "Failed to get physical device present modes" $
      Vk.getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface
  let
    presentMode = chooseSwapPresentMode availablePresentModes
    swapchainSurfaceFormat = chooseSwapSurfaceFormat availableFormats
    swapchainCreateInfo = (zero :: Vk.SwapchainCreateInfoKHR '[])
      { Vk.surface
      , Vk.minImageCount
      , Vk.imageFormat = swapchainSurfaceFormat.format
      , Vk.imageColorSpace = swapchainSurfaceFormat.colorSpace
      , Vk.imageExtent = swapchainExtent
      , Vk.imageArrayLayers = 1
      , Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      , Vk.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE
      , Vk.preTransform = surfaceCapabilities.currentTransform
      , Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      , Vk.presentMode
      , Vk.clipped = True
      }
  (swapchainReleaseKey, swapchain) <- allocate
    (Vk.createSwapchainKHR device swapchainCreateInfo Nothing)
    (flip (Vk.destroySwapchainKHR device) Nothing)
  swapchainImages <-
    withResultCheck "Failed to get swapchain images" $
      Vk.getSwapchainImagesKHR device swapchain
  pure (swapchainReleaseKey, swapchain, swapchainSurfaceFormat, swapchainImages, swapchainExtent)

createImageViews
  :: Vk.SurfaceFormatKHR
  -> Vector Vk.Image
  -> Vk.Device
  -> ResIO (Vector ReleaseKey, Vector Vk.ImageView)
createImageViews swapchainSurfaceFormat swapchainImages device = do
  let
    imageViewCreateInfo = (zero :: Vk.ImageViewCreateInfo '[])
      { Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
      , Vk.format = swapchainSurfaceFormat.format
      , Vk.subresourceRange = Vk.ImageSubresourceRange
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = 0
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }
      }
  Vector.unzip <$> traverse
    (\image -> allocate
      (Vk.createImageView device imageViewCreateInfo{Vk.image} Nothing)
      (flip (Vk.destroyImageView device) Nothing))
    swapchainImages

createShaderModule :: Vk.Device -> ByteString -> ResIO (ReleaseKey, Vk.ShaderModule)
createShaderModule device code = do
  let createInfo = (zero :: Vk.ShaderModuleCreateInfo '[]){Vk.code}
  allocate
    (Vk.createShaderModule device createInfo Nothing)
    (flip (Vk.destroyShaderModule device) Nothing)

vertices :: SVector.Vector Vertex
vertices = SVector.fromList
  [ Vertex (Linear.V2  0  -0.5) (Linear.V3 1 0 0)
  , Vertex (Linear.V2  0.5 0.5) (Linear.V3 0 1 0)
  , Vertex (Linear.V2 -0.5 0.5) (Linear.V3 0 0 1)
  ]

createGraphicsPipeline ::
  Vk.Device -> Vk.Extent2D -> Vk.SurfaceFormatKHR -> ResIO Vk.Pipeline
createGraphicsPipeline device _swapchainExtent swapchainSurfaceFormat = do
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
      , Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
      , Vk.depthBiasEnable = False
      , Vk.lineWidth = 1
      }
    multisampling = (zero :: Vk.PipelineMultisampleStateCreateInfo '[])
      { Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
      , Vk.sampleShadingEnable = False
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

    pipelineLayoutInfo = zero :: Vk.PipelineLayoutCreateInfo
  pipelineLayout <- Vk.withPipelineLayout device pipelineLayoutInfo Nothing allocate'

  let
    pipelineCreateInfoChain = (zero :: Vk.GraphicsPipelineCreateInfo '[])
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
      , Vk.renderPass = Vk.NULL_HANDLE
      , Vk.next = (zero :: Vk.PipelineRenderingCreateInfo)
        { Vk.colorAttachmentFormats = Vector.singleton swapchainSurfaceFormat.format
        }
        :& ()
      }

  graphicsPipelines <-
    withResultCheck "Failed to create graphics pipeline" $
      Vk.withGraphicsPipelines device zero (Vector.singleton $ SomeStruct pipelineCreateInfoChain) Nothing allocate'
  let graphicsPipeline = assert (Vector.length graphicsPipelines == 1) (Vector.head graphicsPipelines)

  release shaderModuleKey

  pure graphicsPipeline

createCommandPool :: Vk.Device -> Word32 -> ResIO Vk.CommandPool
createCommandPool device queueIndex = do
  let
    poolInfo = (zero :: Vk.CommandPoolCreateInfo)
      { Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
      , Vk.queueFamilyIndex = queueIndex
      }
  Vk.withCommandPool device poolInfo Nothing allocate'

createBuffer
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.DeviceSize -> Vk.BufferUsageFlags
  -> Vk.MemoryPropertyFlags -> ResIO (ReleaseKey, ReleaseKey, Vk.Buffer, Vk.DeviceMemory)
createBuffer physicalDevice device size usage properties = do
  let
    bufferInfo = (zero :: Vk.BufferCreateInfo '[])
      { Vk.size
      , Vk.usage
      , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
      }
  (bufferReleaseKey, buffer) <- allocate
    (Vk.createBuffer device bufferInfo Nothing)
    (flip (Vk.destroyBuffer device) Nothing)

  memRequirements <- Vk.getBufferMemoryRequirements device buffer
  memTypeIndex <- findMemoryType physicalDevice memRequirements.memoryTypeBits properties
  let
    memoryAllocateInfo = (zero :: Vk.MemoryAllocateInfo '[])
      { Vk.allocationSize = memRequirements.size
      , Vk.memoryTypeIndex = memTypeIndex
      }
  (bufferMemoryReleaseKey, bufferMemory) <- allocate
    (Vk.allocateMemory device memoryAllocateInfo Nothing)
    (flip (Vk.freeMemory device) Nothing)

  Vk.bindBufferMemory device buffer bufferMemory 0

  pure (bufferReleaseKey, bufferMemoryReleaseKey, buffer, bufferMemory)

createVertexBuffer
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue -> ResIO (Vk.Buffer, Vk.DeviceMemory)
createVertexBuffer physicalDevice device commandPool graphicsQueue = do
  let bufferSize = sizeOf (undefined :: Vertex) * SVector.length vertices

  (stagingBufferReleaseKey, stagingBufferMemoryReleaseKey, stagingBuffer, stagingBufferMemory) <- createBuffer
    physicalDevice
    device
    (fromIntegral bufferSize)
    Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
    (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
  dataStaging <- Vk.mapMemory device stagingBufferMemory 0 (fromIntegral bufferSize) zero
  liftIO $ SVector.unsafeWith vertices \ptr ->
    copyBytes (castPtr dataStaging) ptr bufferSize
  Vk.unmapMemory device stagingBufferMemory

  (_vertexBufferReleaseKey, _vertexBufferMemoryReleaseKey, vertexBuffer, vertexBufferMemory) <- createBuffer
    physicalDevice
    device
    (fromIntegral bufferSize)
    (Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer (fromIntegral bufferSize)

  release stagingBufferMemoryReleaseKey
  release stagingBufferReleaseKey

  pure (vertexBuffer, vertexBufferMemory)

copyBuffer :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize -> ResIO ()
copyBuffer device commandPool graphicsQueue srcBuffer dstBuffer size = do
  let
    allocInfo = (zero :: Vk.CommandBufferAllocateInfo)
      { Vk.commandPool
      , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
      , Vk.commandBufferCount = 1
      }
  commandCopyBuffer <- Vector.head <$> Vk.withCommandBuffers device allocInfo allocate'
  Vk.useCommandBuffer commandCopyBuffer zero{Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
    Vk.cmdCopyBuffer commandCopyBuffer srcBuffer dstBuffer (Vector.singleton $ Vk.BufferCopy 0 0 size)

  Vk.queueSubmit
    graphicsQueue
    (Vector.singleton $ SomeStruct zero{Vk.commandBuffers = Vector.singleton $ Vk.commandBufferHandle commandCopyBuffer})
    zero
  Vk.queueWaitIdle graphicsQueue

findMemoryType :: (MonadIO io) => Vk.PhysicalDevice -> Word32 -> Vk.MemoryPropertyFlags -> io Word32
findMemoryType physicalDevice typeFilter properties = do
  memProperties <- Vk.getPhysicalDeviceMemoryProperties physicalDevice
  let
    memTypeMb = iFindIndex
      (\i memType ->
        testBit typeFilter i && (Vk.propertyFlags memType .&. properties) == properties)
      (Vk.memoryTypes memProperties)
  case memTypeMb of
    Nothing -> throwIO $ RuntimeError "Failed to find suitable memory type!"
    Just memType -> pure $ fromIntegral memType

createCommandBuffers :: Vk.Device -> Vk.CommandPool -> ResIO (Vector Vk.CommandBuffer)
createCommandBuffers device commandPool = do
  let
    allocInfo = (zero :: Vk.CommandBufferAllocateInfo)
      { Vk.commandPool
      , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
      , Vk.commandBufferCount = fromIntegral maxFramesInFlight
      }
  Vk.withCommandBuffers device allocInfo allocate'

createSyncObjects
  :: Vk.Device -> Vector Vk.Image -> ResIO (Vector Vk.Semaphore, Vector Vk.Semaphore, Vector Vk.Fence)
createSyncObjects device swapchainImages = do
  -- Signal that an image has been acquired from the swapchain and is ready for rendering
  presentCompleteSemaphores <- Vector.replicateM
    maxFramesInFlight
    (Vk.withSemaphore device zero Nothing allocate')
  -- Signal that rendering has finished and presentation can happen
  renderFinishedSemaphores <- Vector.replicateM
    (Vector.length swapchainImages)
    (Vk.withSemaphore device zero Nothing allocate')
  -- Ensure only one frame is rendered at a time
  inFlightFences <- Vector.replicateM
    maxFramesInFlight
    (Vk.withFence device zero{Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT} Nothing allocate')
  pure (presentCompleteSemaphores, renderFinishedSemaphores, inFlightFences)

initVulkan :: Bool -> Int -> Int -> ResIO Application
initVulkan enableValidationLayers width height = do
  framebufferResizedRef <- newIORef False
  window <- initWindow width height framebufferResizedRef
  inst <- createInstance enableValidationLayers
  _dbgMsgsMb <- setupDebugMessenger enableValidationLayers inst
  surface <- createSurface inst window
  physicalDevice <- liftIO $ pickPhysicalDevice inst
  (device, queue, queueIndex) <- createLogicalDevice physicalDevice surface
  (swapchainReleaseKey, swapchain, swapchainSurfaceFormat, swapchainImages, swapchainExtent) <-
    createSwapchain device physicalDevice surface window
  (swapchainImageViewsReleaseKeys, swapchainImageViews) <-
    createImageViews swapchainSurfaceFormat swapchainImages device
  graphicsPipeline <- createGraphicsPipeline device swapchainExtent swapchainSurfaceFormat
  commandPool <- createCommandPool device queueIndex
  (vertexBuffer, vertexBufferMemory) <-
    createVertexBuffer physicalDevice device commandPool queue
  commandBuffers <- createCommandBuffers device commandPool
  (presentCompleteSemaphores, renderFinishedSemaphores, inFlightFences) <-
    createSyncObjects device swapchainImages
  let
    frames = Vector.zipWith3
      (\presentCompleteSemaphore inFlightFence commandBuffer -> Frame{..})
      presentCompleteSemaphores
      inFlightFences
      commandBuffers
  swapchainRef <- newIORef Swapchain
    { swapchain
    , surfaceFormat = swapchainSurfaceFormat
    , releaseKey = swapchainReleaseKey
    , images = swapchainImages
    , extent = swapchainExtent
    , imageViews = swapchainImageViews
    , imageViewsReleaseKeys = swapchainImageViewsReleaseKeys
    }
  pure Application{..}

recordCommandBuffer :: Word32 -> Frame -> MonadApplication ()
recordCommandBuffer imageIndex frame = do
  Application{swapchainRef, graphicsPipeline, vertexBuffer} <- ask

  Vk.beginCommandBuffer frame.commandBuffer zero

  transitionImageLayout
    imageIndex
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    zero
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    frame

  swapchain <- readIORef swapchainRef
  let
    clearColor = Vk.Color $ Vk.Float32 0 0 0 1
    attachmentInfo = (zero :: Vk.RenderingAttachmentInfo)
      { Vk.imageView = swapchain.imageViews Vector.! fromIntegral imageIndex
      , Vk.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
      , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
      , Vk.clearValue = clearColor
      }
    renderingInfo = (zero :: Vk.RenderingInfo '[])
      { Vk.renderArea = zero{Vk.offset = Vk.Offset2D 0 0, Vk.extent = swapchain.extent}
      , Vk.layerCount = 1
      , Vk.colorAttachments = Vector.singleton attachmentInfo
      }

  Vk.cmdBeginRendering frame.commandBuffer renderingInfo

  Vk.cmdBindPipeline frame.commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
  Vk.cmdSetViewport
    frame.commandBuffer
    0
    (Vector.singleton $
      Vk.Viewport 0 0 (fromIntegral swapchain.extent.width) (fromIntegral swapchain.extent.height) 0 1)
  Vk.cmdSetScissor frame.commandBuffer 0 (Vector.singleton (Vk.Rect2D (Vk.Offset2D 0 0) swapchain.extent))
  Vk.cmdBindVertexBuffers frame.commandBuffer 0 (Vector.singleton vertexBuffer) (Vector.singleton 0)
  Vk.cmdDraw frame.commandBuffer (fromIntegral $ SVector.length vertices) 1 0 0

  Vk.cmdEndRendering frame.commandBuffer

  transitionImageLayout
    imageIndex
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    zero
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT
    frame

  Vk.endCommandBuffer frame.commandBuffer

transitionImageLayout
  :: Word32
  -> Vk.ImageLayout
  -> Vk.ImageLayout
  -> Vk.AccessFlags2
  -> Vk.AccessFlags2
  -> Vk.PipelineStageFlags2
  -> Vk.PipelineStageFlags2
  -> Frame
  -> MonadApplication ()
transitionImageLayout
    imageIndex
    oldLayout
    newLayout
    srcAccessMask
    dstAccessMask
    srcStageMask
    dstStageMask
    frame = do
  Application{swapchainRef} <- ask
  swapchain <- readIORef swapchainRef
  let
    barrier = (zero :: Vk.ImageMemoryBarrier2 '[])
      { Vk.srcStageMask
      , Vk.srcAccessMask
      , Vk.dstStageMask
      , Vk.dstAccessMask
      , Vk.oldLayout
      , Vk.newLayout
      , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      , Vk.image = swapchain.images Vector.! fromIntegral imageIndex
      , Vk.subresourceRange = zero
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = 0
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }
      }
    dependencyInfo = (zero :: Vk.DependencyInfo)
      { Vk.dependencyFlags = zero
      , Vk.imageMemoryBarriers = Vector.singleton $ SomeStruct barrier
      }
  Vk.cmdPipelineBarrier2 frame.commandBuffer dependencyInfo

drawFrame :: Int -> MonadApplication Bool
drawFrame frameIndex = do
  Application{..} <- ask
  let frame = frames Vector.! frameIndex
  let drawFences = Vector.singleton frame.inFlightFence
  Vk.waitForFences device drawFences True maxBound
    >>= checkResult "Failed to wait for fence!"

  swapchain <- readIORef swapchainRef
  (imageIndexResult, imageIndex) <-
    Vk.acquireNextImageKHR device swapchain.swapchain maxBound frame.presentCompleteSemaphore zero
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
  continue :: Vector Vk.Fence -> Word32 -> Frame -> Swapchain -> MonadApplication Bool
  continue drawFences imageIndex frame swapchain = do
    Application{..} <- ask

    -- Only reset the fence if we are submitting work
    Vk.resetFences device drawFences

    recordCommandBuffer imageIndex frame

    let
      waitDestinationStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      renderFinishedSemaphore = Vector.singleton $ renderFinishedSemaphores Vector.! fromIntegral imageIndex
      submitInfo = (zero :: Vk.SubmitInfo '[])
        { Vk.waitSemaphores = Vector.singleton frame.presentCompleteSemaphore
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

catchOutOfDate :: (MonadUnliftIO m) => m Vk.Result -> m Vk.Result
catchOutOfDate action =
  action `catch` \exn@(VulkanException r) ->
    if r == Vk.ERROR_OUT_OF_DATE_KHR
    then pure r
    else throwIO exn

cleanupSwapchain :: Vector ReleaseKey -> ReleaseKey -> ResIO ()
cleanupSwapchain swapchainImageViewsReleaseKeys swapchainReleaseKey = do
  traverse_ release swapchainImageViewsReleaseKeys
  release swapchainReleaseKey

recreateSwapchain :: MonadApplication ()
recreateSwapchain = do
  Application{..} <- ask

  -- Pause while minimized
  liftIO $ whileM_
    (GLFW.getFramebufferSize window <&> \(width, height) -> width == 0 || height == 0)
    GLFW.waitEvents

  Vk.deviceWaitIdle device

  MonadApplication $ lift do
    do
      Swapchain{..} <- readIORef swapchainRef
      cleanupSwapchain imageViewsReleaseKeys releaseKey
    (releaseKey, swapchain, surfaceFormat, images, extent) <-
      createSwapchain device physicalDevice surface window
    (imageViewsReleaseKeys, imageViews) <-
      createImageViews surfaceFormat images device
    writeIORef swapchainRef Swapchain{..}

mainLoop :: MonadApplication ()
mainLoop = do
  Application{window} <- ask
  frameIndexRef <- newIORef 0
  whileM_ (liftIO $ not <$> GLFW.windowShouldClose window) do
    frameIndex <- readIORef frameIndexRef
    liftIO GLFW.pollEvents
    skippedFrame <- drawFrame frameIndex
    unless skippedFrame do
      writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

defaultMain :: IO ()
defaultMain = catch
  (runResourceT do
    application <- initVulkan enableValidationLayers defaultWidth defaultHeight
    flip runReaderT application $ (.runMonadApplication) do
      mainLoop `finally` Vk.deviceWaitIdle application.device)
  \(err :: SomeException) ->
    hPutStrLn stderr $ displayException err
 where
  enableValidationLayers = True
