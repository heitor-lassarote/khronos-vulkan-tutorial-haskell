module Tutorial.Vulkan.HelloTriangle (
  defaultMain,
) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, ResIO, allocate, allocate_, release, runResourceT)
import Data.Bits (shift, zeroBits, (.&.), (.|.))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (find, for_)
import Data.Kind (Type)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ord (clamp)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word32)
import Graphics.UI.GLFW qualified as GLFW
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)
import UnliftIO.Exception (Exception (displayException), SomeException, assert, catch, throwIO)
import UnliftIO.Foreign (Storable (..), alloca, nullPtr, peekCString)
import Vulkan qualified as Vk
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&))
import Vulkan.Zero (zero)

foreign import ccall unsafe "debug_callback.c &debug_callback"
  debugCallbackPtr :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT

type Application :: Type
data Application = Application
  { window :: GLFW.Window
  , width, height :: Int
  , device :: Vk.Device
  , queue :: Vk.Queue
  , swapChain :: Vk.SwapchainKHR
  , swapChainImages :: Vector Vk.Image
  , swapChainExtent :: Vk.Extent2D
  , swapChainImageViews :: Vector Vk.ImageView
  , presentCompleteSemaphore, renderFinishedSemaphore :: Vk.Semaphore
  , drawFence :: Vk.Fence
  , commandBuffer :: Vk.CommandBuffer
  , graphicsPipeline :: Vk.Pipeline
  }

type MonadApplication :: Type -> Type
newtype MonadApplication a = MonadApplication
  { runMonadApplication :: ReaderT Application ResIO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource)

instance MonadReader Application MonadApplication where
  ask = MonadApplication ask
  local f = MonadApplication . local f . (.runMonadApplication)

type RuntimeError :: Type
data RuntimeError = RuntimeError String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

checkResult :: (MonadIO io) => String -> Vk.Result -> io ()
checkResult message result =
  case result of
    Vk.SUCCESS -> pure ()
    _ -> throwIO $ RuntimeError $ "[" <> show result <> "] " <> message

withResultCheck :: (MonadIO io) => String -> IO (Vk.Result, a) -> io a
withResultCheck msg action = do
  (result, ret) <- liftIO action
  checkResult msg result
  pure ret

defaultWidth, defaultHeight :: Int
defaultWidth = 800
defaultHeight = 600

initWindow :: Int -> Int -> ResIO GLFW.Window
initWindow width height = do
  _glfwKey <- allocate_ GLFW.init GLFW.terminate

  liftIO $ GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  liftIO $ GLFW.windowHint $ GLFW.WindowHint'Resizable False
  (_winKey, win) <-
    allocate
      ( GLFW.createWindow width height "Vulkan" Nothing Nothing >>= \case
          Nothing -> throwIO $ RuntimeError "Could not create window"
          Just window -> pure window
      )
      GLFW.destroyWindow

  pure win

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
    case find (\requiredLayer -> not $ any (== requiredLayer) layerNames) requiredLayers of
      Nothing -> pure ()
      Just layerName -> throwIO $ RuntimeError $ "Unsupported required layer: " <> BS.unpack layerName

  glfwExtensions <- liftIO $ getRequiredInstanceExtensions enableValidationLayers

  let
    appInfo =
      zero
        { Vk.applicationInfo =
            Just
              Vk.ApplicationInfo
                { Vk.applicationName = Just "Hello Triangle"
                , Vk.applicationVersion = makeVersion 1 0 0
                , Vk.engineName = Just "No Engine"
                , Vk.engineVersion = makeVersion 1 0 0
                , Vk.apiVersion = Vk.API_VERSION_1_3
                }
        , Vk.enabledExtensionNames = glfwExtensions
        , Vk.enabledLayerNames = requiredLayers
        }

  (_instKey, inst) <-
    allocate
      (Vk.createInstance appInfo Nothing)
      (\inst -> Vk.destroyInstance inst Nothing)
  pure inst
 where
  requiredLayers =
    if enableValidationLayers
      then Vector.singleton "VK_LAYER_KHRONOS_validation"
      else Vector.empty

setupDebugMessenger :: Bool -> Vk.Instance -> ResIO (Maybe Vk.DebugUtilsMessengerEXT)
setupDebugMessenger enableValidationLayers inst =
  if enableValidationLayers
    then do
      let
        createInfo =
          zero
            { Vk.messageSeverity = severityFlags
            , Vk.messageType = messageTypeFlags
            , Vk.pfnUserCallback = debugCallbackPtr
            }
      (_debugUtilsMessengerKey, debugUtilsMessenger) <-
        allocate
          (Vk.createDebugUtilsMessengerEXT inst createInfo Nothing)
          (flip (Vk.destroyDebugUtilsMessengerEXT inst) Nothing)
      pure $ Just debugUtilsMessenger
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
    throwIO $ RuntimeError $ "Failed to find GPUs with Vulkan support"
  devices <- flip Vector.filterM physicalDevices \physicalDevice -> do
    properties <- Vk.getPhysicalDeviceProperties physicalDevice
    features <-
      Vk.getPhysicalDeviceFeatures2
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
      supportsGraphics =
        isJust $ find (\qfp -> qfp.queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits) queueFamilies
      requiredDeviceExtensions = Vector.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME
      supportsAllRequiredExtensions =
        all
          ( \requiredDeviceExtension ->
              isJust $
                find
                  (\availableDeviceExtension -> availableDeviceExtension.extensionName == requiredDeviceExtension)
                  availableDeviceExtensions
          )
          requiredDeviceExtensions
      (physicalDeviceVulkan11Features, (physicalDeviceVulkan13Features, (physicalDeviceExtendedDynamicStateFeatures, ()))) = features.next
      supportsRequiredFeatures =
        physicalDeviceVulkan11Features.shaderDrawParameters
          && physicalDeviceVulkan13Features.dynamicRendering
          && physicalDeviceVulkan13Features.synchronization2
          && physicalDeviceExtendedDynamicStateFeatures.extendedDynamicState

    pure $ supportsVulkan1_3 && supportsGraphics && supportsAllRequiredExtensions && supportsRequiredFeatures

  when (Vector.null devices) do
    throwIO $ RuntimeError "failed to find a suitable GPU!"
  Vector.headM devices

iFindIndexM :: (Monad m) => (Int -> a -> m Bool) -> Vector a -> m (Maybe Int)
iFindIndexM predicate v = go 0
 where
  len = Vector.length v
  go i
    | i == len = pure Nothing
    | otherwise =
        predicate i (Vector.unsafeIndex v i) >>= \case
          False -> go $ i + 1
          True -> pure $ Just i

createLogicalDevice :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> ResIO (Vk.Device, Vk.Queue, Word32)
createLogicalDevice physicalDevice surface = do
  queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice

  -- Find queue family property that supports both graphics and present
  queueIndexMb <-
    iFindIndexM
      ( \i qfp -> do
          hasSurfaceSupport <- Vk.getPhysicalDeviceSurfaceSupportKHR physicalDevice (fromIntegral i) surface
          pure $ (qfp.queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits) && hasSurfaceSupport
      )
      queueFamilyProperties
  queueIndex <-
    maybe
      (throwIO $ RuntimeError "Could not find a queue for graphics and present -> terminating")
      (pure . fromIntegral)
      queueIndexMb

  let
    graphicsIndex =
      fromJust $
        Vector.findIndex (\qfp -> qfp.queueFlags .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits) queueFamilyProperties
    queuePriority = 0.5
    deviceQueueCreateInfo =
      (zero :: Vk.DeviceQueueCreateInfo '[])
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
    deviceCreateInfo =
      (zero :: Vk.DeviceCreateInfo '[])
        { Vk.next = featureChain
        , Vk.queueCreateInfos = Vector.singleton $ SomeStruct deviceQueueCreateInfo
        , Vk.enabledExtensionNames = requiredDeviceExtensions
        }
  (_deviceReleaseKey, device) <-
    allocate
      (Vk.createDevice physicalDevice deviceCreateInfo Nothing)
      (flip Vk.destroyDevice Nothing)
  graphicsQueue <- Vk.getDeviceQueue device (fromIntegral graphicsIndex) queueIndex
  pure (device, graphicsQueue, queueIndex)

createSurface :: Vk.Instance -> GLFW.Window -> ResIO Vk.SurfaceKHR
createSurface inst window = do
  snd
    <$> allocate
      ( alloca \surfPtr ->
          GLFW.createWindowSurface @Int (Vk.instanceHandle inst) window nullPtr surfPtr >>= \case
            0 -> peek surfPtr
            r -> throwIO $ RuntimeError $ "Failed to create window surface: " <> show r
      )
      (flip (Vk.destroySurfaceKHR inst) Nothing)

chooseSwapSurfaceFormat :: Vector Vk.SurfaceFormatKHR -> Vk.SurfaceFormatKHR
chooseSwapSurfaceFormat availableFormats =
  assert
    (not $ Vector.null availableFormats)
    (fromMaybe (Vector.unsafeHead availableFormats) srgb)
 where
  srgb =
    find
      (\format -> format.format == Vk.FORMAT_B8G8R8A8_SRGB && format.colorSpace == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR)
      availableFormats

chooseSwapPresentMode :: Vector Vk.PresentModeKHR -> Vk.PresentModeKHR
chooseSwapPresentMode availablePresentModes =
  assert
    (fifo `elem` availablePresentModes)
    (if mailbox `elem` availablePresentModes then mailbox else fifo)
 where
  mailbox = Vk.PRESENT_MODE_MAILBOX_KHR
  fifo = Vk.PRESENT_MODE_FIFO_KHR

chooseSwapExtent :: (MonadIO io) => Vk.SurfaceCapabilitiesKHR -> GLFW.Window -> io Vk.Extent2D
chooseSwapExtent capabilities window
  | capabilities.currentExtent.width /= maxBound = pure capabilities.currentExtent
  | otherwise = do
      (width, height) <- liftIO $ GLFW.getFramebufferSize window
      pure
        Vk.Extent2D
          { width = clamp (capabilities.minImageExtent.width, capabilities.maxImageExtent.width) $ fromIntegral width
          , height = clamp (capabilities.minImageExtent.height, capabilities.maxImageExtent.height) $ fromIntegral height
          }

chooseSwapMinImageCount :: Vk.SurfaceCapabilitiesKHR -> Word32
chooseSwapMinImageCount capabilities
  -- 0 means that there is no maximum
  | 0 < capabilities.maxImageCount && capabilities.maxImageCount < minImageCount = capabilities.maxImageCount
  | otherwise = minImageCount
 where
  minImageCount = max 3 capabilities.minImageCount

createSwapChain ::
  Vk.Device ->
  Vk.PhysicalDevice ->
  Vk.SurfaceKHR ->
  GLFW.Window ->
  ResIO (Vk.SwapchainKHR, Vk.SurfaceFormatKHR, Vector Vk.Image, Vk.Extent2D)
createSwapChain device physicalDevice surface window = do
  surfaceCapabilities <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  swapChainExtent <- chooseSwapExtent surfaceCapabilities window
  let minImageCount = chooseSwapMinImageCount surfaceCapabilities
  availableFormats <-
    withResultCheck "Failed to get physical device surface formats" $
      Vk.getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
  availablePresentModes <-
    withResultCheck "Failed to get physical device present modes" $
      Vk.getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface
  let
    presentMode = chooseSwapPresentMode availablePresentModes
    swapChainSurfaceFormat = chooseSwapSurfaceFormat availableFormats
    swapChainCreateInfo =
      (zero :: Vk.SwapchainCreateInfoKHR '[])
        { Vk.surface
        , Vk.minImageCount
        , Vk.imageFormat = swapChainSurfaceFormat.format
        , Vk.imageColorSpace = swapChainSurfaceFormat.colorSpace
        , Vk.imageExtent = swapChainExtent
        , Vk.imageArrayLayers = 1
        , Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        , Vk.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.preTransform = surfaceCapabilities.currentTransform
        , Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , Vk.presentMode
        , Vk.clipped = True
        }
  (_swapChainReleaseKey, swapChain) <-
    allocate
      (Vk.createSwapchainKHR device swapChainCreateInfo Nothing)
      (flip (Vk.destroySwapchainKHR device) Nothing)
  swapChainImages <-
    withResultCheck "Failed to get swapchain images" $
      Vk.getSwapchainImagesKHR device swapChain
  pure (swapChain, swapChainSurfaceFormat, swapChainImages, swapChainExtent)

createImageViews ::
  Vk.SurfaceFormatKHR ->
  Vector Vk.Image ->
  Vk.Device ->
  ResIO (Vector Vk.ImageView)
createImageViews swapChainSurfaceFormat swapChainImages device = do
  let
    imageViewCreateInfo =
      (zero :: Vk.ImageViewCreateInfo '[])
        { Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = swapChainSurfaceFormat.format
        , Vk.subresourceRange =
            Vk.ImageSubresourceRange
              { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
              , Vk.baseMipLevel = 0
              , Vk.levelCount = 1
              , Vk.baseArrayLayer = 0
              , Vk.layerCount = 1
              }
        }
  traverse
    ( \image ->
        snd
          <$> allocate
            (Vk.createImageView device imageViewCreateInfo{Vk.image} Nothing)
            (flip (Vk.destroyImageView device) Nothing)
    )
    swapChainImages

createShaderModule :: Vk.Device -> ByteString -> ResIO (ReleaseKey, Vk.ShaderModule)
createShaderModule device code = do
  let createInfo = (zero :: Vk.ShaderModuleCreateInfo '[]){Vk.code}
  allocate
    (Vk.createShaderModule device createInfo Nothing)
    (flip (Vk.destroyShaderModule device) Nothing)

createGraphicsPipeline ::
  Vk.Device -> Vk.Extent2D -> Vk.SurfaceFormatKHR -> ResIO Vk.Pipeline
createGraphicsPipeline device _swapChainExtent swapChainSurfaceFormat = do
  shaderCode <- liftIO $ BS.readFile ("shaders" </> "triangle" <.> "spv")
  (shaderModuleKey, shaderModule) <- createShaderModule device shaderCode
  let
    vertShaderStageInfo =
      (zero :: Vk.PipelineShaderStageCreateInfo '[])
        { Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.module' = shaderModule
        , Vk.name = "vertMain"
        }
    fragShaderStageInfo =
      (zero :: Vk.PipelineShaderStageCreateInfo '[])
        { Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.module' = shaderModule
        , Vk.name = "fragMain"
        }
    shaderStages = Vector.fromList [vertShaderStageInfo, fragShaderStageInfo]

    dynamicStates = Vector.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR]
    dynamicState = (zero :: Vk.PipelineDynamicStateCreateInfo){Vk.dynamicStates}

    vertexInputInfo = zero :: Vk.PipelineVertexInputStateCreateInfo '[]
    inputAssemblyState =
      (zero :: Vk.PipelineInputAssemblyStateCreateInfo)
        { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        }

    viewportState =
      (zero :: Vk.PipelineViewportStateCreateInfo '[])
        { Vk.viewportCount = 1
        , Vk.scissorCount = 1
        }

    rasterizer =
      (zero :: Vk.PipelineRasterizationStateCreateInfo '[])
        { Vk.depthClampEnable = False
        , Vk.rasterizerDiscardEnable = False
        , Vk.polygonMode = Vk.POLYGON_MODE_FILL
        , Vk.cullMode = Vk.CULL_MODE_BACK_BIT
        , Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
        , Vk.depthBiasEnable = False
        , Vk.lineWidth = 1
        }
    multisampling =
      (zero :: Vk.PipelineMultisampleStateCreateInfo '[])
        { Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.sampleShadingEnable = False
        }
    colorBlendAttachment =
      (zero :: Vk.PipelineColorBlendAttachmentState)
        { Vk.blendEnable = False
        , Vk.colorWriteMask =
            Vk.COLOR_COMPONENT_R_BIT
              .|. Vk.COLOR_COMPONENT_G_BIT
              .|. Vk.COLOR_COMPONENT_B_BIT
              .|. Vk.COLOR_COMPONENT_A_BIT
        }
    colorBlending =
      (zero :: Vk.PipelineColorBlendStateCreateInfo '[])
        { Vk.logicOpEnable = False
        , Vk.logicOp = Vk.LOGIC_OP_COPY
        , Vk.attachmentCount = 1
        , Vk.attachments = Vector.singleton colorBlendAttachment
        }

    pipelineLayoutInfo = zero :: Vk.PipelineLayoutCreateInfo
  (_pipelineLayoutReleaseKey, pipelineLayout) <-
    allocate
      (Vk.createPipelineLayout device pipelineLayoutInfo Nothing)
      (flip (Vk.destroyPipelineLayout device) Nothing)

  let
    pipelineCreateInfoChain =
      (zero :: Vk.GraphicsPipelineCreateInfo '[])
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
        , Vk.next =
            (zero :: Vk.PipelineRenderingCreateInfo)
              { Vk.colorAttachmentFormats = Vector.singleton swapChainSurfaceFormat.format
              }
              :& ()
        }

  (_graphicsPipelineReleaseKey, graphicsPipeline) <-
    allocate
      do
        pipelines <-
          withResultCheck "Failed to create graphics pipeline" $
            Vk.createGraphicsPipelines device zero (Vector.singleton $ SomeStruct pipelineCreateInfoChain) Nothing
        pure $ assert (Vector.length pipelines == 1) (Vector.head pipelines)
      (flip (Vk.destroyPipeline device) Nothing)

  release shaderModuleKey

  pure graphicsPipeline

createCommandPool :: Vk.Device -> Word32 -> ResIO Vk.CommandPool
createCommandPool device queueIndex = do
  let
    poolInfo =
      (zero :: Vk.CommandPoolCreateInfo)
        { Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
        , Vk.queueFamilyIndex = queueIndex
        }
  snd
    <$> allocate
      (Vk.createCommandPool device poolInfo Nothing)
      (flip (Vk.destroyCommandPool device) Nothing)

createSyncObjects :: Vk.Device -> ResIO (Vk.Semaphore, Vk.Semaphore, Vk.Fence)
createSyncObjects device = do
  -- Signal that an image has been acquired from the swapchain and is ready for rendering
  (_presentCompleteSemaphoreReleaseKey, presentCompleteSemaphore) <-
    allocate
      (Vk.createSemaphore device zero Nothing)
      (flip (Vk.destroySemaphore device) Nothing)
  -- Signal that rendering has finished and presentation can happen
  (_renderFinishedSemaphoreReleaseKey, renderFinishedSemaphore) <-
    allocate
      (Vk.createSemaphore device zero Nothing)
      (flip (Vk.destroySemaphore device) Nothing)
  -- Ensure only one frame is rendered at a time
  (_drawFenceReleaseKey, drawFence) <-
    allocate
      (Vk.createFence device zero{Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT} Nothing)
      (flip (Vk.destroyFence device) Nothing)
  pure (presentCompleteSemaphore, renderFinishedSemaphore, drawFence)

initVulkan :: Bool -> Int -> Int -> ResIO Application
initVulkan enableValidationLayers width height = do
  window <- initWindow width height
  inst <- createInstance enableValidationLayers
  _dbgMsgsMb <- setupDebugMessenger enableValidationLayers inst
  surface <- createSurface inst window
  physicalDevice <- liftIO $ pickPhysicalDevice inst
  (device, queue, queueIndex) <- createLogicalDevice physicalDevice surface
  (swapChain, swapChainSurfaceFormat, swapChainImages, swapChainExtent) <-
    createSwapChain device physicalDevice surface window
  swapChainImageViews <- createImageViews swapChainSurfaceFormat swapChainImages device
  graphicsPipeline <- createGraphicsPipeline device swapChainExtent swapChainSurfaceFormat
  commandPool <- createCommandPool device queueIndex
  commandBuffer <- createCommandBuffer device commandPool
  (presentCompleteSemaphore, renderFinishedSemaphore, drawFence) <- createSyncObjects device
  pure Application{..}

createCommandBuffer :: Vk.Device -> Vk.CommandPool -> ResIO Vk.CommandBuffer
createCommandBuffer device commandPool = do
  let
    allocInfo =
      (zero :: Vk.CommandBufferAllocateInfo)
        { Vk.commandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
  Vector.head . snd
    <$> allocate
      (Vk.allocateCommandBuffers device allocInfo)
      (Vk.freeCommandBuffers device commandPool)

recordCommandBuffer :: Word32 -> MonadApplication ()
recordCommandBuffer imageIndex = do
  Application
    { commandBuffer
    , swapChainImageViews
    , swapChainExtent
    , graphicsPipeline
    } <-
    ask
  Vk.beginCommandBuffer commandBuffer zero

  transitionImageLayout
    imageIndex
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    zero
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
  let
    clearColor = Vk.Color $ Vk.Float32 0 0 0 1
    attachmentInfo =
      (zero :: Vk.RenderingAttachmentInfo)
        { Vk.imageView = swapChainImageViews Vector.! fromIntegral imageIndex
        , Vk.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.clearValue = clearColor
        }
    renderingInfo =
      (zero :: Vk.RenderingInfo '[])
        { Vk.renderArea = zero{Vk.offset = Vk.Offset2D 0 0, Vk.extent = swapChainExtent}
        , Vk.layerCount = 1
        , Vk.colorAttachments = Vector.singleton attachmentInfo
        }

  Vk.cmdBeginRendering commandBuffer renderingInfo

  Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
  Vk.cmdSetViewport
    commandBuffer
    0
    ( Vector.singleton $
        Vk.Viewport 0 0 (fromIntegral swapChainExtent.width) (fromIntegral swapChainExtent.height) 0 1
    )
  Vk.cmdSetScissor commandBuffer 0 (Vector.singleton (Vk.Rect2D (Vk.Offset2D 0 0) swapChainExtent))
  Vk.cmdDraw commandBuffer 3 1 0 0

  Vk.cmdEndRendering commandBuffer

  transitionImageLayout
    imageIndex
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
    Vk.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
    zero
    Vk.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT

  Vk.endCommandBuffer commandBuffer

transitionImageLayout ::
  Word32 ->
  Vk.ImageLayout ->
  Vk.ImageLayout ->
  Vk.AccessFlags2 ->
  Vk.AccessFlags2 ->
  Vk.PipelineStageFlags2 ->
  Vk.PipelineStageFlags2 ->
  MonadApplication ()
transitionImageLayout
  imageIndex
  oldLayout
  newLayout
  srcAccessMask
  dstAccessMask
  srcStageMask
  dstStageMask = do
    Application{commandBuffer, swapChainImages} <- ask
    let
      barrier =
        (zero :: Vk.ImageMemoryBarrier2 '[])
          { Vk.srcStageMask
          , Vk.srcAccessMask
          , Vk.dstStageMask
          , Vk.dstAccessMask
          , Vk.oldLayout
          , Vk.newLayout
          , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
          , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
          , Vk.image = swapChainImages Vector.! fromIntegral imageIndex
          , Vk.subresourceRange =
              zero
                { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                , Vk.baseMipLevel = 0
                , Vk.levelCount = 1
                , Vk.baseArrayLayer = 0
                , Vk.layerCount = 1
                }
          }
      dependencyInfo =
        (zero :: Vk.DependencyInfo)
          { Vk.dependencyFlags = zero
          , Vk.imageMemoryBarriers = Vector.singleton $ SomeStruct barrier
          }
    Vk.cmdPipelineBarrier2 commandBuffer dependencyInfo

drawFrame :: MonadApplication ()
drawFrame = do
  Application{..} <- ask
  let drawFences = Vector.singleton drawFence
  Vk.waitForFences device drawFences True maxBound
    >>= checkResult "Failed to wait for fence!"
  Vk.resetFences device drawFences

  (_imageIndexResult, imageIndex) <-
    Vk.acquireNextImageKHR device swapChain maxBound presentCompleteSemaphore zero
  recordCommandBuffer imageIndex

  let
    waitDestinationStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    submitInfo =
      (zero :: Vk.SubmitInfo '[])
        { Vk.waitSemaphores = Vector.singleton presentCompleteSemaphore
        , Vk.waitDstStageMask = Vector.singleton waitDestinationStageMask
        , Vk.commandBuffers = Vector.singleton $ Vk.commandBufferHandle commandBuffer
        , Vk.signalSemaphores = Vector.singleton renderFinishedSemaphore
        }
  Vk.queueSubmit queue (Vector.singleton $ SomeStruct submitInfo) drawFence

  let
    presentInfoKHR =
      (zero :: Vk.PresentInfoKHR '[])
        { Vk.waitSemaphores = Vector.singleton renderFinishedSemaphore
        , Vk.swapchains = Vector.singleton swapChain
        , Vk.imageIndices = Vector.singleton imageIndex
        }

  _presentResult <- Vk.queuePresentKHR queue presentInfoKHR

  pure ()

mainLoop :: MonadApplication ()
mainLoop = do
  Application{window, device} <- ask
  whileM_ (liftIO $ fmap not $ GLFW.windowShouldClose window) do
    liftIO GLFW.pollEvents
    drawFrame

  Vk.deviceWaitIdle device

defaultMain :: IO ()
defaultMain = do
  catch
    ( runResourceT do
        application <- initVulkan enableValidationLayers width height
        flip runReaderT application $ (.runMonadApplication) do
          mainLoop
    )
    \(err :: SomeException) ->
      hPutStrLn stderr $ displayException err
 where
  width = defaultWidth
  height = defaultHeight
  enableValidationLayers = True
