module Tutorial.Vulkan.HelloTriangle (defaultMain) where

import Codec.Picture                (Image (..), convertRGBA8, readImage)
import Control.Lens                 (_1, view, (%~), (&))
import Control.Monad                (unless, when)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Loops          (whileM_)
import Control.Monad.Reader         (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Resource
  (MonadResource, ReleaseKey, ResIO, allocate, allocate_, release, runResourceT)
import Data.Bits                    (Bits (..))
import Data.ByteString.Char8        (ByteString)
import Data.ByteString.Char8        qualified as BS
import Data.Foldable                (find, for_, traverse_)
import Data.Functor                 ((<&>))
import Data.Kind                    (Constraint, Type)
import Data.Maybe                   (fromJust, fromMaybe, isJust)
import Data.Ord                     (clamp)
import Data.Time                    (UTCTime)
import Data.Time                    qualified as Time
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
  (Ptr, Storable (..), alloca, copyBytes, nullPtr, peekCString)
import UnliftIO.IORef               (newIORef, readIORef, writeIORef)
import Vulkan                       qualified as Vk
import Vulkan.CStruct.Extends       (SomeStruct (..), pattern (:&))
import Vulkan.Exception             (VulkanException (..))
import Vulkan.Zero                  (zero)

import Tutorial.Vulkan.Utils        (iFindIndex, iFindIndexM)
import Tutorial.Vulkan.Vertex       (Index (..), Vertex (..))
import Tutorial.Vulkan.Vertex       qualified as Vertex

foreign import ccall unsafe "debug_callback.c &debug_callback"
  debugCallbackPtr :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT

type Frame :: Type
data Frame = Frame
  { presentCompleteSemaphore :: Vk.Semaphore
  , inFlightFence            :: Vk.Fence
  , commandBuffer            :: Vk.CommandBuffer
  , uniformBuffer            :: Vk.Buffer
  , uniformBufferMemory      :: Vk.DeviceMemory
  , uniformBufferMapped      :: Ptr UniformBufferObject
  , descriptorSet            :: Vk.DescriptorSet
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

type UniformBufferObject :: Type
data UniformBufferObject = UniformBufferObject
  { model, view, proj :: Linear.M44 Float
  }

instance Storable UniformBufferObject where
  sizeOf _ = 3 * sizeOf (undefined :: Linear.M44 Float)

  alignment _ = alignment (undefined :: Float)

  peek ptr = do
    let p = castPtr ptr
    model <- peek p
    view' <- peekByteOff p (sizeOf (undefined :: Linear.M44 Float))
    proj <- peekByteOff p (2 * sizeOf (undefined :: Linear.M44 Float))
    pure UniformBufferObject{view = view', ..}

  poke ptr UniformBufferObject{view = view', ..} = do
    let p = castPtr ptr
    poke p model
    pokeByteOff p (sizeOf (undefined :: Linear.M44 Float)) view'
    pokeByteOff p (2 * sizeOf (undefined :: Linear.M44 Float)) proj

type ApplicationEnv :: Type
data ApplicationEnv = ApplicationEnv
  { window                   :: GLFW.Window
  , frames                   :: Vector Frame
  , surface                  :: Vk.SurfaceKHR
  , physicalDevice           :: Vk.PhysicalDevice
  , device                   :: Vk.Device
  , queue                    :: Vk.Queue
  , swapchainRef             :: IORef Swapchain
  , renderFinishedSemaphores :: Vector Vk.Semaphore
  , descriptorSetLayout      :: Vk.DescriptorSetLayout
  , pipelineLayout           :: Vk.PipelineLayout
  , graphicsPipeline         :: Vk.Pipeline
  , framebufferResizedRef    :: IORef Bool
  , vertexBuffer             :: Vk.Buffer
  , vertexBufferMemory       :: Vk.DeviceMemory
  , indexBuffer              :: Vk.Buffer
  , indexBufferMemory        :: Vk.DeviceMemory
  , startTime                :: UTCTime
  }

type MonadApplication :: (Type -> Type) -> Constraint
type MonadApplication m =
  ( MonadReader ApplicationEnv m
  , MonadResource m
  , MonadUnliftIO m
  )

type Application :: Type -> Type
newtype Application a = Application
  { runApplication :: ReaderT ApplicationEnv ResIO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadUnliftIO)

instance MonadReader ApplicationEnv Application where
  ask = Application ask
  local f = Application . local f . (.runApplication)

type RuntimeError :: Type
newtype RuntimeError = RuntimeError String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Throws a runtime error if the provided result is anything other than a success.
checkResult :: (MonadIO io) => String -> Vk.Result -> io ()
checkResult _ Vk.SUCCESS = pure ()
checkResult message result =
  throwIO $ RuntimeError $ "[" <> show result <> "] " <> message

-- | Perform an action and possibly throw an exception with 'checkResult'.
withResultCheck :: (MonadIO io) => String -> io (Vk.Result, a) -> io a
withResultCheck msg action = do
  (result, ret) <- action
  checkResult msg result
  pure ret

-- | The default window width.
defaultWidth :: Int
defaultWidth = 800

-- | The default window height.
defaultHeight :: Int
defaultHeight = 600

-- | Maximum allowed frames in flight.
maxFramesInFlight :: Int
maxFramesInFlight = 2

-- | Allocates memory with @resourcet@'s 'allocate', discarding the release key.
allocate' :: (MonadResource m) => IO a -> (a -> IO ()) -> m a
allocate' create destroy = snd <$> allocate create destroy

-- | Initializes the GLFW window with the provided width and height.
-- The bool reference is set to 'True' if the window is resized with GLFW's resize callback.
initWindow :: (MonadResource m) => Int -> Int -> IORef Bool -> m GLFW.Window
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

-- | Wraps the provided major, minor, and patch values with Vulkan's expected version format.
makeVersion :: Word32 -> Word32 -> Word32 -> Word32
makeVersion major minor patch = major `shift` 22 .|. minor `shift` 12 .|. patch

-- | Enumerates the required Vulkan extensions from GLFW and optionally the validation layer.
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

-- | Creates a Vulkan instance, optionally enabling the validation layer.
createInstance :: (MonadResource m) => Bool -> m Vk.Instance
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

-- | Optionally registers a debug messenger. See 'debugCallbackPtr'.
setupDebugMessenger :: (MonadResource m) => Bool -> Vk.Instance -> m (Maybe Vk.DebugUtilsMessengerEXT)
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

-- | Picks the first physical device (GPU) that supports:
--
--     1. Vulkan API 1.3 or greater.
--     2. Graphics queues.
--     3. The swapchain extension.
--     4. Sampler anisotropy.
--     5. Shaders.
--     6. Dynamic rendering.
--     7. @synchronization2@.
--     8. Extended dynamic state.
pickPhysicalDevice :: Vk.Instance -> IO Vk.PhysicalDevice
pickPhysicalDevice inst = do
  physicalDevices <-
    withResultCheck "Error enumerating physical devices" $
      Vk.enumeratePhysicalDevices inst
  when (Vector.null physicalDevices) do
    throwIO $ RuntimeError "Failed to find GPUs with Vulkan support"
  devices <- flip Vector.filterM physicalDevices \physicalDevice -> do
    properties <- Vk.getPhysicalDeviceProperties physicalDevice
    supportedFeatures <- Vk.getPhysicalDeviceFeatures physicalDevice
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
        supportedFeatures.samplerAnisotropy
        && physicalDeviceVulkan11Features.shaderDrawParameters
        && physicalDeviceVulkan13Features.dynamicRendering
        && physicalDeviceVulkan13Features.synchronization2
        && physicalDeviceExtendedDynamicStateFeatures.extendedDynamicState

    pure $ supportsVulkan1_3 && supportsGraphics && supportsAllRequiredExtensions && supportsRequiredFeatures

  when (Vector.null devices) do
    throwIO $ RuntimeError "failed to find a suitable GPU!"
  Vector.headM devices

-- | Creates a logical device that supports all capabilities from 'pickPhysicalDevice'.
--
-- This function also retrieves the graphics+present queue and its index.
createLogicalDevice :: (MonadResource m) => Vk.PhysicalDevice -> Vk.SurfaceKHR -> m (Vk.Device, Vk.Queue, Word32)
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
        { Vk.features = (zero :: Vk.PhysicalDeviceFeatures)
          { Vk.samplerAnisotropy = True
          }
        }
      :& (zero :: Vk.PhysicalDeviceVulkan11Features)
        { Vk.shaderDrawParameters = True
        }
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

-- | Uses GLFW to create a Vulkan surface.
createSurface :: (MonadResource m) => Vk.Instance -> GLFW.Window -> m Vk.SurfaceKHR
createSurface inst window = do
  allocate'
    (alloca \surfPtr ->
      GLFW.createWindowSurface @Int (Vk.instanceHandle inst) window nullPtr surfPtr >>= \case
        0 -> peek surfPtr
        r -> throwIO $ RuntimeError $ "Failed to create window surface: " <> show r)
    (flip (Vk.destroySurfaceKHR inst) Nothing)

-- | Picks the first format supporting B8G8R8A8 non-linear SRGB.
chooseSwapSurfaceFormat :: Vector Vk.SurfaceFormatKHR -> Vk.SurfaceFormatKHR
chooseSwapSurfaceFormat availableFormats = assert
  (not $ Vector.null availableFormats)
  (fromMaybe (Vector.unsafeHead availableFormats) srgb)
 where
  srgb = find
    (\format -> format.format == Vk.FORMAT_B8G8R8A8_SRGB && format.colorSpace == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR)
    availableFormats

-- | Prefers mailbox (triple-buffering), fallbacks to FIFO (vsync) otherwise.
chooseSwapPresentMode :: Vector Vk.PresentModeKHR -> Vk.PresentModeKHR
chooseSwapPresentMode availablePresentModes = assert
  (fifo `elem` availablePresentModes)
  (if mailbox `elem` availablePresentModes then mailbox else fifo)
 where
  mailbox = Vk.PRESENT_MODE_MAILBOX_KHR
  fifo = Vk.PRESENT_MODE_FIFO_KHR

-- | Uses the swap's current extend if present.
-- Otherwise, clamps the framebuffer's extent (queried via GLFW) between the surface's minimum and maximum.
chooseSwapExtent :: (MonadIO io) => Vk.SurfaceCapabilitiesKHR -> GLFW.Window -> io Vk.Extent2D
chooseSwapExtent capabilities window
  | capabilities.currentExtent.width /= maxBound = pure capabilities.currentExtent
  | otherwise = do
    (fromIntegral -> width, fromIntegral -> height) <- liftIO $ GLFW.getFramebufferSize window
    pure Vk.Extent2D
      { width = clamp (capabilities.minImageExtent.width, capabilities.maxImageExtent.width) width
      , height = clamp (capabilities.minImageExtent.height, capabilities.maxImageExtent.height) height
      }

-- | Requests at least 3 images, capped at the surface's maximum, if there is one.
chooseSwapMinImageCount :: Vk.SurfaceCapabilitiesKHR -> Word32
chooseSwapMinImageCount capabilities
  -- 0 means that there is no maximum
  | 0 < capabilities.maxImageCount && capabilities.maxImageCount < minImageCount = capabilities.maxImageCount
  | otherwise = minImageCount
 where
  minImageCount = max 3 capabilities.minImageCount

-- | Creates the swapchain alongside its release key (managed by 'recreateSwapchain').
--
-- Also returns the swapchain images, format, and extent.
createSwapchain
  :: (MonadResource m)
  => Vk.Device
  -> Vk.PhysicalDevice
  -> Vk.SurfaceKHR
  -> GLFW.Window
  -> m (ReleaseKey, Vk.SwapchainKHR, Vk.SurfaceFormatKHR, Vector Vk.Image, Vk.Extent2D)
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

-- | Creates the views into the images obtained by 'createSwapchain' alongside their release keys.
--
-- See 'createImageView'.
createImageViews
  :: (MonadResource m)
  => Vk.Device
  -> Vk.SurfaceFormatKHR
  -> Vector Vk.Image
  -> m (Vector ReleaseKey, Vector Vk.ImageView)
createImageViews device swapchainSurfaceFormat swapchainImages = do
  Vector.unzip <$> traverse
    (\image -> createImageView device image swapchainSurfaceFormat.format)
    swapchainImages

-- | Creates a descriptor set layout with two layouts:
--
--     1. UBO binding at 0, visible to the vertex stage.
--     2. Combined image sampler binding at 1, visible to the fragment stage.
createDescriptorSetLayout :: (MonadResource m) => Vk.Device -> m Vk.DescriptorSetLayout
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
  Vk.withDescriptorSetLayout device layoutInfo Nothing allocate'

-- | Loads a pre-compiled SPIR-V bytecode into a shader module.
createShaderModule :: (MonadResource m) => Vk.Device -> ByteString -> m (ReleaseKey, Vk.ShaderModule)
createShaderModule device code = do
  let createInfo = (zero :: Vk.ShaderModuleCreateInfo '[]){Vk.code}
  allocate
    (Vk.createShaderModule device createInfo Nothing)
    (flip (Vk.destroyShaderModule device) Nothing)

-- | Describes the vertices of a square ranging from (-0.5, -0.5) to (0.5, 0.5).
--
-- The colors are (top-left, clockwise): red, green, blue, white.
--
-- The texture coordinates (UV) simply go from (0, 0) in the top-left corner to (1, 1) in the bottom-right corner.
--
-- The vertices are linked by 'indices'.
vertices :: SVector.Vector Vertex
vertices = SVector.fromList
  [ Vertex (Linear.V2 -0.5 -0.5) (Linear.V3 1 0 0) (Linear.V2 1 0)
  , Vertex (Linear.V2  0.5 -0.5) (Linear.V3 0 1 0) (Linear.V2 0 0)
  , Vertex (Linear.V2  0.5  0.5) (Linear.V3 0 0 1) (Linear.V2 0 1)
  , Vertex (Linear.V2 -0.5  0.5) (Linear.V3 1 1 1) (Linear.V2 1 1)
  ]

-- | Describes two triangles from 'vertices'.
indices :: SVector.Vector Index
indices = SVector.fromList
  [ 0, 1, 2
  , 2, 3, 0
  ]

-- | Creates the full graphics pipeline, rendering the pipeline layout and pipeline:
--
--     * Loads the shader.
--     * Configures the vertices from 'Vertex'.
--     * Configures the rasterizer:
--         * Back-face culling.
--         * Counter-clockwise winding.
--         * Disabled blending.
--         * Dynamic viewport and scissor.
--     * Sets dynamic rendering.
createGraphicsPipeline
  :: (MonadResource m) => Vk.Device -> Vk.DescriptorSetLayout -> Vk.SurfaceFormatKHR
  -> m (Vk.PipelineLayout, Vk.Pipeline)
createGraphicsPipeline device descriptorSetLayout swapchainSurfaceFormat = do
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

    pipelineLayoutInfo = (zero :: Vk.PipelineLayoutCreateInfo)
      { Vk.setLayouts = Vector.singleton descriptorSetLayout
      , Vk.pushConstantRanges = Vector.empty
      }
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

  pure (pipelineLayout, graphicsPipeline)

-- | Creates a command pool with reset-command-buffer flag for the provided queue family index.
createCommandPool :: (MonadResource m) => Vk.Device -> Word32 -> m Vk.CommandPool
createCommandPool device queueIndex = do
  let
    poolInfo = (zero :: Vk.CommandPoolCreateInfo)
      { Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
      , Vk.queueFamilyIndex = queueIndex
      }
  Vk.withCommandPool device poolInfo Nothing allocate'

-- | Loads a texture from the disk (./khronos-vulkan-tutorial-cpp/images/texture.jpg) in RGBA8 format.
--
-- The image is optimized for transfer destination and sampled,
-- created with 'createImage', using optimal tiling on the local device.
--
-- Returns the allocated image and image memory.
createTextureImage
  :: (MonadResource m) => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> m Vk.Image
createTextureImage physicalDevice device commandPool graphicsQueue =
  liftIO (readImage ("khronos-vulkan-tutorial-cpp" </> "images" </> "texture" <.> "jpg")) >>= \case
    Left err -> throwIO $ RuntimeError $ "Failed to load texture image: " <> err
    Right (convertRGBA8 -> pixels) -> do
      let imageSize = fromIntegral $ pixels.imageWidth * pixels.imageHeight * 4
      (stagingBufferReleaseKey, stagingBufferMemoryReleaseKey, stagingBuffer, stagingBufferMemory) <- createBuffer
        physicalDevice
        device
        imageSize
        Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
        (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
      data' <- Vk.mapMemory device stagingBufferMemory 0 imageSize zero
      liftIO $ SVector.unsafeWith pixels.imageData \ptr ->
        copyBytes (castPtr data') ptr (fromIntegral imageSize)
      Vk.unmapMemory device stagingBufferMemory

      (textureImage, _imageMemory) <- createImage
        physicalDevice
        device
        (fromIntegral pixels.imageWidth)
        (fromIntegral pixels.imageHeight)
        Vk.FORMAT_R8G8B8A8_SRGB
        Vk.IMAGE_TILING_OPTIMAL
        (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT)
        Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

      transitionImageLayout'
        device
        commandPool
        graphicsQueue
        textureImage
        Vk.IMAGE_LAYOUT_UNDEFINED
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      copyBufferToImage
        device
        commandPool
        graphicsQueue
        stagingBuffer
        textureImage
        (fromIntegral pixels.imageWidth)
        (fromIntegral pixels.imageHeight)
      transitionImageLayout'
        device
        commandPool
        graphicsQueue
        textureImage
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

      release stagingBufferMemoryReleaseKey
      release stagingBufferReleaseKey

      pure textureImage

-- | Creates a 2D image with undefined layout and no multisampling.
--
-- Returns the allocated image and image memory.
createImage
  :: (MonadResource m) => Vk.PhysicalDevice -> Vk.Device
  -> Word32 -> Word32
  -> Vk.Format -> Vk.ImageTiling -> Vk.ImageUsageFlags -> Vk.MemoryPropertyFlags
  -> m (Vk.Image, Vk.DeviceMemory)
createImage physicalDevice device width height format tiling usage properties = do
  let
    imageInfo = (zero :: Vk.ImageCreateInfo '[])
      { Vk.imageType = Vk.IMAGE_TYPE_2D
      , Vk.format
      , Vk.tiling
      , Vk.extent = Vk.Extent3D width height 1
      , Vk.mipLevels = 1
      , Vk.arrayLayers = 1
      , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
      , Vk.usage
      , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
      , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
      }
  image <- Vk.withImage device imageInfo Nothing allocate'

  memRequirements <- Vk.getImageMemoryRequirements device image
  memoryTypeIndex <- findMemoryType physicalDevice memRequirements.memoryTypeBits properties
  let
    allocInfo = (zero :: Vk.MemoryAllocateInfo '[])
      { Vk.allocationSize = memRequirements.size
      , Vk.memoryTypeIndex
      }
  imageMemory <- Vk.withMemory device allocInfo Nothing allocate'
  Vk.bindImageMemory device image imageMemory 0

  pure (image, imageMemory)

-- | Allocates an one-time command buffer, performs an action, submits it,
-- waits for the queue to idle, and releases the command buffer.
withSingleTimeCommands
  :: (MonadResource io) => Vk.Device -> Vk.CommandPool -> Vk.Queue -> (Vk.CommandBuffer -> io r)
  -> io r
withSingleTimeCommands device commandPool graphicsQueue action = do
  let
    allocInfo = (zero :: Vk.CommandBufferAllocateInfo)
      { Vk.commandPool
      , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
      , Vk.commandBufferCount = 1
      }
  (commandBufferReleaseKey, commandBuffers) <- allocate
    (Vk.allocateCommandBuffers device allocInfo)
    (Vk.freeCommandBuffers device commandPool)
  let commandBuffer = Vector.head commandBuffers

  ret <- Vk.useCommandBuffer commandBuffer zero{Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} $
    action commandBuffer

  let
    submitInfo = (zero :: Vk.SubmitInfo '[])
      { Vk.commandBuffers = Vector.singleton $ Vk.commandBufferHandle commandBuffer
      }
  Vk.queueSubmit
    graphicsQueue
    (Vector.singleton $ SomeStruct submitInfo)
    zero
  Vk.queueWaitIdle graphicsQueue

  release commandBufferReleaseKey

  pure ret

-- | Helper function to transition a texture image between layouts using a pipeline barrier.
--
-- Currently, only the following transitions are supported:
--
--     1. From undefined to transfer-dst (preparing to receive a copy).
--     2. From transfer-dst to shader-read-only (making it available to the fragment shader after a copy).
transitionImageLayout'
  :: (MonadResource m) => Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> Vk.Image -> Vk.ImageLayout -> Vk.ImageLayout -> m ()
transitionImageLayout' device commandPool graphicsQueue image oldLayout newLayout =
  withSingleTimeCommands device commandPool graphicsQueue \commandBuffer -> do
    (srcAccessMask, dstAccessMask, sourceStage, destinationStage) <-
      case (oldLayout, newLayout) of
        (Vk.IMAGE_LAYOUT_UNDEFINED, Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) ->
          pure
            ( zeroBits
            , Vk.ACCESS_TRANSFER_WRITE_BIT
            , Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
            , Vk.PIPELINE_STAGE_TRANSFER_BIT
            )
        (Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
          pure
            ( Vk.ACCESS_TRANSFER_WRITE_BIT
            , Vk.ACCESS_SHADER_READ_BIT
            , Vk.PIPELINE_STAGE_TRANSFER_BIT
            , Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
            )
        (_, _) -> throwIO $ RuntimeError "Unsupported layout transition!"
    let
      barrier = (zero :: Vk.ImageMemoryBarrier '[])
        { Vk.srcAccessMask
        , Vk.dstAccessMask
        , Vk.oldLayout
        , Vk.newLayout
        , Vk.image
        , Vk.subresourceRange = zero
          { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
          , Vk.baseMipLevel = 0
          , Vk.levelCount = 1
          , Vk.baseArrayLayer = 0
          , Vk.layerCount = 1
          }
        }
    Vk.cmdPipelineBarrier
      commandBuffer
      sourceStage
      destinationStage
      zero
      Vector.empty
      Vector.empty
      (Vector.singleton $ SomeStruct barrier)
    pure ()

-- | Copies pixel data from the provided staging buffer into the provided image.
--
-- The image is fully covered, with a single mip level, single layer, and tightly packed rows.
copyBufferToImage
  :: (MonadResource m) => Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> Vk.Buffer -> Vk.Image -> Word32 -> Word32
  -> m ()
copyBufferToImage device commandPool graphicsQueue buffer image width height =
  withSingleTimeCommands device commandPool graphicsQueue \commandBuffer -> do
    let
      region = Vk.BufferImageCopy
        { Vk.bufferOffset = 0
        , Vk.bufferRowLength = 0
        , Vk.bufferImageHeight = 0
        , Vk.imageSubresource = Vk.ImageSubresourceLayers
          { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
          , Vk.mipLevel = 0
          , Vk.baseArrayLayer = 0
          , Vk.layerCount = 1
          }
        , Vk.imageOffset = Vk.Offset3D 0 0 0
        , Vk.imageExtent = Vk.Extent3D width height 1
        }
    Vk.cmdCopyBufferToImage
      commandBuffer
      buffer
      image
      Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      (Vector.singleton region)

-- | Helper function to create an image view for a given image with a format.
--
-- The image view each have one mip level, one array layer, and color aspect.
createImageView :: (MonadResource m) => Vk.Device -> Vk.Image -> Vk.Format -> m (ReleaseKey, Vk.ImageView)
createImageView device image format = do
  let
    imageViewCreateInfo = (zero :: Vk.ImageViewCreateInfo '[])
      { Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
      , Vk.format
      , Vk.subresourceRange = Vk.ImageSubresourceRange
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = 0
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }
      }
  allocate
    (Vk.createImageView device imageViewCreateInfo{Vk.image} Nothing)
    (flip (Vk.destroyImageView device) Nothing)

-- | Creates an image view for a texture.
--
-- See 'createImageView'.
createTextureImageView :: (MonadResource m) => Vk.Device -> Vk.Image -> m (ReleaseKey, Vk.ImageView)
createTextureImageView device textureImage =
  createImageView device textureImage Vk.FORMAT_R8G8B8A8_SRGB

-- | Creates a texture sampler with linear filtering and repeat address mode.
--
-- The anisotropy is enabled with the maximum supported sampler anisotropy.
createTextureSampler :: (MonadResource m) => Vk.Device -> Vk.PhysicalDevice -> m Vk.Sampler
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
      , Vk.maxLod = 0
      }
  Vk.withSampler device samplerInfo Nothing allocate'

-- | Helper to create a generic GPU buffer.
--
-- Queries memory requirements, finds a suitable buffer memory type, allocates, and device binds memory.
--
-- Returns the buffer and device memory along with their release keys.
createBuffer
  :: (MonadResource m) => Vk.PhysicalDevice -> Vk.Device -> Vk.DeviceSize -> Vk.BufferUsageFlags
  -> Vk.MemoryPropertyFlags -> m (ReleaseKey, ReleaseKey, Vk.Buffer, Vk.DeviceMemory)
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

-- | Helper to create static data buffers (like vertex and index buffers).
--
-- The vector is first copied into a host-visible staging buffer, then into a local-device buffer.
--
-- The staging buffer is released before the function returns.
createBuffer'
  :: forall bufferElem m. (MonadResource m, Storable bufferElem)
  => SVector.Vector bufferElem -> Vk.BufferUsageFlags
  -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> m (Vk.Buffer, Vk.DeviceMemory)
createBuffer' inBuffer bufferTypeBit physicalDevice device commandPool graphicsQueue = do
  let bufferSize = sizeOf (undefined :: bufferElem) * SVector.length inBuffer

  (stagingBufferReleaseKey, stagingBufferMemoryReleaseKey, stagingBuffer, stagingBufferMemory) <- createBuffer
    physicalDevice
    device
    (fromIntegral bufferSize)
    Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
    (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
  dataStaging <- Vk.mapMemory device stagingBufferMemory 0 (fromIntegral bufferSize) zero
  liftIO $ SVector.unsafeWith inBuffer \ptr ->
    copyBytes (castPtr dataStaging) ptr bufferSize
  Vk.unmapMemory device stagingBufferMemory

  (_bufferReleaseKey, _bufferMemoryReleaseKey, buffer, bufferMemory) <- createBuffer
    physicalDevice
    device
    (fromIntegral bufferSize)
    (bufferTypeBit .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  copyBuffer device commandPool graphicsQueue stagingBuffer buffer (fromIntegral bufferSize)

  release stagingBufferMemoryReleaseKey
  release stagingBufferReleaseKey

  pure (buffer, bufferMemory)

-- | Creates a vertex buffer for 'vertices'.
createVertexBuffer
  :: (MonadResource m) => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> m (Vk.Buffer, Vk.DeviceMemory)
createVertexBuffer = createBuffer' vertices Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT

-- | Creates an index buffer for 'indices'.
createIndexBuffer
  :: (MonadResource m) => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> m (Vk.Buffer, Vk.DeviceMemory)
createIndexBuffer = createBuffer' indices Vk.BUFFER_USAGE_INDEX_BUFFER_BIT

-- | Creates one host-visible, host-coherent buffer for each in-flight frame.
--
-- The buffers are mapped persisently.
--
-- Returns a vector containing each buffer, its memory, and its memory handle.
--
-- The memory handle is updated at 'updateUniformBuffer'.
createUniformBuffers
  :: (MonadResource m) => Vk.PhysicalDevice -> Vk.Device
  -> m (Vector (Vk.Buffer, Vk.DeviceMemory, Ptr UniformBufferObject))
createUniformBuffers physicalDevice device = do
  Vector.replicateM maxFramesInFlight do
    let bufferSize = sizeOf (undefined :: UniformBufferObject)
    (_bufferReleaseKey, _bufferMemoryReleaseKey, buffer, bufferMemory) <- createBuffer
      physicalDevice
      device
      (fromIntegral bufferSize)
      Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
      (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
    uniformBufferMapped <- Vk.mapMemory device bufferMemory 0 (fromIntegral bufferSize) zero
    pure (buffer, bufferMemory, castPtr uniformBufferMapped)

-- | Creates a pool sized to hold one UBO descriptor per in-flight frame.
createDescriptorPool :: (MonadResource m) => Vk.Device -> m Vk.DescriptorPool
createDescriptorPool device = do
  let
    uboPoolSize = Vk.DescriptorPoolSize
      { Vk.type' = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , Vk.descriptorCount = fromIntegral maxFramesInFlight
      }
    combinedImageSamplerPoolSize = Vk.DescriptorPoolSize
      { Vk.type' = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , Vk.descriptorCount = fromIntegral maxFramesInFlight
      }
    poolInfo = (zero :: Vk.DescriptorPoolCreateInfo '[])
      { Vk.flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
      , Vk.maxSets = fromIntegral maxFramesInFlight
      , Vk.poolSizes = Vector.fromList
        [ uboPoolSize
        , combinedImageSamplerPoolSize
        ]
      }
  Vk.withDescriptorPool device poolInfo Nothing allocate'

-- | Allocates one descriptor set per in-flight frame.
--
--
-- The following are written into each set's binding:
--
--     1. The uniform buffer at binding 0.
--     2. The texture sampler and image view at binding 1.
createDescriptorSets
  :: (MonadResource m)
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
  descriptorSets <- Vk.withDescriptorSets device allocInfo allocate'

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

-- | Allocates an one-time command buffer, records a copy-buffer, submits it, and waits for the queue to idle.
copyBuffer
  :: (MonadResource m)
  => Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize
  -> m ()
copyBuffer device commandPool graphicsQueue srcBuffer dstBuffer size =
  withSingleTimeCommands device commandPool graphicsQueue \commandCopyBuffer ->
    Vk.cmdCopyBuffer commandCopyBuffer srcBuffer dstBuffer (Vector.singleton $ Vk.BufferCopy 0 0 size)

-- | Searches the device's memory type table for an index that satisfies the filter bitmask and the provided set of properties.
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

-- | Helper to create 'maxFramesInFlight' numbers of command buffers.
createCommandBuffers :: (MonadResource m) => Vk.Device -> Vk.CommandPool -> m (Vector Vk.CommandBuffer)
createCommandBuffers device commandPool = do
  let
    allocInfo = (zero :: Vk.CommandBufferAllocateInfo)
      { Vk.commandPool
      , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
      , Vk.commandBufferCount = fromIntegral maxFramesInFlight
      }
  Vk.withCommandBuffers device allocInfo allocate'

-- | Creates the following synchronization objects:
--
--     * Per-frame semaphores to signal that an image has been acquired from the swapchain and is ready for rendering.
--     * Per-swapchain-image semaphores to signal that rendering has finished and presentation can happen.
--     * Per-frame fences to ensure only one frame is rendered at a time.
createSyncObjects
  :: (MonadResource m)
  => Vk.Device -> Vector Vk.Image
  -> m (Vector Vk.Semaphore, Vector Vk.Semaphore, Vector Vk.Fence)
createSyncObjects device swapchainImages = do
  presentCompleteSemaphores <- Vector.replicateM
    maxFramesInFlight
    (Vk.withSemaphore device zero Nothing allocate')
  renderFinishedSemaphores <- Vector.replicateM
    (Vector.length swapchainImages)
    (Vk.withSemaphore device zero Nothing allocate')
  inFlightFences <- Vector.replicateM
    maxFramesInFlight
    (Vk.withFence device zero{Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT} Nothing allocate')
  pure (presentCompleteSemaphores, renderFinishedSemaphores, inFlightFences)

-- | Initializes GLFW, Vulkan, and creates all necessary objects for 'Application'.
initVulkan :: (MonadResource m) => Bool -> Int -> Int -> m ApplicationEnv
initVulkan enableValidationLayers width height = do
  startTime <- liftIO Time.getCurrentTime
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
    createImageViews device swapchainSurfaceFormat swapchainImages
  descriptorSetLayout <- createDescriptorSetLayout device
  (pipelineLayout, graphicsPipeline) <-
    createGraphicsPipeline device descriptorSetLayout swapchainSurfaceFormat
  commandPool <- createCommandPool device queueIndex
  textureImage <- createTextureImage physicalDevice device commandPool queue
  (_textureImageViewReleaseKey, textureImageView) <- createTextureImageView device textureImage
  textureSampler <- createTextureSampler device physicalDevice
  (vertexBuffer, vertexBufferMemory) <-
    createVertexBuffer physicalDevice device commandPool queue
  (indexBuffer, indexBufferMemory) <-
    createIndexBuffer physicalDevice device commandPool queue
  uniformBuffers <- createUniformBuffers physicalDevice device
  descriptorPool <- createDescriptorPool device
  descriptorSets <- do
    let uniformBuffers' = fmap (view _1) uniformBuffers
    createDescriptorSets device descriptorPool descriptorSetLayout uniformBuffers' textureSampler textureImageView
  commandBuffers <- createCommandBuffers device commandPool
  (presentCompleteSemaphores, renderFinishedSemaphores, inFlightFences) <-
    createSyncObjects device swapchainImages
  let
    frames = Vector.zipWith5
      (\presentCompleteSemaphore
        inFlightFence
        commandBuffer
        (uniformBuffer, uniformBufferMemory, uniformBufferMapped)
        descriptorSet -> Frame{..})
      presentCompleteSemaphores
      inFlightFences
      commandBuffers
      uniformBuffers
      descriptorSets
  swapchainRef <- newIORef Swapchain
    { swapchain
    , surfaceFormat = swapchainSurfaceFormat
    , releaseKey = swapchainReleaseKey
    , images = swapchainImages
    , extent = swapchainExtent
    , imageViews = swapchainImageViews
    , imageViewsReleaseKeys = swapchainImageViewsReleaseKeys
    }
  pure ApplicationEnv{..}

-- | Records a full frame.
--
--     * Transitions the image to color attachment.
--     * Begins dynamic rendering (clears to black).
--     * Binds the pipeline.
--     * Sets dynamic viewport/scissor.
--     * Binds vertex/input buffers and the descriptor set.
--     * Draws.
--     * Ends rendering.
--     * Transitions the image to present layout.
recordCommandBuffer :: (MonadApplication m) => Word32 -> Frame -> m ()
recordCommandBuffer imageIndex frame = do
  ApplicationEnv{..} <- ask

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
  Vk.cmdBindIndexBuffer frame.commandBuffer indexBuffer 0 Vertex.indexType
  Vk.cmdBindDescriptorSets
    frame.commandBuffer
    Vk.PIPELINE_BIND_POINT_GRAPHICS
    pipelineLayout
    0
    (Vector.singleton frame.descriptorSet)
    Vector.empty
  Vk.cmdDrawIndexed frame.commandBuffer (fromIntegral $ SVector.length indices) 1 0 0 0

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

-- | Records a pipeline barrier to transition a swapchain image between two layouts with the specified access masks and pipeline stage masks.
transitionImageLayout
  :: (MonadApplication m)
  => Word32
  -> Vk.ImageLayout
  -> Vk.ImageLayout
  -> Vk.AccessFlags2
  -> Vk.AccessFlags2
  -> Vk.PipelineStageFlags2
  -> Vk.PipelineStageFlags2
  -> Frame
  -> m ()
transitionImageLayout
    imageIndex
    oldLayout
    newLayout
    srcAccessMask
    dstAccessMask
    srcStageMask
    dstStageMask
    frame = do
  ApplicationEnv{swapchainRef} <- ask
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
drawFrame :: (MonadApplication m) => Int -> m Bool
drawFrame frameIndex = do
  ApplicationEnv{..} <- ask
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
  continue :: (MonadApplication m) => Vector Vk.Fence -> Word32 -> Frame -> Swapchain -> m Bool
  continue drawFences imageIndex frame swapchain = do
    ApplicationEnv{..} <- ask

    updateUniformBuffer frame

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

-- | Computes the MVP matrices and writes the result into the frame's 'uniformBufferMapped'.
--
-- The model is rotated around the Z axis at 90 degrees per second.
--
-- The camera is located at (2, 2, 2), and looks towards the origin.
--
-- The FOV is set at 45 degrees and y-flipped for Vulkan.
updateUniformBuffer :: (MonadApplication m) => Frame -> m ()
updateUniformBuffer frame = do
  ApplicationEnv{startTime, swapchainRef} <- ask
  currentTime <- liftIO Time.getCurrentTime
  swapchain <- readIORef swapchainRef
  let
    time = realToFrac $ Time.diffUTCTime currentTime startTime
    -- Need to transpose the matrices to column-major.
    model = Linear.transpose $ Linear.mkTransformation
      (Linear.axisAngle (Linear.V3 0 0 1) (time * pi / 2))
      (Linear.V3 0 0 0)
    view' = Linear.transpose $ Linear.lookAt (Linear.V3 2 2 2) (Linear.V3 0 0 0) (Linear.V3 0 0 1)
    projFlipped = Linear.transpose $ Linear.perspective
      (pi / 4)
      (realToFrac swapchain.extent.width / realToFrac swapchain.extent.height)
      0.1
      10
    -- We negate the y coordinate's scaling factor because Vulkan's y axis points down.
    proj = projFlipped & Linear._y . Linear._y %~ negate
    ubo = UniformBufferObject{view = view', ..}
  liftIO $ poke frame.uniformBufferMapped ubo

-- | Checks whether a Vulkan exception was thrown indicating an out of date result.
--
-- If yes, it's returned as a result, otherwise the exception is rethrown.
catchOutOfDate :: (MonadUnliftIO m) => m Vk.Result -> m Vk.Result
catchOutOfDate action =
  action `catch` \exn@(VulkanException r) ->
    if r == Vk.ERROR_OUT_OF_DATE_KHR
    then pure r
    else throwIO exn

-- | Releases the swapchain's image views and the swapchain.
cleanupSwapchain :: (MonadResource m) => Vector ReleaseKey -> ReleaseKey -> m ()
cleanupSwapchain swapchainImageViewsReleaseKeys swapchainReleaseKey = do
  traverse_ release swapchainImageViewsReleaseKeys
  release swapchainReleaseKey

-- | Calls 'cleanupSwapchain' and recreates them, updating the swapchain reference.
--
-- This function should be called whenever the application is resized.
--
-- If the application is minimized, the application is paused.
recreateSwapchain :: (MonadApplication m) => m ()
recreateSwapchain = do
  ApplicationEnv{..} <- ask

  -- Pause while minimized
  liftIO $ whileM_
    (GLFW.getFramebufferSize window <&> \(width, height) -> width == 0 || height == 0)
    GLFW.waitEvents

  Vk.deviceWaitIdle device

  do
    Swapchain{..} <- readIORef swapchainRef
    cleanupSwapchain imageViewsReleaseKeys releaseKey
  (releaseKey, swapchain, surfaceFormat, images, extent) <-
    createSwapchain device physicalDevice surface window
  (imageViewsReleaseKeys, imageViews) <-
    createImageViews device surfaceFormat images
  writeIORef swapchainRef Swapchain{..}

-- | While the window should not close, pools events and renders frames.
mainLoop :: (MonadApplication m) => m ()
mainLoop = do
  ApplicationEnv{window} <- ask
  frameIndexRef <- newIORef 0
  whileM_ (liftIO $ not <$> GLFW.windowShouldClose window) do
    frameIndex <- readIORef frameIndexRef
    liftIO GLFW.pollEvents
    skippedFrame <- drawFrame frameIndex
    unless skippedFrame do
      writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

-- | Creates a window and renders the contents from the Vulkan triangle (now a square) tutorial.
defaultMain :: IO ()
defaultMain = catch
  (runResourceT do
    applicationEnv <- initVulkan enableValidationLayers defaultWidth defaultHeight
    flip runReaderT applicationEnv $ (.runApplication) @(Application _) do
      mainLoop `finally` Vk.deviceWaitIdle applicationEnv.device)
  \(err :: SomeException) ->
    hPutStrLn stderr $ displayException err
 where
  enableValidationLayers = True
