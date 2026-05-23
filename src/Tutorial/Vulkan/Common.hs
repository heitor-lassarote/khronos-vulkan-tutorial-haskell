module Tutorial.Vulkan.Common (module Tutorial.Vulkan.Common) where

import Control.Lens                        (Lens', view)
import Control.Monad                       (guard, unless, when)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Control.Monad.Reader                (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Resource
  (MonadResource, ReleaseKey, ResIO, allocate, allocate_, release, runResourceT)
import Data.Bits                           (Bits (..))
import Data.ByteString.Char8               (ByteString)
import Data.ByteString.Char8               qualified as BS
import Data.Foldable                       (find, for_, traverse_)
import Data.Kind                           (Constraint, Type)
import Data.Maybe                          (fromMaybe, isJust)
import Data.Ord                            (clamp)
import Data.Traversable                    (for)
import Data.Vector                         (Vector)
import Data.Vector                         qualified as Vector
import Data.Vector.Storable                qualified as SVector
import Data.Word                           (Word32)
import Graphics.UI.GLFW                    qualified as GLFW
import System.IO                           (hPutStrLn, stderr)
import UnliftIO
  (Exception (..), IORef, MonadUnliftIO)
import UnliftIO.Exception
  (SomeException, assert, catch, finally, throwIO)
import UnliftIO.Foreign
  (Ptr, Storable (..), alloca, castPtr, copyBytes, nullPtr, peekCString)
import UnliftIO.IORef
  (modifyIORef', newIORef, readIORef, writeIORef)
import Vulkan                              qualified as Vk
import Vulkan.CStruct.Extends              (SomeStruct (..), pattern (:&))
import Vulkan.Exception                    (VulkanException (..))
import Vulkan.Zero                         (zero)

import Tutorial.Vulkan.Utils               (iFindIndex, iFindIndexM)

foreign import ccall unsafe "debug_callback.c &debug_callback"
  debugCallbackPtr :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT

type FrameExtra :: Type -> Type
data family FrameExtra extra

type Frame :: Type -> Type
data Frame extra = Frame
  { inFlightFence :: Vk.Fence
  , commandBuffer :: Vk.CommandBuffer
  , extra         :: FrameExtra extra
  }

type SwapchainExtra :: Type -> Type
data family SwapchainExtra extra

type Swapchain :: Type -> Type
data Swapchain extra = Swapchain
  { swapchain     :: Vk.SwapchainKHR
  , surfaceFormat :: Vk.SurfaceFormatKHR
  , images        :: Vector Vk.Image
  , extent        :: Vk.Extent2D
  , imageViews    :: Vector Vk.ImageView
  , extra         :: SwapchainExtra extra
  }

type ApplicationExtra :: Type -> Type
data family ApplicationExtra extra

type ApplicationEnv :: Type -> Type
data ApplicationEnv extra = ApplicationEnv
  { window                :: GLFW.Window
  , frames                :: Vector (Frame extra)
  , surface               :: Vk.SurfaceKHR
  , physicalDevice        :: Vk.PhysicalDevice
  , device                :: Vk.Device
  , queue                 :: Vk.Queue
  , swapchainRef          :: IORef (Swapchain extra)
  , descriptorSetLayout   :: Vk.DescriptorSetLayout
  , pipelineLayout        :: Vk.PipelineLayout
  , graphicsPipeline      :: Vk.Pipeline
  , framebufferResizedRef :: IORef Bool
  , allocations           :: AllocatorEnv
  , extra                 :: ApplicationExtra extra
  }

type HasApplicationEnv :: Type -> Type -> Constraint
class HasApplicationEnv extra r where
  applicationEnvL :: Lens' r (ApplicationEnv extra)

instance HasAllocatorEnv (ApplicationEnv extra) where
  allocatorEnvL f env = (\allocations -> env{allocations}) <$> f env.allocations

instance HasApplicationEnv extra (ApplicationEnv extra) where
  applicationEnvL = id

type MonadApplication :: Type -> Type -> (Type -> Type) -> Constraint
type MonadApplication extra r m =
  ( HasApplicationEnv extra r
  , MonadScopedAllocator r m
  , MonadUnliftIO m
  )

type Application :: Type -> Type -> Type
newtype Application extra a = Application
  { runApplication :: ReaderT (ApplicationEnv extra) ResIO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadUnliftIO)

instance MonadReader (ApplicationEnv extra) (Application extra) where
  ask = Application ask
  local f = Application . local f . (.runApplication)

type RuntimeError :: Type
newtype RuntimeError = RuntimeError String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

type AllocatorScope :: Type
data AllocatorScope
  = GlobalAllocatorScope
  | SwapchainAllocatorScope

type AllocatorEnv :: Type
data AllocatorEnv = AllocatorEnv
  { swapchainAllocationsRef :: IORef [ReleaseKey]
  }

type HasAllocatorEnv :: Type -> Constraint
class HasAllocatorEnv r where
  allocatorEnvL :: Lens' r AllocatorEnv

instance HasAllocatorEnv AllocatorEnv where
  allocatorEnvL = id

type MonadScopedAllocator :: Type -> (Type -> Type) -> Constraint
type MonadScopedAllocator r m =
  ( HasAllocatorEnv r
  , MonadReader r m
  , MonadResource m
  , MonadUnliftIO m
  )

type ScopedAllocator :: Type -> Type
newtype ScopedAllocator a = ScopedAllocator
  { runScopedAllocator :: ReaderT AllocatorEnv ResIO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadUnliftIO)

instance MonadReader AllocatorEnv ScopedAllocator where
  ask = ScopedAllocator ask
  local f = ScopedAllocator . local f . (.runScopedAllocator)

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

-- | Allocates memory with @resourcet@'s 'allocate'.
allocate' :: (MonadScopedAllocator r m) => AllocatorScope -> IO a -> (a -> IO ()) -> m a
allocate' scope create destroy = do
  (releaseKey, resource) <- allocate create destroy
  case scope of
    GlobalAllocatorScope -> pure ()
    SwapchainAllocatorScope -> do
      AllocatorEnv{..} <- view allocatorEnvL
      modifyIORef' swapchainAllocationsRef (releaseKey :)
  pure resource

-- | Initializes the GLFW window with the provided width and height.
-- The bool reference is set to 'True' if the window is resized with GLFW's resize callback.
initWindow :: (MonadScopedAllocator r m) => Int -> Int -> IORef Bool -> m GLFW.Window
initWindow width height framebufferResizedRef = do
  _glfwKeyReleaseKey <- allocate_ GLFW.init GLFW.terminate

  liftIO $ GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  liftIO $ GLFW.windowHint $ GLFW.WindowHint'Resizable True
  window <- allocate' GlobalAllocatorScope
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
getRequiredInstanceExtensions :: (MonadIO m) => m (Vector ByteString)
getRequiredInstanceExtensions = do
  glfwExtensions <-
    traverse (fmap BS.pack . peekCString) . Vector.fromList =<< liftIO GLFW.getRequiredInstanceExtensions
  extensionProperties <-
    withResultCheck "Error enumerating instance extension properties" $
      Vk.enumerateInstanceExtensionProperties Nothing

  for_ glfwExtensions \glfwExtension ->
    unless (any ((== glfwExtension) . Vk.extensionName) extensionProperties) do
      throwIO $ RuntimeError $ "Unsupported GLFW extension: " <> BS.unpack glfwExtension

  let
    debugUtilsAvailable =
      elem Vk.EXT_DEBUG_UTILS_EXTENSION_NAME $ fmap (.extensionName) extensionProperties

  pure $ (if debugUtilsAvailable then Vector.cons Vk.EXT_DEBUG_UTILS_EXTENSION_NAME else id) glfwExtensions

-- | Creates a Vulkan instance, optionally enabling the validation layer.
createInstance :: (MonadScopedAllocator r m) => m Vk.Instance
createInstance = do
  glfwExtensions <- getRequiredInstanceExtensions

  let
    createInfo = (zero :: Vk.InstanceCreateInfo '[])
      { Vk.applicationInfo = Just Vk.ApplicationInfo
        { Vk.applicationName = Just "Hello Triangle"
        , Vk.applicationVersion = makeVersion 1 0 0
        , Vk.engineName = Just "No Engine"
        , Vk.engineVersion = makeVersion 1 0 0
        , Vk.apiVersion = Vk.API_VERSION_1_3
        }
      , Vk.enabledExtensionNames = glfwExtensions
      }

  Vk.withInstance createInfo Nothing (allocate' GlobalAllocatorScope)

-- | Optionally registers a debug messenger. See 'debugCallbackPtr'.
setupDebugMessenger :: (MonadScopedAllocator r m) => Vk.Instance -> m (Maybe Vk.DebugUtilsMessengerEXT)
setupDebugMessenger inst = catch
  do
    let
      createInfo = zero
        { Vk.messageSeverity = severityFlags
        , Vk.messageType = messageTypeFlags
        , Vk.pfnUserCallback = debugCallbackPtr
        }
    Just <$> Vk.withDebugUtilsMessengerEXT inst createInfo Nothing (allocate' GlobalAllocatorScope)
  (\(_ :: IOError) -> pure Nothing)
 where
  severityFlags =
    Vk.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
    .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT

  messageTypeFlags =
    Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
    .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT

getMaxUsableSampleCount :: (MonadIO m) => Vk.PhysicalDevice -> m Vk.SampleCountFlagBits
getMaxUsableSampleCount physicalDevice = do
  physicalDeviceProperties <- Vk.getPhysicalDeviceProperties physicalDevice
  let
    counts =
      physicalDeviceProperties.limits.framebufferColorSampleCounts
      .&. physicalDeviceProperties.limits.framebufferDepthSampleCounts
    countM = find
      (\count -> counts .&. count /= zeroBits)
      [ Vk.SAMPLE_COUNT_64_BIT, Vk.SAMPLE_COUNT_32_BIT, Vk.SAMPLE_COUNT_16_BIT
      , Vk.SAMPLE_COUNT_8_BIT, Vk.SAMPLE_COUNT_4_BIT, Vk.SAMPLE_COUNT_2_BIT
      ]
  pure $ fromMaybe Vk.SAMPLE_COUNT_1_BIT countM

-- | Picks the first physical device (GPU) that supports:
--
--     1. Vulkan API 1.3 or greater.
--     2. Graphics queues.
--     3. The swapchain extension.
--     4. Sampler anisotropy.
--     5. Sample rate shading.
--     6. Shaders.
--     7. Dynamic rendering.
--     8. @synchronization2@.
--     9. Extended dynamic state.
--
-- Additionally returns whether dynamic rendering is supported ('True' if supported).
pickPhysicalDevice :: (MonadIO m) => Vk.Instance -> m (Vk.PhysicalDevice, Bool)
pickPhysicalDevice inst = do
  physicalDevices <-
    withResultCheck "Error enumerating physical devices" $
      Vk.enumeratePhysicalDevices inst
  when (Vector.null physicalDevices) do
    throwIO $ RuntimeError "Failed to find GPUs with Vulkan support"
  devices <- flip Vector.mapMaybeM physicalDevices \physicalDevice -> do
    properties <- Vk.getPhysicalDeviceProperties physicalDevice
    supportedFeatures <- Vk.getPhysicalDeviceFeatures physicalDevice
    features <- Vk.getPhysicalDeviceFeatures2
      @'[ Vk.PhysicalDeviceVulkan11Features
        , Vk.PhysicalDeviceVulkan13Features
        , Vk.PhysicalDeviceExtendedDynamicStateFeaturesEXT
        , Vk.PhysicalDeviceTimelineSemaphoreFeaturesKHR
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
        :& physicalDeviceTimelineSemaphoreFeatures
        :& ()) = features.next
      -- TODO: Make synchronization2 support optional. I'm too lazy to do it now.
      --
      -- According to vulkan.gpuinfo.org, this is supported by 77.26% of the devices as of now (2026-05-16),
      -- so I think it's fine.
      --
      -- In contrast, dynamic rendering is supported by 67.06%.
      supportsRequiredFeatures =
        supportedFeatures.samplerAnisotropy
        && supportedFeatures.sampleRateShading
        && physicalDeviceVulkan11Features.shaderDrawParameters
        && physicalDeviceVulkan13Features.synchronization2
        && physicalDeviceExtendedDynamicStateFeatures.extendedDynamicState
        && physicalDeviceTimelineSemaphoreFeatures.timelineSemaphore
      supportsDynamicRendering =
        physicalDeviceVulkan13Features.dynamicRendering
        || elem Vk.KHR_DYNAMIC_RENDERING_EXTENSION_NAME ((.extensionName) <$> availableDeviceExtensions)

    pure do
      guard (supportsVulkan1_3 && supportsGraphics && supportsAllRequiredExtensions && supportsRequiredFeatures)
      pure (physicalDevice, supportsDynamicRendering)

  when (Vector.null devices) do
    throwIO $ RuntimeError "failed to find a suitable GPU!"
  let deviceWithDynamicRenderingSupport = Vector.find snd devices
  maybe (Vector.headM devices) pure deviceWithDynamicRenderingSupport

-- | Creates a logical device that supports all capabilities from 'pickPhysicalDevice'.
--
-- This function also retrieves the graphics+compute+present queue and its index.
createLogicalDevice
  :: (MonadScopedAllocator r m) => Vk.PhysicalDevice -> Vk.SurfaceKHR -> Bool
  -> m (Vk.Device, Vk.Queue, Word32)
createLogicalDevice physicalDevice surface dynamicRendering = do
  queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties physicalDevice

  -- Find queue family property that supports graphics, compute and present
  queueIndexMb <- iFindIndexM
    (\i qfp -> do
      hasSurfaceSupport <- Vk.getPhysicalDeviceSurfaceSupportKHR physicalDevice (fromIntegral i) surface
      pure $ (qfp.queueFlags .&. (Vk.QUEUE_GRAPHICS_BIT .|. Vk.QUEUE_COMPUTE_BIT) /= zeroBits) && hasSurfaceSupport)
    queueFamilyProperties
  queueIndex <- maybe
    (throwIO $ RuntimeError "Could not find a queue for graphics, compute and present -> terminating")
    (pure . fromIntegral)
    queueIndexMb

  let
    queuePriority = 0.5
    deviceQueueCreateInfo = (zero :: Vk.DeviceQueueCreateInfo '[])
      { Vk.queueFamilyIndex = queueIndex
      , Vk.queuePriorities = Vector.singleton queuePriority
      }
    featureChain =
      (zero :: Vk.PhysicalDeviceFeatures2 '[])
        { Vk.features = (zero :: Vk.PhysicalDeviceFeatures)
          { Vk.samplerAnisotropy = True
          , Vk.sampleRateShading = True
          }
        }
      :& (zero :: Vk.PhysicalDeviceVulkan11Features)
        { Vk.shaderDrawParameters = True
        }
      :& (zero :: Vk.PhysicalDeviceVulkan13Features)
        { Vk.dynamicRendering
        , Vk.synchronization2 = True
        }
      :& (zero :: Vk.PhysicalDeviceExtendedDynamicStateFeaturesEXT)
        { Vk.extendedDynamicState = True
        }
      :& (zero :: Vk.PhysicalDeviceTimelineSemaphoreFeaturesKHR)
        { Vk.timelineSemaphore = True
        }
      :& ()
    requiredDeviceExtensions = Vector.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME
    deviceCreateInfo = (zero :: Vk.DeviceCreateInfo '[])
      { Vk.next = featureChain
      , Vk.queueCreateInfos = Vector.singleton $ SomeStruct deviceQueueCreateInfo
      , Vk.enabledExtensionNames = requiredDeviceExtensions
      }
  device <- Vk.withDevice physicalDevice deviceCreateInfo Nothing (allocate' GlobalAllocatorScope)
  graphicsAndComputeQueue <- Vk.getDeviceQueue device queueIndex 0
  pure (device, graphicsAndComputeQueue, queueIndex)

-- | Uses GLFW to create a Vulkan surface.
createSurface :: (MonadScopedAllocator r m) => Vk.Instance -> GLFW.Window -> m Vk.SurfaceKHR
createSurface inst window = do
  allocate' GlobalAllocatorScope
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
  :: (MonadScopedAllocator r m)
  => Vk.Device
  -> Vk.PhysicalDevice
  -> Vk.SurfaceKHR
  -> GLFW.Window
  -> m (Vk.SwapchainKHR, Vk.SurfaceFormatKHR, Vector Vk.Image, Vk.Extent2D)
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
  swapchain <- Vk.withSwapchainKHR device swapchainCreateInfo Nothing (allocate' SwapchainAllocatorScope)
  swapchainImages <-
    withResultCheck "Failed to get swapchain images" $
      Vk.getSwapchainImagesKHR device swapchain
  pure (swapchain, swapchainSurfaceFormat, swapchainImages, swapchainExtent)

-- | Creates the views into the images obtained by 'createSwapchain'.
--
-- The images are allocated in the swapchain scope to be released.
--
-- See 'createImageView'.
createImageViews
  :: (MonadScopedAllocator r m)
  => Vk.Device
  -> Vk.SurfaceFormatKHR
  -> Vector Vk.Image
  -> m (Vector Vk.ImageView)
createImageViews device swapchainSurfaceFormat swapchainImages =
  let mipLevels = 1 in
  for swapchainImages \image ->
    createImageView device image mipLevels swapchainSurfaceFormat.format Vk.IMAGE_ASPECT_COLOR_BIT SwapchainAllocatorScope

-- | Loads a pre-compiled SPIR-V bytecode into a shader module.
createShaderModule :: (MonadScopedAllocator r m) => Vk.Device -> ByteString -> m (ReleaseKey, Vk.ShaderModule)
createShaderModule device code = do
  let createInfo = (zero :: Vk.ShaderModuleCreateInfo '[]){Vk.code}
  Vk.withShaderModule device createInfo Nothing allocate

-- | Creates a command pool with reset-command-buffer flag for the provided queue family index.
createCommandPool :: (MonadScopedAllocator r m) => Vk.Device -> Word32 -> m Vk.CommandPool
createCommandPool device queueIndex = do
  let
    poolInfo = (zero :: Vk.CommandPoolCreateInfo)
      { Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
      , Vk.queueFamilyIndex = queueIndex
      }
  Vk.withCommandPool device poolInfo Nothing (allocate' GlobalAllocatorScope)

-- | Creates a 2D image with undefined layout and the provided number of samples for multisampling.
--
-- Returns the allocated image and image memory.
--
-- The allocator scope indicates where to allocate these objects.
createImage
  :: (MonadScopedAllocator r m) => Vk.PhysicalDevice -> Vk.Device
  -> Word32 -> Word32 -> Word32
  -> Vk.Format -> Vk.ImageTiling -> Vk.ImageUsageFlags -> Vk.MemoryPropertyFlags
  -> Vk.SampleCountFlags -> AllocatorScope
  -> m (Vk.Image, Vk.DeviceMemory)
createImage physicalDevice device width height mipLevels format tiling usage properties numSamples scope = do
  let
    imageInfo = (zero :: Vk.ImageCreateInfo '[])
      { Vk.imageType = Vk.IMAGE_TYPE_2D
      , Vk.format
      , Vk.tiling
      , Vk.extent = Vk.Extent3D width height 1
      , Vk.mipLevels
      , Vk.arrayLayers = 1
      , Vk.samples = numSamples
      , Vk.usage
      , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
      , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
      }
  image <- Vk.withImage device imageInfo Nothing (allocate' scope)

  memRequirements <- Vk.getImageMemoryRequirements device image
  memoryTypeIndex <- findMemoryType physicalDevice memRequirements.memoryTypeBits properties
  let
    allocInfo = (zero :: Vk.MemoryAllocateInfo '[])
      { Vk.allocationSize = memRequirements.size
      , Vk.memoryTypeIndex
      }
  imageMemory <- Vk.withMemory device allocInfo Nothing (allocate' scope)
  Vk.bindImageMemory device image imageMemory 0

  pure (image, imageMemory)

-- | Allocates an one-time command buffer, performs an action, submits it,
-- waits for the queue to idle, and releases the command buffer.
withSingleTimeCommands
  :: (MonadScopedAllocator r m) => Vk.Device -> Vk.CommandPool -> Vk.Queue -> (Vk.CommandBuffer -> m a)
  -> m a
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
--
-- This function is called @transitionImageLayout@ in the C++ tutorial.
transitionImageLayout'
  :: (MonadScopedAllocator r m) => Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> Vk.Image -> Word32 -> Vk.ImageLayout -> Vk.ImageLayout -> m ()
transitionImageLayout' device commandPool graphicsQueue image mipLevels oldLayout newLayout =
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
        , Vk.subresourceRange = Vk.ImageSubresourceRange
          { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
          , Vk.baseMipLevel = 0
          , Vk.levelCount = mipLevels
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
  :: (MonadScopedAllocator r m) => Vk.Device -> Vk.CommandPool -> Vk.Queue
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
-- Each image view has one array layer.
--
-- The allocator scope indicates where to allocate these objects.
createImageView
  :: (MonadScopedAllocator r m)
  => Vk.Device -> Vk.Image -> Word32 -> Vk.Format -> Vk.ImageAspectFlags -> AllocatorScope
  -> m Vk.ImageView
createImageView device image mipLevels format aspectFlags scope = do
  let
    imageViewCreateInfo = (zero :: Vk.ImageViewCreateInfo '[])
      { Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
      , Vk.format
      , Vk.subresourceRange = Vk.ImageSubresourceRange
        { Vk.aspectMask = aspectFlags
        , Vk.baseMipLevel = 0
        , Vk.levelCount = mipLevels
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }
      , Vk.image
      }
  Vk.withImageView device imageViewCreateInfo Nothing (allocate' scope)

-- | Helper to create a generic GPU buffer.
--
-- Queries memory requirements, finds a suitable buffer memory type, allocates, and device binds memory.
--
-- Returns the buffer and device memory along with their release keys.
createBuffer
  :: (MonadScopedAllocator r m) => Vk.PhysicalDevice -> Vk.Device -> Vk.DeviceSize -> Vk.BufferUsageFlags
  -> Vk.MemoryPropertyFlags -> m (ReleaseKey, ReleaseKey, Vk.Buffer, Vk.DeviceMemory)
createBuffer physicalDevice device size usage properties = do
  let
    bufferInfo = (zero :: Vk.BufferCreateInfo '[])
      { Vk.size
      , Vk.usage
      , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
      }
  (bufferReleaseKey, buffer) <- Vk.withBuffer device bufferInfo Nothing allocate

  memRequirements <- Vk.getBufferMemoryRequirements device buffer
  memTypeIndex <- findMemoryType physicalDevice memRequirements.memoryTypeBits properties
  let
    memoryAllocateInfo = (zero :: Vk.MemoryAllocateInfo '[])
      { Vk.allocationSize = memRequirements.size
      , Vk.memoryTypeIndex = memTypeIndex
      }
  (bufferMemoryReleaseKey, bufferMemory) <- Vk.withMemory device memoryAllocateInfo Nothing allocate

  Vk.bindBufferMemory device buffer bufferMemory 0

  pure (bufferReleaseKey, bufferMemoryReleaseKey, buffer, bufferMemory)

-- | Helper to create static data buffers (like vertex and index buffers).
--
-- The vector is first copied into a host-visible staging buffer, then into a local-device buffer.
--
-- The staging buffer is released before the function returns.
createBuffer'
  :: forall bufferElem r m. (MonadScopedAllocator r m, Storable bufferElem)
  => Vk.BufferUsageFlags
  -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> SVector.Vector bufferElem
  -> m (Vk.Buffer, Vk.DeviceMemory)
createBuffer' bufferTypeBit physicalDevice device commandPool graphicsQueue inBuffer = do
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

-- | Creates one host-visible, host-coherent buffer for each in-flight frame.
--
-- The buffers are mapped persisently.
--
-- Returns a vector containing each buffer, its memory, and its memory handle.
--
-- The memory handle is updated at 'updateUniformBuffer'.
createUniformBuffers
  :: forall ubo r m
   . (MonadScopedAllocator r m, Storable ubo) => Vk.PhysicalDevice -> Vk.Device
  -> m (Vector (Vk.Buffer, Vk.DeviceMemory, Ptr ubo))
createUniformBuffers physicalDevice device = do
  Vector.replicateM maxFramesInFlight do
    let bufferSize = sizeOf (undefined :: ubo)
    (_bufferReleaseKey, _bufferMemoryReleaseKey, buffer, bufferMemory) <- createBuffer
      physicalDevice
      device
      (fromIntegral bufferSize)
      Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
      (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
    uniformBufferMapped <- Vk.mapMemory device bufferMemory 0 (fromIntegral bufferSize) zero
    pure (buffer, bufferMemory, castPtr uniformBufferMapped)

-- | Allocates an one-time command buffer, records a copy-buffer, submits it, and waits for the queue to idle.
copyBuffer
  :: (MonadScopedAllocator r m)
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
createCommandBuffers :: (MonadScopedAllocator r m) => Vk.Device -> Vk.CommandPool -> m (Vector Vk.CommandBuffer)
createCommandBuffers device commandPool = do
  let
    allocInfo = (zero :: Vk.CommandBufferAllocateInfo)
      { Vk.commandPool
      , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
      , Vk.commandBufferCount = fromIntegral maxFramesInFlight
      }
  Vk.withCommandBuffers device allocInfo (allocate' GlobalAllocatorScope)

-- | Records a pipeline barrier to transition a swapchain image between two layouts with the specified access masks and pipeline stage masks.
--
-- This function is called @transition_image_layout@ in the C++ tutorial.
transitionImageLayout
  :: (MonadApplication extra r m)
  => Vk.Image
  -> Vk.ImageLayout
  -> Vk.ImageLayout
  -> Vk.AccessFlags2
  -> Vk.AccessFlags2
  -> Vk.PipelineStageFlags2
  -> Vk.PipelineStageFlags2
  -> Vk.ImageAspectFlags
  -> Frame extra
  -> m ()
transitionImageLayout
    image
    oldLayout
    newLayout
    srcAccessMask
    dstAccessMask
    srcStageMask
    dstStageMask
    imageAspectFlags
    frame = do
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
      , Vk.image = image
      , Vk.subresourceRange = Vk.ImageSubresourceRange
        { Vk.aspectMask = imageAspectFlags
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

-- | Checks whether a Vulkan exception was thrown indicating an out of date result.
--
-- If yes, it's returned as a result, otherwise the exception is rethrown.
catchOutOfDate :: (MonadUnliftIO m) => m Vk.Result -> m Vk.Result
catchOutOfDate action =
  action `catch` \exn@(VulkanException r) ->
    if r == Vk.ERROR_OUT_OF_DATE_KHR
    then pure r
    else throwIO exn

-- | Releases the resources allocated to swapchain's scope.
cleanupSwapchain :: (MonadScopedAllocator r m) => m ()
cleanupSwapchain = do
  AllocatorEnv{swapchainAllocationsRef} <- view allocatorEnvL
  swapchainReleaseKeys <- readIORef swapchainAllocationsRef
  traverse_ release swapchainReleaseKeys
  writeIORef swapchainAllocationsRef []

-- | Creates a window and renders the contents from the Vulkan compute tutorial.
mkDefaultMain
  :: forall extra
   . (Int -> Int -> ScopedAllocator (ApplicationEnv extra))
  -> Application extra ()
  -> IO ()
mkDefaultMain initVulkan mainLoop = catch
  (runResourceT do
    swapchainAllocationsRef <- newIORef []
    applicationEnv <- flip runReaderT AllocatorEnv{..} $ (.runScopedAllocator) $
      initVulkan defaultWidth defaultHeight
    flip runReaderT applicationEnv $ (.runApplication) do
      mainLoop `finally` Vk.deviceWaitIdle applicationEnv.device)
  \(err :: SomeException) ->
    hPutStrLn stderr $ displayException err
