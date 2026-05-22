module Tutorial.Vulkan.Particles (defaultMain) where

import Control.Lens                 (Lens', _1, _3, view, (&), (^.))
import Control.Monad                (guard, unless, when)
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
import Data.Maybe                   (fromMaybe, isJust)
import Data.Ord                     (clamp)
import Data.Time                    qualified as Time
import Data.Traversable             (for)
import Data.Vector                  (Vector)
import Data.Vector                  qualified as Vector
import Data.Vector.Storable         qualified as SVector
import Data.Word                    (Word32, Word64)
import Graphics.UI.GLFW             qualified as GLFW
import Linear                       qualified as Linear
import System.FilePath              ((<.>), (</>))
import System.IO                    (hPutStrLn, stderr)
import System.Random.Stateful       (StatefulGen)
import System.Random.Stateful       qualified as Random
import UnliftIO                     (IORef, MonadUnliftIO)
import UnliftIO.Exception
  (Exception (displayException), SomeException, assert, catch, finally, throwIO)
import UnliftIO.Foreign
  (Ptr, Storable (..), alloca, castPtr, copyBytes, nullPtr, peekCString)
import UnliftIO.IORef
  (modifyIORef', newIORef, readIORef, writeIORef)
import Vulkan                       qualified as Vk
import Vulkan.CStruct.Extends       (SomeStruct (..), pattern (:&))
import Vulkan.Exception             (VulkanException (..))
import Vulkan.Zero                  (zero)

import Tutorial.Vulkan.Utils        (iFindIndex, iFindIndexM)

foreign import ccall unsafe "debug_callback.c &debug_callback"
  debugCallbackPtr :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT

type Frame :: Type
data Frame = Frame
  { inFlightFence        :: Vk.Fence
  , commandBuffer        :: Vk.CommandBuffer
  , computeCommandBuffer :: Vk.CommandBuffer
  , shaderStorageBuffer  :: Vk.Buffer
  , computeDescriptorSet :: Vk.DescriptorSet
  , computeUniformBuffer :: (Vk.Buffer, Vk.DeviceMemory, Ptr UniformBufferObject)
  }

type Swapchain :: Type
data Swapchain = Swapchain
  { swapchain     :: Vk.SwapchainKHR
  , surfaceFormat :: Vk.SurfaceFormatKHR
  , images        :: Vector Vk.Image
  , extent        :: Vk.Extent2D
  , imageViews    :: Vector Vk.ImageView
  }

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

type ApplicationEnv :: Type
data ApplicationEnv = ApplicationEnv
  { window                     :: GLFW.Window
  , frames                     :: Vector Frame
  , surface                    :: Vk.SurfaceKHR
  , physicalDevice             :: Vk.PhysicalDevice
  , device                     :: Vk.Device
  , queue                      :: Vk.Queue
  , swapchainRef               :: IORef Swapchain
  , descriptorSetLayout        :: Vk.DescriptorSetLayout
  , pipelineLayout             :: Vk.PipelineLayout
  , graphicsPipeline           :: Vk.Pipeline
  , framebufferResizedRef      :: IORef Bool
  , allocations                :: AllocatorEnv
  , semaphore                  :: Vk.Semaphore

  -- Compute
  , computeDescriptorSetLayout :: Vk.DescriptorSetLayout
  , computePipelineLayout      :: Vk.PipelineLayout
  , computePipeline            :: Vk.Pipeline
  }

type HasApplicationEnv :: Type -> Constraint
class HasApplicationEnv r where
  applicationEnvL :: Lens' r ApplicationEnv

instance HasAllocatorEnv ApplicationEnv where
  allocatorEnvL f env = (\allocations -> env{allocations}) <$> f env.allocations

instance HasApplicationEnv ApplicationEnv where
  applicationEnvL = id

type MonadApplication :: Type -> (Type -> Type) -> Constraint
type MonadApplication r m =
  ( HasApplicationEnv r
  , MonadScopedAllocator r m
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

particleCount :: Int
particleCount = 8192

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

-- | Loads a pre-compiled SPIR-V bytecode into a shader module.
createShaderModule :: (MonadScopedAllocator r m) => Vk.Device -> ByteString -> m (ReleaseKey, Vk.ShaderModule)
createShaderModule device code = do
  let createInfo = (zero :: Vk.ShaderModuleCreateInfo '[]){Vk.code}
  Vk.withShaderModule device createInfo Nothing allocate

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

-- | Creates a command pool with reset-command-buffer flag for the provided queue family index.
createCommandPool :: (MonadScopedAllocator r m) => Vk.Device -> Word32 -> m Vk.CommandPool
createCommandPool device queueIndex = do
  let
    poolInfo = (zero :: Vk.CommandPoolCreateInfo)
      { Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
      , Vk.queueFamilyIndex = queueIndex
      }
  Vk.withCommandPool device poolInfo Nothing (allocate' GlobalAllocatorScope)

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

updateUniformBuffer :: (MonadApplication r m) => Frame -> Float -> m ()
updateUniformBuffer frame lastFrameTime = do
  let ubo = UniformBufferObject{deltaTime = lastFrameTime * 2}
  liftIO $ poke (frame.computeUniformBuffer & view _3) ubo

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
  :: (MonadApplication r m) => Frame -> m ()
recordComputeCommandBuffer frame = do
  ApplicationEnv{computePipeline, computePipelineLayout} <- view applicationEnvL
  let commandBuffer = frame.computeCommandBuffer

  Vk.resetCommandBuffer commandBuffer zero
  Vk.beginCommandBuffer commandBuffer zero

  Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_COMPUTE computePipeline
  Vk.cmdBindDescriptorSets
    commandBuffer
    Vk.PIPELINE_BIND_POINT_COMPUTE
    computePipelineLayout
    0
    (Vector.singleton frame.computeDescriptorSet)
    Vector.empty
  Vk.cmdDispatch commandBuffer (fromIntegral particleCount `div` 256) 1 1

  Vk.endCommandBuffer commandBuffer

-- | Helper to create 'maxFramesInFlight' numbers of command buffers.
createComputeCommandBuffers
  :: (MonadScopedAllocator r m) => Vk.Device -> Vk.CommandPool -> m (Vector Vk.CommandBuffer)
createComputeCommandBuffers = createCommandBuffers

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
  :: forall ubo r m. (MonadScopedAllocator r m, Storable ubo)
  => Vk.PhysicalDevice -> Vk.Device -> m (Vector (Vk.Buffer, Vk.DeviceMemory, Ptr ubo))
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
initVulkan :: (MonadScopedAllocator r m) => Int -> Int -> m ApplicationEnv
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
  computeCommandBuffers <- createComputeCommandBuffers device commandPool
  (semaphore, inFlightFences) <- createSyncObjects device
  let
    frames = Vector.zipWith6
      (\inFlightFence
        commandBuffer
        computeCommandBuffer
        shaderStorageBuffer
        computeDescriptorSet
        computeUniformBuffer -> Frame{..})
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
    }
  allocations <- view allocatorEnvL
  pure ApplicationEnv{..}

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
recordCommandBuffer :: (MonadApplication r m) => Word32 -> Frame -> m ()
recordCommandBuffer imageIndex frame = do
  ApplicationEnv{..} <- view applicationEnvL
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
  Vk.cmdBindVertexBuffers frame.commandBuffer 0 (Vector.singleton frame.shaderStorageBuffer) (Vector.singleton 0)
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

-- | Records a pipeline barrier to transition a swapchain image between two layouts with the specified access masks and pipeline stage masks.
--
-- This function is called @transition_image_layout@ in the C++ tutorial.
transitionImageLayout
  :: (MonadApplication r m)
  => Vk.Image
  -> Vk.ImageLayout
  -> Vk.ImageLayout
  -> Vk.AccessFlags2
  -> Vk.AccessFlags2
  -> Vk.PipelineStageFlags2
  -> Vk.PipelineStageFlags2
  -> Vk.ImageAspectFlags
  -> Frame
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
drawFrame :: (MonadApplication r m) => Int -> Word64 -> Float -> m Bool
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
  continue :: (MonadApplication r m) => Word32 -> Frame -> Swapchain -> m Bool
  continue imageIndex frame swapchain = do
    ApplicationEnv{..} <- view applicationEnvL

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
        , Vk.commandBuffers = Vector.singleton $ Vk.commandBufferHandle frame.computeCommandBuffer
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

-- | Calls 'cleanupSwapchain' and recreates them, updating the swapchain reference.
--
-- This function should be called whenever the application is resized.
--
-- If the application is minimized, the application is paused.
recreateSwapchain :: (MonadApplication r m) => m ()
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
  writeIORef swapchainRef Swapchain{..}

-- | Helper to get the current time from GLFW. Throws if it couldn't.
getTime :: (MonadIO m) => m Double
getTime =
  liftIO GLFW.getTime >>= \case
    Nothing -> throwIO $ RuntimeError "Failed to get GLFW time"
    Just time -> pure time

-- | While the window should not close, pools events and renders frames.
mainLoop :: (MonadApplication r m) => m ()
mainLoop = do
  ApplicationEnv{window} <- view applicationEnvL
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
defaultMain = catch
  (runResourceT $ do
    swapchainAllocationsRef <- newIORef []
    applicationEnv <- flip runReaderT AllocatorEnv{..} $ (.runScopedAllocator) @(ScopedAllocator _) $
      initVulkan defaultWidth defaultHeight
    flip runReaderT applicationEnv $ (.runApplication) @(Application _) do
      mainLoop `finally` Vk.deviceWaitIdle applicationEnv.device)
  \(err :: SomeException) ->
    hPutStrLn stderr $ displayException err
