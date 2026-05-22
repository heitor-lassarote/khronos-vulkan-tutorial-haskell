module Tutorial.Vulkan.HelloTriangle (defaultMain) where

import Codec.Ktx2                          qualified as Ktx2
import Codec.Ktx2.Header                   qualified as Ktx2
import Codec.Ktx2.Read                     qualified as Ktx2
import Control.Lens                        (Lens', view, (&), (+~))
import Control.Monad                       (guard, unless, when)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Control.Monad.Loops                 (whileM_)
import Control.Monad.Reader                (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Resource
  (MonadResource, ReleaseKey, ResIO, allocate, allocate_, release, runResourceT)
import Data.Bits                           (Bits (..))
import Data.ByteString.Char8               (ByteString)
import Data.ByteString.Char8               qualified as BS
import Data.ByteString.Unsafe              (unsafeUseAsCStringLen)
import Data.Foldable                       (find, for_, traverse_)
import Data.Functor                        ((<&>))
import Data.Kind                           (Constraint, Type)
import Data.Maybe
  (fromJust, fromMaybe, isJust, isNothing)
import Data.Ord                            (clamp)
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
import System.IO                           (hPutStrLn, stderr)
import Text.GLTF.Loader                    qualified as GLTF
import UnliftIO                            (IORef, MonadUnliftIO)
import UnliftIO.Exception
  (Exception (displayException), SomeException, assert, catch, finally, throwIO)
import UnliftIO.Foreign
  (Ptr, Storable (..), alloca, castPtr, copyBytes, nullPtr, peekCString)
import UnliftIO.IORef
  (modifyIORef', newIORef, readIORef, writeIORef)
import Vulkan                              qualified as Vk
import Vulkan.CStruct.Extends              (SomeStruct (..), pattern (:&))
import Vulkan.Exception                    (VulkanException (..))
import Vulkan.Zero                         (zero)

import Tutorial.Vulkan.GameObject          (GameObject (..), modelMatrix)
import Tutorial.Vulkan.UniformBufferObject (UniformBufferObject (..))
import Tutorial.Vulkan.Utils
  (findM, iFindIndex, iFindIndexM, perspectiveVulkan)
import Tutorial.Vulkan.Vertex              (Index (..), Vertex (..))
import Tutorial.Vulkan.Vertex              qualified as Vertex

foreign import ccall unsafe "debug_callback.c &debug_callback"
  debugCallbackPtr :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT

type Frame :: Type
data Frame = Frame
  { presentCompleteSemaphore :: Vk.Semaphore
  , inFlightFence            :: Vk.Fence
  , commandBuffer            :: Vk.CommandBuffer
  , frameIndex               :: Word32
  }

type Swapchain :: Type
data Swapchain = Swapchain
  { swapchain      :: Vk.SwapchainKHR
  , surfaceFormat  :: Vk.SurfaceFormatKHR
  , images         :: Vector Vk.Image
  , extent         :: Vk.Extent2D
  , imageViews     :: Vector Vk.ImageView
  , depthImage     :: Vk.Image
  , depthImageView :: Vk.ImageView
  , colorImage     :: Vk.Image
  , colorImageView :: Vk.ImageView
  , framebuffers   :: Vector Vk.Framebuffer
  , renderPassM    :: Maybe Vk.RenderPass
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
  , allocations              :: AllocatorEnv
  , vertices                 :: SVector.Vector Vertex
  , indices                  :: SVector.Vector Index
  , msaaSamples              :: Vk.SampleCountFlagBits
  , gameObjects              :: Vector GameObject
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

modelPath :: FilePath
modelPath = "khronos-vulkan-tutorial-cpp" </> "attachments" </> "assets" </> "viking_room" <.> "glb"

texturePath :: FilePath
texturePath = "khronos-vulkan-tutorial-cpp" </> "attachments" </> "assets" </> "viking_room" <.> "ktx2"

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

-- | Queries the physical device for the maximum sample count for multisampling.
--
-- The device should support this for color and depth framebuffers.
--
-- If multisampling is not supported, defaults to 1 (multisampling disabled).
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
-- This function also retrieves the graphics+present queue and its index.
createLogicalDevice
  :: (MonadScopedAllocator r m) => Vk.PhysicalDevice -> Vk.SurfaceKHR -> Bool
  -> m (Vk.Device, Vk.Queue, Word32)
createLogicalDevice physicalDevice surface dynamicRendering = do
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
      :& (zero :: Vk.PhysicalDeviceExtendedDynamicStateFeaturesEXT){Vk.extendedDynamicState = True}
      :& ()
    requiredDeviceExtensions = Vector.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME
    deviceCreateInfo = (zero :: Vk.DeviceCreateInfo '[])
      { Vk.next = featureChain
      , Vk.queueCreateInfos = Vector.singleton $ SomeStruct deviceQueueCreateInfo
      , Vk.enabledExtensionNames = requiredDeviceExtensions
      }
  device <- Vk.withDevice physicalDevice deviceCreateInfo Nothing (allocate' GlobalAllocatorScope)
  graphicsQueue <- Vk.getDeviceQueue device (fromIntegral graphicsIndex) queueIndex
  pure (device, graphicsQueue, queueIndex)

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
  allocate
    (Vk.createShaderModule device createInfo Nothing)
    (flip (Vk.destroyShaderModule device) Nothing)

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

-- | Creates a command pool with reset-command-buffer flag for the provided queue family index.
createCommandPool :: (MonadScopedAllocator r m) => Vk.Device -> Word32 -> m Vk.CommandPool
createCommandPool device queueIndex = do
  let
    poolInfo = (zero :: Vk.CommandPoolCreateInfo)
      { Vk.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
      , Vk.queueFamilyIndex = queueIndex
      }
  Vk.withCommandPool device poolInfo Nothing (allocate' GlobalAllocatorScope)

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

-- | Creates one host-visible, host-coherent buffer for each in-flight frame.
--
-- The buffers are mapped persisently.
--
-- Returns a vector containing each buffer, its memory, and its memory handle.
--
-- The memory handle is updated at 'updateUniformBuffer'.
createUniformBuffers
  :: (MonadScopedAllocator r m) => Vk.PhysicalDevice -> Vk.Device
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
initVulkan :: (MonadScopedAllocator r m) => Int -> Int -> m ApplicationEnv
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
        frameIndex -> Frame{..})
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
    , depthImage
    , depthImageView
    , colorImage
    , colorImageView
    , framebuffers
    , renderPassM
    }
  allocations <- view allocatorEnvL
  pure ApplicationEnv{..}

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
recordCommandBuffer :: (MonadApplication r m) => Word32 -> Frame -> m ()
recordCommandBuffer imageIndex frame = do
  ApplicationEnv{..} <- view applicationEnvL
  swapchain <- readIORef swapchainRef
  let image = swapchain.images Vector.! fromIntegral imageIndex

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
    swapchain.colorImage
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
    swapchain.depthImage
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

  case swapchain.renderPassM of
    Nothing -> do
      let
        colorAttachmentInfo = (zero :: Vk.RenderingAttachmentInfo)
          { Vk.imageView = swapchain.colorImageView
          , Vk.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , Vk.resolveMode = Vk.RESOLVE_MODE_AVERAGE_BIT
          , Vk.resolveImageView = swapchain.imageViews Vector.! fromIntegral imageIndex
          , Vk.resolveImageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
          , Vk.clearValue = clearColor
          }
        depthAttachmentInfo = (zero :: Vk.RenderingAttachmentInfo)
          { Vk.imageView = swapchain.depthImageView
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
          , Vk.framebuffer = swapchain.framebuffers Vector.! fromIntegral imageIndex
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
      (Vector.singleton (descriptorSets Vector.! fromIntegral frame.frameIndex))
      Vector.empty
    Vk.cmdDrawIndexed frame.commandBuffer (fromIntegral $ SVector.length indices) 1 0 0 0

  case swapchain.renderPassM of
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
drawFrame :: (MonadApplication r m) => Int -> m Bool
drawFrame frameIndex = do
  ApplicationEnv{..} <- view applicationEnvL
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
  continue :: (MonadApplication r m) => Vector Vk.Fence -> Word32 -> Frame -> Swapchain -> m Bool
  continue drawFences imageIndex frame swapchain = do
    ApplicationEnv{..} <- view applicationEnvL

    updateUniformBuffers frame

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

-- | Computes the MVP matrices for each game object and writes the result into the object's 'uniformBufferMapped'.
--
-- The models are rotated around the Y axis at 0.1 radians per second.
--
-- The camera is located at (2, 2, 6), and looks towards the origin.
--
-- The FOV is set at 45 degrees and y-flipped for Vulkan.
updateUniformBuffers :: (MonadApplication r m) => Frame -> m ()
updateUniformBuffers frame = do
  ApplicationEnv{startTime, swapchainRef, gameObjects} <- view applicationEnvL
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
    liftIO $ poke (gameObject.uniformBuffersMapped Vector.! fromIntegral frame.frameIndex) ubo

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
  oldSwapchain <- readIORef swapchainRef

  -- Pause while minimized
  liftIO $ whileM_
    (GLFW.getFramebufferSize window <&> \(width, height) -> width == 0 || height == 0)
    GLFW.waitEvents

  Vk.deviceWaitIdle device

  cleanupSwapchain
  (swapchain, surfaceFormat, images, extent) <- createSwapchain device physicalDevice surface window
  imageViews <- createImageViews device surfaceFormat images
  renderPassM <- createRenderPass physicalDevice device surfaceFormat.format msaaSamples (isNothing oldSwapchain.renderPassM)
  (colorImage, colorImageView) <- createColorResources physicalDevice device extent surfaceFormat msaaSamples
  (depthImage, depthImageView) <- createDepthResources physicalDevice device extent msaaSamples
  framebuffers <- createFramebuffers device colorImageView depthImageView imageViews extent renderPassM
  writeIORef swapchainRef Swapchain{..}

-- | While the window should not close, pools events and renders frames.
mainLoop :: (MonadApplication r m) => m ()
mainLoop = do
  ApplicationEnv{window} <- view applicationEnvL
  frameIndexRef <- newIORef 0
  whileM_ (liftIO $ not <$> GLFW.windowShouldClose window) do
    frameIndex <- readIORef frameIndexRef
    liftIO GLFW.pollEvents
    shouldIncrementFrameCounter <- drawFrame frameIndex
    when shouldIncrementFrameCounter do
      writeIORef frameIndexRef $ (frameIndex + 1) `mod` maxFramesInFlight

-- | Creates a window and renders the contents from the Vulkan tutorial.
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
