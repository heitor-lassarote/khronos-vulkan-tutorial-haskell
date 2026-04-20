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
import Data.Foldable (find, for_, traverse_)
import Data.Kind (Type)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ord (clamp)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word32)
import Graphics.UI.GLFW qualified as GLFW
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
  , glfwKey, windowKey, instanceKey, deviceKey, surfaceKey, swapChainKey :: ReleaseKey
  , debugMessengerKeyMb :: Maybe ReleaseKey
  , swapChainImageViewsKeys :: Vector ReleaseKey
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

initWindow :: Int -> Int -> ResIO (ReleaseKey, ReleaseKey, GLFW.Window)
initWindow width height = do
  glfwKey <- allocate_ GLFW.init GLFW.terminate

  liftIO $ GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  liftIO $ GLFW.windowHint $ GLFW.WindowHint'Resizable False
  (winKey, win) <-
    allocate
      ( GLFW.createWindow width height "Vulkan" Nothing Nothing >>= \case
          Nothing -> throwIO $ RuntimeError "Could not create window"
          Just window -> pure window
      )
      GLFW.destroyWindow

  pure (glfwKey, winKey, win)

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

createInstance :: Bool -> ResIO (ReleaseKey, Vk.Instance)
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

  allocate
    (Vk.createInstance appInfo Nothing)
    (\inst -> Vk.destroyInstance inst Nothing)
 where
  requiredLayers =
    if enableValidationLayers
      then Vector.singleton "VK_LAYER_KHRONOS_validation"
      else Vector.empty

setupDebugMessenger :: Bool -> Vk.Instance -> ResIO (Maybe (ReleaseKey, Vk.DebugUtilsMessengerEXT))
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
      Just
        <$> allocate
          (Vk.createDebugUtilsMessengerEXT inst createInfo Nothing)
          (flip (Vk.destroyDebugUtilsMessengerEXT inst) Nothing)
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
        @'[Vk.PhysicalDeviceVulkan13Features, Vk.PhysicalDeviceExtendedDynamicStateFeaturesEXT]
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
      (physicalDeviceVulkan13Features, (physicalDeviceExtendedDynamicStateFeatures, ())) = features.next
      supportsRequiredFeatures =
        physicalDeviceVulkan13Features.dynamicRendering && physicalDeviceExtendedDynamicStateFeatures.extendedDynamicState

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

createLogicalDevice :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> ResIO (ReleaseKey, Vk.Device, Vk.Queue)
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
      pure
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
        :& (zero :: Vk.PhysicalDeviceVulkan13Features){Vk.dynamicRendering = True}
        :& (zero :: Vk.PhysicalDeviceExtendedDynamicStateFeaturesEXT){Vk.extendedDynamicState = True}
        :& ()
    requiredDeviceExtensions = Vector.singleton Vk.KHR_SWAPCHAIN_EXTENSION_NAME
    deviceCreateInfo =
      (zero :: Vk.DeviceCreateInfo '[])
        { Vk.next = featureChain
        , Vk.queueCreateInfos = Vector.singleton $ SomeStruct deviceQueueCreateInfo
        , Vk.enabledExtensionNames = requiredDeviceExtensions
        }
  (deviceReleaseKey, device) <-
    allocate
      (Vk.createDevice physicalDevice deviceCreateInfo Nothing)
      (flip Vk.destroyDevice Nothing)
  graphicsQueue <- Vk.getDeviceQueue device (fromIntegral graphicsIndex) (fromIntegral queueIndex)
  pure (deviceReleaseKey, device, graphicsQueue)

createSurface :: Vk.Instance -> GLFW.Window -> ResIO (ReleaseKey, Vk.SurfaceKHR)
createSurface inst window = do
  allocate
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
  ResIO (ReleaseKey, Vk.SwapchainKHR, Vk.SurfaceFormatKHR, Vector Vk.Image)
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
  (swapChainReleaseKey, swapChain) <-
    allocate
      (Vk.createSwapchainKHR device swapChainCreateInfo Nothing)
      (flip (Vk.destroySwapchainKHR device) Nothing)
  swapChainImages <-
    withResultCheck "Failed to get swapchain images" $
      Vk.getSwapchainImagesKHR device swapChain
  pure (swapChainReleaseKey, swapChain, swapChainSurfaceFormat, swapChainImages)

createImageViews ::
  Vk.SurfaceFormatKHR ->
  Vector Vk.Image ->
  Vk.Device ->
  ResIO (Vector (ReleaseKey, Vk.ImageView))
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
        allocate
          ( Vk.createImageView
              device
              imageViewCreateInfo{Vk.image}
              Nothing
          )
          (flip (Vk.destroyImageView device) Nothing)
    )
    swapChainImages

initVulkan ::
  Bool ->
  GLFW.Window ->
  ResIO
    ( (ReleaseKey, Vk.Instance)
    , Maybe (ReleaseKey, Vk.DebugUtilsMessengerEXT)
    , (ReleaseKey, Vk.Device, Vk.Queue)
    , (ReleaseKey, Vk.SurfaceKHR)
    , (ReleaseKey, Vk.SwapchainKHR, Vk.SurfaceFormatKHR, Vector Vk.Image)
    , (Vector (ReleaseKey, Vk.ImageView))
    )
initVulkan enableValidationLayers window = do
  insts@(_, inst) <- createInstance enableValidationLayers
  dbgMsgsMb <- setupDebugMessenger enableValidationLayers inst
  surfaces@(_, surface) <- createSurface inst window
  physicalDevice <- liftIO $ pickPhysicalDevice inst
  logicalDevices@(_, device, _) <- createLogicalDevice physicalDevice surface
  swapChains@(_, _, swapChainSurfaceFormat, swapChainImages) <- createSwapChain device physicalDevice surface window
  imageViews <- createImageViews swapChainSurfaceFormat swapChainImages device
  pure (insts, dbgMsgsMb, logicalDevices, surfaces, swapChains, imageViews)

mainLoop :: MonadApplication ()
mainLoop = do
  window <- asks (.window)
  whileM_ (liftIO $ fmap not $ GLFW.windowShouldClose window) do
    liftIO GLFW.pollEvents

cleanup :: MonadApplication ()
cleanup = do
  Application{..} <- ask
  traverse_ release swapChainImageViewsKeys
  traverse_ release [swapChainKey, surfaceKey, deviceKey]
  traverse_ release debugMessengerKeyMb
  traverse_ release [instanceKey, windowKey, glfwKey]

defaultMain :: IO ()
defaultMain = do
  catch
    ( runResourceT do
        (glfwKey, windowKey, window) <- initWindow width height
        ( (instanceKey, _inst)
          , dbgMsgsMb
          , (deviceKey, _device, _queue)
          , (surfaceKey, _surface)
          , (swapChainKey, _swapChain, _swapChainSurfaceFormat, _swapChainImages)
          , (Vector.unzip -> (swapChainImageViewsKeys, _swapChainImageViews))
          ) <-
          initVulkan enableValidationLayers window
        let debugMessengerKeyMb = fst <$> dbgMsgsMb
        flip runReaderT Application{..} $ (.runMonadApplication) do
          mainLoop
          cleanup
    )
    \(err :: SomeException) ->
      hPutStrLn stderr $ displayException err
 where
  width = defaultWidth
  height = defaultHeight
  enableValidationLayers = True
