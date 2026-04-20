#include <stdio.h>
#include <vulkan/vulkan.h>

VKAPI_ATTR VkBool32 VKAPI_CALL debug_callback(
    VkDebugUtilsMessageSeverityFlagBitsEXT     severity,
    VkDebugUtilsMessageTypeFlagsEXT            type,
    const VkDebugUtilsMessengerCallbackDataEXT *callback_data,
    void                                       *user_data
) {
  fprintf(stderr, "validation layer: type %u msg %s\n", type, callback_data->pMessage);
  return VK_FALSE;
}
