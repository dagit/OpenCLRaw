{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-| Declaration of types, bounds and constants for OpenCL 1.0 -}
module System.OpenCL.Raw.V10.Types where

import Foreign
import Control.Exception
import Data.Typeable

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <CL/cl.h>
#include <CL/cl_gl.h>

data DeviceIDc     = DeviceIDc
data Contextc      = Contextc
data CommandQueuec = CommandQueuec
data Memc          = Memc
data Programc      = Programc
data Kernelc       = Kernelc
data Eventc        = Eventc
data Samplerc      = Samplerc

type PlatformID   = IntPtr
type DeviceID     = Ptr DeviceIDc
type Context      = Ptr Contextc
type CommandQueue = Ptr CommandQueuec
type Mem          = Ptr Memc
type Program      = Ptr Programc
type Event        = Ptr Eventc
type Sampler      = Ptr Samplerc
type Kernel       = Ptr Kernelc

-- These are useful in the foreign import statements to make sure the type
-- match up with the API specification
type CLsizei                  = #type size_t
type CLint                    = #type cl_int
type CLuint                   = #type cl_uint
type CLbool                   = #type cl_bool
type CLulong                  = #type cl_ulong
type CLbitfield               = #type cl_bitfield
type CLDeviceType             = #type cl_device_type
type CLPlatformInfo           = #type cl_platform_info
type CLDeviceInfo             = #type cl_device_info
type CLDeviceFPConfig         = #type cl_device_fp_config
type CLDeviceMemCacheType     = #type cl_device_mem_cache_type
type CLDeviceLocalMemType     = #type cl_device_local_mem_type
type CLDeviceExecCapabilities = #type cl_device_exec_capabilities
type CLCommandQueueProperties = #type cl_command_queue_properties
type CLContextProperties      = IntPtr
type CLContextInfo            = #type cl_context_info
type CLCommandQueueInfo       = #type cl_command_queue_info
type CLChannelOrder           = #type cl_channel_order
type CLChannelType            = #type cl_channel_type
type CLMemFlags               = #type cl_mem_flags
type CLMemObjectType          = #type cl_mem_object_type
type CLMemInfo                = #type cl_mem_info
type CLImageInfo              = #type cl_image_info
type CLBufferCreateType       = #type cl_buffer_create_type
type CLAddressingMode         = #type cl_addressing_mode
type CLFilterMode             = #type cl_filter_mode
type CLSamplerInfo            = #type cl_sampler_info
type CLMapFlags               = #type cl_map_flags
type CLProgramInfo            = #type cl_program_info
type CLProgramBuildInfo       = #type cl_program_build_info
type CLBuildStatus            = #type cl_build_status
type CLKernelInfo             = #type cl_kernel_info
type CLKernelWorkGroupInfo    = #type cl_kernel_work_group_info
type CLEventInfo              = #type cl_event_info
type CLCommandType            = #type cl_command_type
type CLProfilingInfo          = #type cl_profiling_info

-- Defined in <CL/cl_gl.h>
type CLGLObjectType           = #type cl_gl_object_type
type CLGLTextureInfo          = #type cl_gl_texture_info
type CLGLPlatformInfo         = #type cl_gl_platform_info
type CLGLContextInfo          = #type cl_gl_context_info

data ImageFormat = ImageFormat
  { imageChannelOrder    :: ChannelOrder
  , imageChannelDataType :: ChannelType
  }
  deriving (Eq, Show, Typeable)

instance Storable ImageFormat where
  sizeOf _    = #size cl_image_format
  alignment _ = #alignment cl_image_format
  peek ptr = do
    order    <- (#peek cl_image_format, image_channel_order) ptr
    datatype <- (#peek cl_image_format, image_channel_data_type) ptr
    return $ ImageFormat order datatype
  poke ptr val = do
    (#poke cl_image_format, image_channel_order) ptr (imageChannelOrder val)
    (#poke cl_image_format, image_channel_data_type) ptr
                                                     (imageChannelDataType val)

type ImageFormatp = Ptr ImageFormat

-- | Note: These data constructor names differ from those in the spec.  Should
-- be 'origin' and 'size' but with the way Haskell records work, I thought
-- maybe it was better to prefix them with 'buffer'.
data BufferRegion = BufferRegion
  { bufferOrigin :: CLsizei
  , bufferSize   :: CLsizei
  }
  deriving (Eq, Show, Typeable)

instance Storable BufferRegion where
  sizeOf _    = #size cl_buffer_region
  alignment _ = #alignment cl_buffer_region
  peek ptr = do
    origin' <- (#peek cl_buffer_region, origin) ptr
    size'   <- (#peek cl_buffer_region, size) ptr
    return $ BufferRegion origin' size'
  poke ptr val = do
    (#poke cl_buffer_region, origin) ptr (bufferOrigin val)
    (#poke cl_buffer_region, size) ptr (bufferSize val)

newtype AddressingMode = AddressingMode CLAddressingMode
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype BuildStatus = BuildStatus CLBuildStatus
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ChannelOrder = ChannelOrder CLChannelOrder
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ChannelType = ChannelType CLChannelType
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype CommandQueueInfo = CommandQueueInfo CLCommandQueueInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype CommandQueueProperties =
  CommandQueueProperties CLCommandQueueProperties
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype CommandType = CommandType CLCommandType
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ContextInfo = ContextInfo CLContextInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
-- | This is a special case to support C99 intptr_t
type ContextProperties = CLContextProperties
newtype DeviceExecCapabilities =
  DeviceExecCapabilities CLDeviceExecCapabilities
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype DeviceFPConfig = DeviceFPConfig CLDeviceFPConfig
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype DeviceInfo = DeviceInfo CLDeviceInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype DeviceLocalMemType = DeviceLocalMemType CLDeviceLocalMemType
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype DeviceMemCacheType = DeviceMemCacheType CLDeviceMemCacheType
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype DeviceType = DeviceType CLDeviceType
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ErrorCode = ErrorCode CLint
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
instance Exception ErrorCode
newtype EventInfo = EventInfo CLEventInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype FilterMode = FilterMode CLFilterMode
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype GLObjectType = GLObjectType CLGLObjectType
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype GLTextureInfo = GLTextureInfo CLGLTextureInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ImageInfo = ImageInfo CLImageInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype KernelInfo = KernelInfo CLKernelInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype KernelWorkGroupInfo = KernelWorkGroupInfo CLKernelWorkGroupInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype MapFlags = MapFlags CLMapFlags
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype MemFlags = MemFlags CLMemFlags
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype MemInfo = MemInfo CLMemInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype MemObjectType = MemObjectType CLMemObjectType
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype PlatformInfo = PlatformInfo CLPlatformInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ProfilingInfo = ProfilingInfo CLProfilingInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ProgramBuildInfo = ProgramBuildInfo CLProgramBuildInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype ProgramInfo = ProgramInfo CLProgramInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)
newtype SamplerInfo = SamplerInfo CLSamplerInfo
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Bits, Storable, Typeable)

-------------------------------------------------------------------------------

#{enum AddressingMode, AddressingMode
  ,clAddressNone        = CL_ADDRESS_NONE
  ,clAddressClampToEdge = CL_ADDRESS_CLAMP_TO_EDGE
  ,clAddressClamp       = CL_ADDRESS_CLAMP
  ,clAddressRepeat      = CL_ADDRESS_REPEAT
}

-------------------------------------------------------------------------------

clFalse, clTrue :: CLbool 
clFalse = #const CL_FALSE
clTrue  = #const CL_TRUE

-------------------------------------------------------------------------------

#{enum BuildStatus, BuildStatus
  ,clBuildError      = CL_BUILD_ERROR
  ,clBuildInProgress = CL_BUILD_IN_PROGRESS
  ,clBuildNone       = CL_BUILD_NONE
  ,clBuildSuccess    = CL_BUILD_SUCCESS
}

-------------------------------------------------------------------------------

#{enum ChannelOrder, ChannelOrder
  ,clA         = CL_A
  ,clR         = CL_R
  ,clRG        = CL_RG
  ,clRA        = CL_RA
  ,clRGB       = CL_RGB
  ,clRGBA      = CL_RGBA
  ,clBGRA      = CL_BGRA
  ,clARGB      = CL_ARGB
  ,clIntensity = CL_INTENSITY
  ,clLuminance = CL_LUMINANCE
}

-------------------------------------------------------------------------------

#{enum ChannelType, ChannelType
  ,clFloat          = CL_FLOAT
  ,clHalfFloat      = CL_HALF_FLOAT
  ,clSignedInt16    = CL_SIGNED_INT16
  ,clSignedInt32    = CL_SIGNED_INT32
  ,clSignedInt8     = CL_SIGNED_INT8
  ,clSNormInt8      = CL_SNORM_INT8
  ,clSNormInt16     = CL_SNORM_INT16
  ,clUNormInt101010 = CL_UNORM_INT_101010
  ,clUNormInt16     = CL_UNORM_INT16
  ,clUNormInt8      = CL_UNORM_INT8
  ,clUNormShort555  = CL_UNORM_SHORT_555
  ,clUNormShort565  = CL_UNORM_SHORT_565
  ,clUnsignedInt16  = CL_UNSIGNED_INT16
  ,clUnsignedInt32  = CL_UNSIGNED_INT32
  ,clUnsignedInt8   = CL_UNSIGNED_INT8
}

-------------------------------------------------------------------------------

#{enum CommandQueueInfo, CommandQueueInfo
  ,clQueueContext        = CL_QUEUE_CONTEXT
  ,clQueueDevice         = CL_QUEUE_DEVICE
  ,clQueueProperties     = CL_QUEUE_PROPERTIES
  ,clQueueReferenceCount = CL_QUEUE_REFERENCE_COUNT
}

-------------------------------------------------------------------------------

#{enum CommandQueueProperties, CommandQueueProperties
  ,clQueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
  ,clQueueProfilingEnable          = CL_QUEUE_PROFILING_ENABLE
}

-------------------------------------------------------------------------------

#{enum CommandType, CommandType
  ,clCommandAcquireGLObjects  = CL_COMMAND_ACQUIRE_GL_OBJECTS 
  ,clCommandCopyBuffer        = CL_COMMAND_COPY_BUFFER
  ,clCommandCopyBufferToImage = CL_COMMAND_COPY_BUFFER_TO_IMAGE
  ,clCommandCopyImage         = CL_COMMAND_COPY_IMAGE
  ,clCommandCopyImageToBuffer = CL_COMMAND_COPY_IMAGE_TO_BUFFER
  ,clCommandMapBuffer         = CL_COMMAND_MAP_BUFFER
  ,clCommandMapImage          = CL_COMMAND_MAP_IMAGE
  ,clCommandMarker            = CL_COMMAND_MARKER
  ,clCommandNativeKernel      = CL_COMMAND_NATIVE_KERNEL
  ,clCommandNDRangeKernel     = CL_COMMAND_NDRANGE_KERNEL
  ,clCommandReadBuffer        = CL_COMMAND_READ_BUFFER
  ,clCommandReadImage         = CL_COMMAND_READ_IMAGE
  ,clCommandReleaseGLObjects  = CL_COMMAND_RELEASE_GL_OBJECTS
  ,clCommandTask              = CL_COMMAND_TASK
  ,clCommandUnmapMemObject    = CL_COMMAND_UNMAP_MEM_OBJECT
  ,clCommandWriteBuffer       = CL_COMMAND_WRITE_BUFFER
  ,clCommandWriteImage        = CL_COMMAND_WRITE_IMAGE
}

-------------------------------------------------------------------------------

#{enum ContextInfo, ContextInfo
  ,clContextDevices        = CL_CONTEXT_DEVICES
  ,clContextProperties     = CL_CONTEXT_PROPERTIES
  ,clContextReferenceCount = CL_CONTEXT_REFERENCE_COUNT
}

-------------------------------------------------------------------------------

clContextPlatform :: ContextProperties
clContextPlatform = #const CL_CONTEXT_PLATFORM

-------------------------------------------------------------------------------

#{enum DeviceExecCapabilities, DeviceExecCapabilities
  ,clExecKernel       = CL_EXEC_KERNEL
  ,clExecNativeKernel = CL_EXEC_NATIVE_KERNEL
}

-------------------------------------------------------------------------------

#{enum DeviceFPConfig, DeviceFPConfig
  ,clFPDenorm         = CL_FP_DENORM
  ,clFPFMA            = CL_FP_FMA
  ,clFPInfNan         = CL_FP_INF_NAN
  ,clFPRoundToInf     = CL_FP_ROUND_TO_INF
  ,clFPRoundToNearest = CL_FP_ROUND_TO_NEAREST
  ,clFPRoundToZero    = CL_FP_ROUND_TO_ZERO
}

-------------------------------------------------------------------------------

#{enum DeviceInfo, DeviceInfo
  ,clDeviceAddressBits                = CL_DEVICE_ADDRESS_BITS
  ,clDeviceAvailable                  = CL_DEVICE_AVAILABLE
  ,clDeviceCompilerAvailable          = CL_DEVICE_COMPILER_AVAILABLE
  ,clDeviceEndianLittle               = CL_DEVICE_ENDIAN_LITTLE
  ,clDeviceErrorCorrectionSupport     = CL_DEVICE_ERROR_CORRECTION_SUPPORT
  ,clDeviceExecutionCapabilities      = CL_DEVICE_EXECUTION_CAPABILITIES
  ,clDeviceExtensions                 = CL_DEVICE_EXTENSIONS
  ,clDeviceGlobalMemCacheSize         = CL_DEVICE_GLOBAL_MEM_CACHE_SIZE
  ,clDeviceGlobalMemCacheType         = CL_DEVICE_GLOBAL_MEM_CACHE_TYPE
  ,clDeviceGlobalMemCacheLineSize     = CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE
  ,clDeviceGlobalMemSize              = CL_DEVICE_GLOBAL_MEM_SIZE
  ,clDeviceImageSupport               = CL_DEVICE_IMAGE_SUPPORT
  ,clDeviceImage2DMaxHeight           = CL_DEVICE_IMAGE2D_MAX_HEIGHT
  ,clDeviceImage2DMaxWidth            = CL_DEVICE_IMAGE2D_MAX_WIDTH
  ,clDeviceImage3DMaxHeight           = CL_DEVICE_IMAGE3D_MAX_HEIGHT
  ,clDeviceImage3DMaxWidth            = CL_DEVICE_IMAGE3D_MAX_WIDTH
  ,clDeviceImage3DMaxDepth            = CL_DEVICE_IMAGE3D_MAX_DEPTH
  ,clDeviceLocalMemSize               = CL_DEVICE_LOCAL_MEM_SIZE
  ,clDeviceMLocalMemType              = CL_DEVICE_LOCAL_MEM_TYPE
  ,clDeviceMaxClockFrequency          = CL_DEVICE_MAX_CLOCK_FREQUENCY
  ,clDeviceMaxComputeUnits            = CL_DEVICE_MAX_COMPUTE_UNITS
  ,clDeviceMaxConstantArgs            = CL_DEVICE_MAX_CONSTANT_ARGS
  ,clDeviceMaxConstantBuffersize      = CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
  ,clDeviceMaxMemAllocSize            = CL_DEVICE_MAX_MEM_ALLOC_SIZE
  ,clDeviceMaxParameterSize           = CL_DEVICE_MAX_PARAMETER_SIZE
  ,clDeviceMaxReadImageArgs           = CL_DEVICE_MAX_READ_IMAGE_ARGS
  ,clDeviceMaxSamplers                = CL_DEVICE_MAX_SAMPLERS
  ,clDeviceMaxWorkGroupSize           = CL_DEVICE_MAX_WORK_GROUP_SIZE
  ,clDeviceMaxWorkItemDimensions      = CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS
  ,clDeviceMaxWorkItemSizes           = CL_DEVICE_MAX_WORK_ITEM_SIZES
  ,clDeviceMaxWriteImageArgs          = CL_DEVICE_MAX_WRITE_IMAGE_ARGS
  ,clDeviceMemBaseAddrAlign           = CL_DEVICE_MEM_BASE_ADDR_ALIGN
  ,clDeviceMinDataTypeAlignSize       = CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE
  ,clDeviceName                       = CL_DEVICE_NAME
  ,clDevicePlatform                   = CL_DEVICE_PLATFORM
  ,clDevicePreferredVectorWidthChar   = CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR
  ,clDevicePreferredVectorWidthDouble = CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE
  ,clDevicePreferredVectorWidthFloat  = CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT
  ,clDevicePreferredVectorWidthInt    = CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT
  ,clDevicePreferredVectorWidthLong   = CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG
  ,clDevicePreferredVectorWidthShort  = CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT
  ,clDeviceProfile                    = CL_DEVICE_PROFILE
  ,clDeviceProfilingTimerResolution   = CL_DEVICE_PROFILING_TIMER_RESOLUTION
  ,clDeviceQueueProperties            = CL_DEVICE_QUEUE_PROPERTIES
  ,clDeviceSingleFPConfig             = CL_DEVICE_SINGLE_FP_CONFIG
  ,clDeviceType                       = CL_DEVICE_TYPE
  ,clDeviceVendorID                   = CL_DEVICE_VENDOR_ID
  ,clDeviceVendor                     = CL_DEVICE_VENDOR
  ,clDeviceVersion                    = CL_DEVICE_VERSION
  ,clDriverVersion                    = CL_DRIVER_VERSION
}

-------------------------------------------------------------------------------

#{enum DeviceLocalMemType, DeviceLocalMemType
  ,clGlobal = CL_GLOBAL
  ,clLocal  = CL_LOCAL
}

-------------------------------------------------------------------------------

#{enum DeviceMemCacheType, DeviceMemCacheType
  ,clNone           = CL_NONE
  ,clReadOnlyCache  = CL_READ_ONLY_CACHE
  ,clReadWriteCache = CL_READ_WRITE_CACHE
}

-------------------------------------------------------------------------------

#{enum DeviceType, DeviceType
  ,clDeviceTypeDefault     = CL_DEVICE_TYPE_DEFAULT
  ,clDeviceTypeCPU         = CL_DEVICE_TYPE_CPU
  ,clDeviceTypeGPU         = CL_DEVICE_TYPE_GPU
  ,clDeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR
  ,clDeviceTypeAll         = CL_DEVICE_TYPE_ALL
}

-------------------------------------------------------------------------------

#{enum EventInfo, EventInfo
  ,clEventCommandQueue           = CL_EVENT_COMMAND_QUEUE
  ,clEventCommandType            = CL_EVENT_COMMAND_TYPE
  ,clEventReferenceCount         = CL_EVENT_REFERENCE_COUNT
  ,clEventCommandExecutionStatus = CL_EVENT_COMMAND_EXECUTION_STATUS
}

-------------------------------------------------------------------------------

#{enum FilterMode, FilterMode
  ,clFilterNearest = CL_FILTER_NEAREST
  ,clFilterLinear  = CL_FILTER_LINEAR
}

-------------------------------------------------------------------------------

#{enum GLObjectType, GLObjectType
  ,clGLObjectBuffer       = CL_GL_OBJECT_BUFFER
  ,clGLObjectTexture2D    = CL_GL_OBJECT_TEXTURE2D
  ,clGLObjectTexture3D    = CL_GL_OBJECT_TEXTURE3D
  ,clGLObjectRenderBuffer = CL_GL_OBJECT_RENDERBUFFER
}

-------------------------------------------------------------------------------

#{enum GLTextureInfo, GLTextureInfo
  ,clGLTextureTarget = CL_GL_TEXTURE_TARGET
  ,clGLMIPMapLevel   = CL_GL_MIPMAP_LEVEL
}

-------------------------------------------------------------------------------

#{enum ImageInfo, ImageInfo
  ,clImageFormat      = CL_IMAGE_FORMAT
  ,clImageElementSize = CL_IMAGE_ELEMENT_SIZE
  ,clImageRowPitch    = CL_IMAGE_ROW_PITCH
  ,clImageSlicePitch  = CL_IMAGE_SLICE_PITCH
  ,clImageWidth       = CL_IMAGE_WIDTH
  ,clImageHeight      = CL_IMAGE_HEIGHT
  ,clImageDepth       = CL_IMAGE_DEPTH
}

-------------------------------------------------------------------------------

#{enum KernelInfo, KernelInfo
  ,clKernelFunctionName   = CL_KERNEL_FUNCTION_NAME
  ,clKernelNumFlags       = CL_KERNEL_NUM_ARGS
  ,clKernelReferenceCount = CL_KERNEL_REFERENCE_COUNT
  ,clKernelContext        = CL_KERNEL_CONTEXT
  ,clKernelProgram        = CL_KERNEL_PROGRAM
}

-------------------------------------------------------------------------------

#{enum KernelWorkGroupInfo, KernelWorkGroupInfo
  ,clKernelWorkGroupSize        = CL_KERNEL_WORK_GROUP_SIZE
  ,clKernelCompileWorkGroupSize = CL_KERNEL_COMPILE_WORK_GROUP_SIZE
  ,clKernelLocalMemSize         = CL_KERNEL_LOCAL_MEM_SIZE
}

-------------------------------------------------------------------------------

#{enum MapFlags, MapFlags
  ,clMapRead  = CL_MAP_READ
  ,clMapWrite = CL_MAP_WRITE
}

-------------------------------------------------------------------------------

#{enum MemFlags, MemFlags
  ,clMemReadWrite    = CL_MEM_READ_WRITE
  ,clMemWriteOnly    = CL_MEM_WRITE_ONLY
  ,clMemReadOnly     = CL_MEM_READ_ONLY
  ,clMemUseHostPtr   = CL_MEM_USE_HOST_PTR
  ,clMemAllocHostPtr = CL_MEM_ALLOC_HOST_PTR
  ,clMemCopyHostPtr  = CL_MEM_COPY_HOST_PTR
}

-------------------------------------------------------------------------------

#{enum MemInfo, MemInfo
  ,clMemType           = CL_MEM_TYPE
  ,clMemFlags          = CL_MEM_FLAGS
  ,clMemSize           = CL_MEM_SIZE
  ,clMemHostPtr        = CL_MEM_HOST_PTR
  ,clMemMapCount       = CL_MEM_MAP_COUNT
  ,clMemReferenceCount = CL_MEM_REFERENCE_COUNT
  ,clMemContext        = CL_MEM_CONTEXT
}

-------------------------------------------------------------------------------

#{enum MemObjectType, MemObjectType
  ,clMemObjectBuffer  = CL_MEM_OBJECT_BUFFER
  ,clMemObjectImage2D = CL_MEM_OBJECT_IMAGE2D
  ,clMemObjectImage3D = CL_MEM_OBJECT_IMAGE3D
}

-------------------------------------------------------------------------------

#{enum PlatformInfo, PlatformInfo
  ,clPlatformProfile       = CL_PLATFORM_PROFILE
  ,clPlatformVersion       = CL_PLATFORM_VERSION
  ,clPlatformName          = CL_PLATFORM_NAME
  ,clPlatformVendor        = CL_PLATFORM_VENDOR
  ,clPlatformExtensions    = CL_PLATFORM_EXTENSIONS
}

-------------------------------------------------------------------------------

#{enum ProfilingInfo, ProfilingInfo
  ,clProfilingCommandQueued = CL_PROFILING_COMMAND_QUEUED
  ,clProfilingCommandSubmit = CL_PROFILING_COMMAND_SUBMIT
  ,clProfilingCommandStart  = CL_PROFILING_COMMAND_START
  ,clProfilingCommandEnd    = CL_PROFILING_COMMAND_END
}
 
-------------------------------------------------------------------------------

#{enum ProgramBuildInfo, ProgramBuildInfo
  ,clProgramBuildStatus  = CL_PROGRAM_BUILD_STATUS
  ,clProgramBuildOptions = CL_PROGRAM_BUILD_OPTIONS
  ,clProgramBuildLog     = CL_PROGRAM_BUILD_LOG
}

-------------------------------------------------------------------------------

#{enum ProgramInfo, ProgramInfo
  ,clProgramReferenceCount = CL_PROGRAM_REFERENCE_COUNT
  ,clProgramContext        = CL_PROGRAM_CONTEXT
  ,clProgramNumDevices     = CL_PROGRAM_NUM_DEVICES
  ,clProgramDevices        = CL_PROGRAM_DEVICES
  ,clProgramSource         = CL_PROGRAM_SOURCE
  ,clProgramBinarySizes    = CL_PROGRAM_BINARY_SIZES
  ,clProgramBinaries       = CL_PROGRAM_BINARIES
}

-------------------------------------------------------------------------------

#{enum SamplerInfo, SamplerInfo
  ,clSamplerReferenceCount   = CL_SAMPLER_REFERENCE_COUNT
  ,clSamplerContext          = CL_SAMPLER_CONTEXT
  ,clSamplerNormalizedCoords = CL_SAMPLER_NORMALIZED_COORDS
  ,clSamplerAddressingMode   = CL_SAMPLER_ADDRESSING_MODE
  ,clSamplerFilterMode       = CL_SAMPLER_FILTER_MODE
}

