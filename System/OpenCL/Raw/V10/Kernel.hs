{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.5 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.Kernel 
    (clCreateKernel
    ,clCreateKernelsInProgram
    ,clRetainKernel
    ,clReleaseKernel
    ,clSetKernelArg
    ,clGetKernelInfo
    ,clGetKernelWorkGroupInfo
    ,clEnqueueNDRangeKernel
    ,clEnqueueTask
    ,clEnqueueNativeKernel)
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Foreign.C
import Control.Applicative
import Control.Exception ( throw )


foreign import stdcall "clCreateKernel" raw_clCreateKernel :: Program -> CString -> Ptr CLint -> IO Kernel 
clCreateKernel :: Program -> CString -> IO Kernel
clCreateKernel program kernel_name = wrapErrorPtr $ raw_clCreateKernel program kernel_name 

foreign import stdcall "clCreateKernelsInProgram" raw_clCreateKernelsInProgram :: Program -> CLuint -> Ptr Kernel -> Ptr CLuint -> IO CLint 
clCreateKernelsInProgram :: Program -> CLuint -> IO [Kernel]
clCreateKernelsInProgram program num_kernels = allocaArray (fromIntegral num_kernels) $ \kernels -> do
    nkr <- fetchPtr $ raw_clCreateKernelsInProgram program num_kernels kernels
    peekArray (fromIntegral nkr) kernels

foreign import stdcall "clRetainKernel" raw_clRetainKernel :: Kernel -> IO CLint 
clRetainKernel :: Kernel -> IO ()
clRetainKernel kernel = wrapError $ raw_clRetainKernel kernel

foreign import stdcall "clReleaseKernel" raw_clReleaseKernel :: Kernel -> IO CLint 
clReleaseKernel :: Kernel -> IO ()
clReleaseKernel kernel = wrapError $ raw_clReleaseKernel kernel

foreign import stdcall "clSetKernelArg" raw_clSetKernelArg :: Kernel -> CLuint -> CLsizei -> Ptr () -> IO CLint
clSetKernelArg :: Kernel -> CLuint -> CLsizei -> Ptr () -> IO ()
clSetKernelArg kernel arg_index arg_size arg_value = 
    wrapError $ raw_clSetKernelArg kernel arg_index arg_size arg_value

foreign import stdcall "clGetKernelInfo" raw_clGetKernelInfo :: Kernel -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetKernelInfo :: Kernel -> KernelInfo -> CLsizei -> IO (ForeignPtr (), CLsizei)
clGetKernelInfo kernel (KernelInfo param_name) param_value_size = wrapGetInfo (raw_clGetKernelInfo kernel param_name) param_value_size

foreign import stdcall "clGetKernelWorkGroupInfo" raw_clGetKernelWorkGroupInfo :: Kernel -> DeviceID -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetKernelWorkGroupInfo :: Kernel -> DeviceID -> KernelWorkGroupInfo -> CLsizei -> IO (ForeignPtr (), CLsizei)
clGetKernelWorkGroupInfo kernel device (KernelWorkGroupInfo param_name) param_value_size = wrapGetInfo (raw_clGetKernelWorkGroupInfo kernel device param_name) param_value_size

foreign import stdcall "clEnqueueNDRangeKernel" raw_clEnqueueNDRangeKernel :: CommandQueue -> Kernel -> CLuint -> Ptr CLsizei -> Ptr CLsizei -> Ptr CLsizei -> CLuint -> Ptr Event  -> Ptr Event -> IO CLint
clEnqueueNDRangeKernel :: CommandQueue -> Kernel -> [CLsizei] -> [CLsizei] -> [Event] -> IO Event 
clEnqueueNDRangeKernel queue kernel global_work_sizeL local_work_sizeL event_wait_listL = 
    allocaArray work_dim $ \global_work_size ->
    allocaArray work_dim $ \local_work_size ->
    allocaArray num_events_in_wait_list $ \event_wait_list ->
    alloca $ \event -> do
        pokeArray global_work_size global_work_sizeL
        pokeArray local_work_size local_work_sizeL
        pokeArray event_wait_list event_wait_listL
        err <- ErrorCode <$> raw_clEnqueueNDRangeKernel queue kernel (fromIntegral work_dim) nullPtr global_work_size local_work_size (fromIntegral num_events_in_wait_list) event_wait_list event
        if err == clSuccess
          then peek event
          else throw err
    where work_dim = length global_work_sizeL
          num_events_in_wait_list = length event_wait_listL
        
foreign import stdcall "clEnqueueTask" raw_clEnqueueTask :: CommandQueue -> Kernel -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
clEnqueueTask :: CommandQueue -> Kernel -> [Event] -> IO Event
clEnqueueTask queue kernel event_wait_listL =
    allocaArray num_events_in_wait_list $ \event_wait_list ->
    alloca $ \event -> do
        pokeArray event_wait_list event_wait_listL
        err <- ErrorCode <$> raw_clEnqueueTask queue kernel (fromIntegral num_events_in_wait_list) event_wait_list event
        if err == clSuccess
          then peek event
          else throw err
    where num_events_in_wait_list = length event_wait_listL

type NKCallbackFunction = Ptr () -> IO ()
foreign import stdcall "wrapper" wrapNativeKernelCallback :: NKCallbackFunction -> IO (FunPtr NKCallbackFunction)
foreign import stdcall "clEnqueueNativeKernel" raw_clEnqueueNativeKernel :: FunPtr NKCallbackFunction -> Ptr () -> CLsizei -> CLuint -> Ptr Mem -> Ptr (Ptr ()) -> CLuint -> Ptr Event -> Ptr Event -> IO CLint 
clEnqueueNativeKernel :: NKCallbackFunction -> Ptr () -> CLsizei -> [Mem] -> [Ptr ()] -> [Event] -> IO Event
clEnqueueNativeKernel user_funcF args cb_args mem_listL args_mem_locL event_wait_listL = 
    allocaArray num_events_in_wait_list $ \event_wait_list ->
    allocaArray num_mem_objects $ \mem_list ->
    allocaArray (length args_mem_locL) $ \args_mem_loc ->
    alloca $ \event -> do
        user_func <- wrapNativeKernelCallback user_funcF
        pokeArray event_wait_list event_wait_listL
        pokeArray mem_list mem_listL
        pokeArray args_mem_loc args_mem_locL
        err <- ErrorCode <$> raw_clEnqueueNativeKernel user_func args cb_args (fromIntegral num_mem_objects) mem_list args_mem_loc (fromIntegral num_events_in_wait_list) event_wait_list event
        if err == clSuccess
          then peek event
          else throw err
    where num_events_in_wait_list = length event_wait_listL
          num_mem_objects = length mem_listL
