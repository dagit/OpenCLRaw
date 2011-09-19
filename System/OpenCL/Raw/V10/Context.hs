{-# LANGUAGE ForeignFunctionInterface #-}
{-|
    Conforms to section 4.3 of the OpenCL 1.0 specification
-}
module System.OpenCL.Raw.V10.Context 
    (clCreateContext
    ,ContextCallback
    ,clCreateContextFromType
    ,clRetainContext
    ,clReleaseContext
    ,clGetContextInfo
    )
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Utils
import Foreign
import Foreign.C
import Control.Monad

-------------------------------------------------------------------------------

type ContextCallback = CString -> Ptr () -> CLsizei -> Ptr () -> IO ()
foreign import stdcall "clCreateContext"
  raw_clCreateContext :: Ptr ContextProperties
                      -> CLuint 
                      -> Ptr DeviceID
                      -> FunPtr ContextCallback
                      -> Ptr ()
                      -> Ptr CLint
                      -> IO Context

foreign import stdcall "wrapper"
  wrapCreateContextCallback :: ContextCallback -> IO (FunPtr ContextCallback)

clCreateContext :: [PlatformID] -> [DeviceID]
                -> Maybe ContextCallback -> Ptr () -> IO Context
clCreateContext ps devices pfn_notify user_dat
  | null ps   = go nullPtr
  | otherwise = allocaArray propertiesN $ \propertiesP -> do
    pokeArray0 0 propertiesP contexts
    go propertiesP
  where propertiesN = 2*(length ps)+1
        devicesN = length devices
        contexts = concatMap (\p->clContextPlatform:[p]) ps
        mbToFunPtr Nothing       = return nullFunPtr
        mbToFunPtr (Just notify) = wrapCreateContextCallback notify
        go ctxPtr = do
          allocaArray devicesN $ \devicesP -> do
            pokeArray devicesP devices
            pNotify <- mbToFunPtr pfn_notify
            wrapErrorPtr $ raw_clCreateContext ctxPtr
              (fromIntegral devicesN) devicesP pNotify user_dat

-------------------------------------------------------------------------------

foreign import stdcall "clCreateContextFromType"
  raw_clCreateContextFromType :: Ptr ContextProperties
                              -> CLbitfield
                              -> FunPtr ContextCallback
                              -> Ptr a -> Ptr CLint -> IO Context

clCreateContextFromType :: [PlatformID]
                        -> DeviceType
                        -> Maybe ContextCallback
                        -> Ptr () -> IO Context
clCreateContextFromType ps (DeviceType device) mb_notify dat
  | null ps   = go nullPtr
  | otherwise = allocaArray psN $ \psPtr -> do
    pokeArray0 0 psPtr contexts
    go psPtr
    where contexts = concatMap (\p->clContextPlatform:[p]) ps
          psN = 2*(length ps)+1
          go ctxPtr = do
            pNotify <- mbToFunPtr mb_notify
            wrapErrorPtr $ raw_clCreateContextFromType
                             ctxPtr device pNotify dat
          mbToFunPtr Nothing       = return nullFunPtr
          mbToFunPtr (Just notify) = wrapCreateContextCallback notify

-------------------------------------------------------------------------------

foreign import stdcall "clRetainContext"
  raw_clRetainContext :: Context -> IO CLint

clRetainContext :: Context -> IO ()
clRetainContext ctx = wrapError (raw_clRetainContext ctx)

-------------------------------------------------------------------------------

foreign import stdcall "clReleaseContext"
  raw_clReleaseContext :: Context -> IO CLint

clReleaseContext :: Context -> IO ()
clReleaseContext ctx = wrapError (raw_clReleaseContext ctx)

-------------------------------------------------------------------------------
 
foreign import stdcall "clGetContextInfo"
  raw_clGetContextInfo :: Context -> CLuint -> CLsizei
                       -> Ptr a -> Ptr CLsizei -> IO CLint

clGetContextInfo :: Context -> ContextInfo
                 -> CLsizei -> IO (ForeignPtr a, CLsizei)
clGetContextInfo ctx (ContextInfo param_name) param_size =
  wrapGetInfo (raw_clGetContextInfo ctx param_name) param_size
