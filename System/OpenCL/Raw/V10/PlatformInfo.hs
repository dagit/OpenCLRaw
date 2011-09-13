{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
{-| Conforms to section 4.1 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.PlatformInfo (
    clGetPlatformIDs
  , clGetPlatformInfo
  ) where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Foreign.C
import Control.Applicative
import Control.Exception ( throw )


foreign import stdcall "clGetPlatformIDs" raw_clGetPlatformIDs :: CLuint -> Ptr PlatformID -> Ptr CLuint -> IO CLint


clGetPlatformIDs :: CLuint -> IO [PlatformID]
clGetPlatformIDs num_entries
  | num_entries < 1 = error "clGetPlatformIDs must have room for at least 1"
clGetPlatformIDs num_entries =
  allocaArray (fromIntegral num_entries) $ \(platforms::Ptr PlatformID) ->
    alloca $ \(num_platforms::Ptr CLuint) -> do
      errcode <- ErrorCode <$> raw_clGetPlatformIDs (fromIntegral num_entries) platforms num_platforms
      if errcode == clSuccess
          then do
            num <- peek num_platforms
            peekArray (fromIntegral num) platforms
          else throw errcode

foreign import stdcall "clGetPlatformInfo" raw_clGetPlatformInfo :: PlatformID -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint 

clGetPlatformInfo :: PlatformID -> PlatformInfo -> CLsizei -> Ptr () -> IO CLsizei
clGetPlatformInfo mem (PlatformInfo param_name) param_value_size param_value =
  fetchPtr $ raw_clGetPlatformInfo mem param_name param_value_size param_value
