{-# LANGUAGE ForeignFunctionInterface #-}
{-| Module for querying extensions -}
module System.OpenCL.Raw.V10.Etc 
    (clGetExtensionFunctionAddress)
where

import Foreign
import Foreign.C

foreign import stdcall "clGetExtensionFunctionAddress" raw_clGetExtensionFunctionAddress :: CString -> IO (Ptr ())
clGetExtensionFunctionAddress :: String -> IO (Ptr ())
clGetExtensionFunctionAddress str = withCString str raw_clGetExtensionFunctionAddress
