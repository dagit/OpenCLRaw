{-# LANGUAGE ForeignFunctionInterface #-}
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


foreign import stdcall "clGetPlatformIDs"
  raw_clGetPlatformIDs :: CLuint -> Ptr PlatformID -> Ptr CLuint -> IO CLint

clGetPlatformIDs :: IO [PlatformID]
clGetPlatformIDs = do
  alloca $ \numPtr -> do
    -- first we query to find out how big of a buffer is needed
    checkErr (raw_clGetPlatformIDs 0 nullPtr numPtr) $ do
      num <- peek numPtr
      allocaArray (fromIntegral num) $ \platformsPtr -> do
        -- now we are ready to query the result
        checkErr (raw_clGetPlatformIDs num platformsPtr nullPtr) $ do
          peekArray (fromIntegral num) platformsPtr
  where checkErr f g = do
          errcode <- ErrorCode <$> f
          if errcode == clSuccess then g else throw errcode

foreign import stdcall "clGetPlatformInfo"
  raw_clGetPlatformInfo :: PlatformID -> CLuint -> CLsizei -> Ptr CChar 
                        -> Ptr CLsizei -> IO CLint 

clGetPlatformInfo :: PlatformID -> PlatformInfo -> IO String
clGetPlatformInfo pid (PlatformInfo name) = do
  -- first we query to find out how big of a buffer is needed
  n <- fetchPtr $ raw_clGetPlatformInfo pid name 0 nullPtr
  allocaBytes (fromIntegral n) $ \valuePtr -> do
    -- now we are ready to query the result
    _ <- raw_clGetPlatformInfo pid name n valuePtr nullPtr
    peekCString valuePtr
