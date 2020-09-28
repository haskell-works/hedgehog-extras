#include <fcntl.h>
#include <windows.h>

{-# LANGUAGE CPP                #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NumericUnderscores #-}

module Hedgehog.Extras.Internal.NamedPipes
  ( waitNamedPipe
  , eRROR_PIPE_BUSY
  ) where

import Control.Applicative
import Data.Bool
import Data.Eq
import Data.Function
import Data.String
import Foreign.C.String (withCString)
import System.IO (IO)
import System.Win32.Types hiding (try)

type TimeOut = DWORD
-- From https://github.com/input-output-hk/ouroboros-network/commit/a04a821335531a0e77d8fc5ad6e826925515f5f4

-- | Wait until a named pipe instance is available.  If there is no instance at
-- hand before the timeout, it will error with 'ERROR_SEM_TIMEOUT', i.e.
-- @invalid argument (The semaphore timeout period has expired)@
--
-- It returns 'True' if there is an available instance, subusequent
-- 'createFile' might still fail, if another thread will take turn and connect
-- before, or if the other end shuts down the name pipe.
--
-- It returns 'False' if timeout fired.
waitNamedPipe :: String  -- ^ pipe name
              -> TimeOut -- ^ nTimeOut
              -> IO Bool
waitNamedPipe name timeout =
    withCString name $ \ c_name -> do
      r <- c_WaitNamedPipe c_name timeout
      e <- getLastError
      if | r                      -> pure r
         | e == eRROR_SEM_TIMEOUT -> pure False
         | otherwise              -> failWith "waitNamedPipe" e
-- From https://github.com/input-output-hk/ouroboros-network/commit/a04a821335531a0e77d8fc5ad6e826925515f5f4

-- 'c_WaitNamedPipe' is a blocking call, hence the _safe_ import.
foreign import ccall safe "windows.h WaitNamedPipeA"
  c_WaitNamedPipe :: LPCSTR -- lpNamedPipeName
                  -> DWORD  -- nTimeOut
                  -> IO BOOL
-- From https://github.com/input-output-hk/ouroboros-network/commit/a04a821335531a0e77d8fc5ad6e826925515f5f4

-- | [ERROR_PIPE_BUSY](https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-#ERROR_PIPE_BUSY):
-- all pipe instances are busy.
--
eRROR_PIPE_BUSY :: ErrCode
eRROR_PIPE_BUSY = #const ERROR_PIPE_BUSY

eRROR_SEM_TIMEOUT :: ErrCode
eRROR_SEM_TIMEOUT = #const ERROR_SEM_TIMEOUT
