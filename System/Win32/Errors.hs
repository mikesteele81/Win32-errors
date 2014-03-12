{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Win32.Errors
  ( ErrCode (..)
  , toDWORD
  , fromDWORD
  , Win32Error (..)
  , tryWin32
  , failIfFalse_
  , failIf
  , failUnlessSuccess
  , failWith
  , errorWin
  ) where

import Control.Exception
import Data.Bits
import Data.Char
import Data.Text as T
import Data.Text.Foreign as T
import Foreign
import Numeric
import System.Win32 (DWORD)
import qualified System.Win32 as Win32

import System.Win32.Errors.Foreign
import System.Win32.Errors.Types

-- |Actions calling out to Win32 may throw exceptions. Wrapping the action in
-- `tryWin32` will catch any `Win32Error` exceptions, but will allow any other
-- exception type to pass through.
tryWin32 :: IO a -> IO (Either Win32Error a)
tryWin32 = try

failIfFalse_ :: Text -> IO Bool -> IO ()
failIfFalse_ wh act = failIf not wh act >> return ()

-- |Copied from the Win32 package. Use this to throw a Win32 exception
-- when an action returns a value satisfying the given predicate.
-- The exception thrown will depend on a thead-local global error condition.
-- The supplied `Text` value should be set to the human-friendly name of the
-- action that triggered the error.
failIf :: (a -> Bool) -> Text -> IO a -> IO a
failIf p wh act = do
    v <- act
    if p v then errorWin wh else return v

-- |Perform the supplied action, and throw a `Win32Error` exception if the
-- return code is anything other than `Success`. The supplied action returns
-- a `DWORD` instead of an `ErrCode` so that foreign imports can be used more
-- conveniently.
failUnlessSuccess :: Text -> IO DWORD -> IO ()
failUnlessSuccess fn_name act = do
    r <- act
    if r == toDWORD Success then return () else failWith' fn_name r

-- |Windows maintains a thread-local value representing the previously triggered
-- error code. Calling `errorWin` will look up the value, and throw a `Win32Error`
-- exception. The supplied `Text` argument should be set to the name of the function
-- which triggered the error condition.
--
-- Calling this action when no error has occurred (0x00000000 -- ERROR_SUCCESS) will
-- result in an exception being thrown for the `Success` error code.
errorWin :: Text -> IO a
errorWin fn_name = Win32.getLastError >>= failWith' fn_name

-- |Like failWith, but avoid multiple conversions to and from 'ErrCode'.
failWith' :: Text -> DWORD -> IO a
failWith' fn_name err_code = do
    msg <- formatMessage err_code
    -- drop trailing \n
    let msg' = T.reverse . T.dropWhile isSpace . T.reverse $ msg
    throw $ Win32Error fn_name (fromDWORD err_code) msg'

-- |Throw a `Win32Error` exception for the given function name and error code.
failWith :: Text -> ErrCode -> IO a
failWith fn_name err_code = failWith' fn_name $ toDWORD err_code

#define FORMAT_MESSAGE_FROM_SYSTEM     0x00001000
#define FORMAT_MESSAGE_ALLOCATE_BUFFER 0x00000100

formatMessage :: DWORD -> IO Text
formatMessage err =
    -- Specifying FORMAT_MESSAGE_ALLOCATE_BUFFER changes the lpBuffer argument
    -- to a pointer to LPTSTR
    alloca $ \ ppBuffer -> do
    len <- c_FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM .|. FORMAT_MESSAGE_ALLOCATE_BUFFER)
                             nullPtr err 0 (castPtr ppBuffer) 0 nullPtr
    pBuffer <- peek ppBuffer
    if (len == 0 || pBuffer == nullPtr)
       then return $ "Error 0x" `T.append` T.pack (Numeric.showHex err "")
       else fromPtr pBuffer (fromIntegral len)
