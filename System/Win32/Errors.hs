{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Win32.Errors
  ( ErrCode (..)
  , toDWORD
  , fromDWORD
  , Win32Error (..)
  , tryWin32
  , failIfFalse_
  , failIf
  , errorWin
  , failWith
  ) where

import Control.Exception
import Data.Bits
import Data.Char
import Data.Text as T
import Data.Text.Foreign as T
import Data.Typeable
import Foreign
import Numeric
import System.Win32 (DWORD)
import qualified System.Win32 as Win32

import System.Win32.Errors.Foreign
import System.Win32.Errors.TH

-- |Win32 actions typically return an error code to indicate success or failure.
-- These codes are intended to be globally unique, though there may be some overlap.
-- MSDN documents which errors may be returned by any given action.
--
-- There are thousands of errors, so it would be impractical to add them all. The `Other`
-- constructor is used to represent error codes which are not handled specifically.
genErrCode

-- |Convert an `ErrCode` into a `DWORD`.
gentoDWORD

-- |Convert a `DWORD` into an `ErrCode`. Values which don't have a
-- corresponding constructor will end up becoming an `Other`.
genfromDWORD

instance Storable ErrCode where
  sizeOf _ = sizeOf (undefined :: DWORD)
  alignment _ = alignment (undefined :: DWORD)
  peek ptr = (peek . castPtr) ptr >>= return . fromDWORD
  poke ptr ec = poke (castPtr ptr) (toDWORD ec)

-- |Exception type for Win32 errors.
data Win32Error = Win32Error
    { function :: Text
    -- ^ The foreign action which triggered this exception.
    , errCode  :: ErrCode
    -- ^ The error code
    , systemMessage :: Text
    -- ^ The standard system message associated with the error code.
    } deriving (Typeable, Show)

instance Exception Win32Error

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

-- |Windows maintains a thread-local value representing the previously triggered
-- error code. Calling `errorWin` will look up the value, and throw a `Win32Error`
-- exception. The supplied `Text` argument should be set to the name of the function
-- which triggered the error condition.
--
-- Calling this action when no error has occurred (0x00000000 -- ERROR_SUCCESS) will
-- result in an exception being thrown for the `Success` error code.
errorWin :: Text -> IO a
errorWin fn_name = do
    err_code <- Win32.getLastError
    failWith fn_name err_code

-- |Throw a `Win32Error` exception for the given function name and error code.
failWith :: Text -> DWORD -> IO a
failWith fn_name err_code = do
    msg <- formatMessage err_code
    -- drop trailing \n
    let msg' = T.reverse . T.dropWhile isSpace . T.reverse $ msg
    throw $ Win32Error fn_name (fromDWORD err_code) msg'

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
