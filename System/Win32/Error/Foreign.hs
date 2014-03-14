{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- module: System.Win32.Error.Foreign
-- copyright: (c) Michael Steele, 2014
-- license: BSD3
-- maintainer: mikesteele81@gmail.com
-- stability: experimental
-- portability: Windows
--
-- This module provides functions which can be used as drop-in replacements
-- for Win32 when writing wrappers to foreign imports.
--
-- You will likely need to import modules from Win32 as well. To avoid
-- accidentally calling the standard error handling functions it's a good idea
-- to hide a few names:
--
-- > import qualified System.Win32.Error.Foreign as E
-- > import System.Win32 hiding (failIfFalse_, failIf, failUnlessSuccess, failWith)
--
-- Handling error conditions in Windows revolves around a thread-local
-- global variable representing the most recent error condition. Functions
-- indicate that an error occurred in various ways. The C++ programmer will
-- observe that a function failed, and immediately call GetLastError to
-- retrieve details on the possible cause or to get a localized error message
-- which can be relayed to a human in some way.
--
-- There are some cases where an error code may mean different things
-- depending on varying context, but in general these codes are globally
-- unique. Microsoft documents which error codes may be expected for any
-- given function.
--
-- When working with functions exported by Win32, error conditions are dealt
-- with using the `IOError` exception type. Most native Win32 functions return
-- an error code which can be used to determine whether something went wrong
-- during its execution. By convention these functions are all named something
-- of the form "c_DoSomething" where "DoSomething" matches the name given by
-- Microsoft. A haskell wrapper function named "doSomething" will typically,
-- among other things, check this error code. Based on its value the operating
-- system will be queried for additional error information, and a Haskell
-- exception will be thrown.
--
-- Consider the `System.Win32.File.createFile` function used to
-- open existing files which may or may not actually exist.
--
-- > createFile "c:\\nofilehere.txt" gENERIC_READ
-- >            fILE_SHARE_NONE Nothing oPEN_EXISTING 0 Nothing
--
-- If no file by that name exists the underlying `Win32.c_CreateFile` call will
-- return `Win32.iNVALID_HANDLE_VALUE`. This will result in an `IOError` exception
-- being thrown with a `String` value indicating the function and file name.
-- Internally, the `IOError` will also contain the error code, which will be
-- converted to a general Haskell value.
--
-- The Win32-errors package works similarly. A (simplified) wrapper around
-- c_CreateFile could be written as follows. Source code
-- from the Win32 package often provides a good starting point:
--
-- > createFile name access mode = withTString name $ \ c_name ->
-- >     E.failIf (== E.toDWORD E.InvalidHandle) "CreateFile" $
-- >     c_CreateFile c_name access fILE_SHARE_NONE nullPtr
-- >                  mode 0 nullPtr
--
module System.Win32.Error.Foreign
  ( failIf
  , failIfFalse_
  , failIfNull
  , failUnlessSuccess
  , failWith
  , errorWin
  ) where

import Control.Exception
import Data.Char
import Data.Text as T
import Data.Text.Foreign as T
import Foreign
import Numeric
import System.Win32 (DWORD, LPTSTR)
import qualified System.Win32 as Win32

import System.Win32.Error.Types

#include "windows_cconv.h"

-- |This function mirrors the Win32 package's 'System.Win32.Types.failIfFalse_'
-- function.
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

-- |This function mirrors the Win32 package's 'System.Win32.Types.failIfNull'
-- function.
failIfNull :: Text -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

-- |Perform the supplied action, and throw a `Win32Exception` exception if the
-- return code is anything other than `Success`. The supplied action returns
-- a `DWORD` instead of an `ErrCode` so that foreign imports can be used more
-- conveniently.
failUnlessSuccess :: Text -> IO DWORD -> IO ()
failUnlessSuccess fn_name act = do
    r <- act
    if r == toDWORD Success then return () else failWith' fn_name r

-- |Windows maintains a thread-local value representing the previously triggered
-- error code. Calling `errorWin` will look up the value, and throw a `Win32Exception`
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
    throw $ Win32Exception fn_name (fromDWORD err_code) msg'

-- |Throw a `Win32Exception` exception for the given function name and error code.
failWith :: Text -> ErrCode -> IO a
failWith fn_name err_code = failWith' fn_name $ toDWORD err_code

#define FORMAT_MESSAGE_FROM_SYSTEM     0x00001000
#define FORMAT_MESSAGE_ALLOCATE_BUFFER 0x00000100

-- |This function doesn't belong in Win32-errors, which is why it isn't
-- exported. FormatMessage is required to get the standard system message
-- associated with an error code.
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

-- DWORD WINAPI FormatMessage(
--   _In_      DWORD dwFlags,
--   _In_opt_  LPCVOID lpSource,
--   _In_      DWORD dwMessageId,
--   _In_      DWORD dwLanguageId,
--   _Out_     LPTSTR lpBuffer,
--   _In_      DWORD nSize,
--   _In_opt_  va_list *Arguments
-- );
foreign import WINDOWS_CCONV "Windows.h FormatMessageW"
    c_FormatMessage :: DWORD -> Ptr () -> DWORD -> DWORD -> LPTSTR -> DWORD
                    -> Ptr () -> IO DWORD
