{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Win32.Errors
  ( ErrCode (..)
  , Win32Error (..)
  , tryWin32
  , toDWORD
  , fromDWORD
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
import Foreign
import Numeric
import System.Win32 (DWORD)
import qualified System.Win32 as Win32

import System.Win32.Errors.Foreign
import System.Win32.Errors.TH
import System.Win32.Errors.Types

-- Generate toDWORD and fromDWORD. This produces something like the following:
--     fromDWORD :: DWORD -> ErrCode
--     fromDWORD 0 = ErrorSuccess
--     fromDWORD # = ErrorSomethingElse
--     fromDWORD # = ErrorSomethingElse
--     fromDWORD # = ErrorSomethingElse
--     fromDWORD x = ErrorOther x
--
--     toDWORD :: ErrCode -> DWORD
--     toDWORD (ErrorOther x) = x
--     toDWORD errorSomethingElse = #
--     toDWORD errorSomethingElse = #
--     toDWORD errorSomethingElse = #
genConvert [ (0, ErrorSuccess)
           ]
tryWin32 :: IO a -> IO (Either Win32Error a)
tryWin32 = try

failIfFalse_ :: Text -> IO Bool -> IO ()
failIfFalse_ wh act = failIf not wh act >> return ()

failIf :: (a -> Bool) -> Text -> IO a -> IO a
failIf p wh act = do
    v <- act
    if p v then errorWin wh else return v

errorWin :: Text -> IO a
errorWin fn_name = do
    err_code <- Win32.getLastError
    failWith fn_name err_code

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
    alloca $ \ pBuffer -> do
    len <- c_FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM .|. FORMAT_MESSAGE_ALLOCATE_BUFFER)
                             nullPtr err 0 pBuffer 0 nullPtr
    if len == 0
       then return $ "Error 0x" `T.append` T.pack (Numeric.showHex err "")
       else fromPtr (castPtr pBuffer) (fromIntegral len)
