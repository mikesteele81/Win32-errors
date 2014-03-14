{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Win32.Error.Types where

import Control.Exception
import Data.Text
import Data.Typeable
import Foreign
import System.Win32.Types (DWORD)

import System.Win32.Error.TH

-- |Win32 actions typically return an error code to indicate success or failure.
-- These codes are intended to be globally unique, though there may be some overlap.
-- MSDN documents which errors may be returned by any given action.
--
-- The naming of errors follows a convention. An error such as ERROR_SUCCESS
-- becomes `Success`, ERROR_FILE_NOT_FOUND becomes `FileNotFound`, and so
-- on. There are thousands of errors, so it would be impractical to add them
-- all. The `Other` constructor is used to represent error codes which are not
-- handled specifically.
--
-- User's of this library are encouraged to submit new error codes. Add new entries to
-- System.Win32.Errors.Mapping. Send your pull requests along with a link to relevent
-- documentation to
-- <https://github.com/mikesteele81/Win32-errors.git https://github.com/mikesteele81/Win32-errors.git>.
genErrCode

-- |Convert an `ErrCode` into a `DWORD`.
gentoDWORD

-- |Convert a `DWORD` into an `ErrCode`. Values which don't have a
-- corresponding constructor will end up becoming an `Other`.
genfromDWORD

-- |Performs marshalling by converting to and from `DWORD`.
instance Storable ErrCode where
  sizeOf _ = sizeOf (undefined :: DWORD)
  alignment _ = alignment (undefined :: DWORD)
  peek ptr = (peek . castPtr) ptr >>= return . fromDWORD
  poke ptr ec = poke (castPtr ptr) (toDWORD ec)

-- |Exception type for Win32 errors.
--
-- This type will be thrown as an extensible exception when a foreign call out
-- to part of the Win32 indicates that an error has occurred. In most cases you
-- should wrap an IO computation in a call to `tryWin32`.
--
-- The following example uses the custom 'createFile' function described in
-- "System.Win32.Error.Foreign":
--
-- > eHandle <- do
-- >     h <- E.tryWin32 $ createFile "c:\\missing.txt" gENERIC_READ oPEN_EXISTING
-- >     -- perform other actions
-- >     return h
-- > case eHandle of
-- >   Right handle -> do
-- >     -- do something with the file handle
-- >   Left w32Err -> do
-- >     case E.errCode w32Err of
-- >       E.InvalidHandle -> do
-- >         -- perform cleanup
-- >       -- handle other error codes.
-- >     T.putStrLn $ E.systemMessage w32Err
data Win32Exception = Win32Exception
    { function :: Text
    -- ^ The foreign action which triggered this exception.
    , errCode  :: ErrCode
    -- ^ The error code
    , systemMessage :: Text
    -- ^ The standard system message associated with the error code.
    } deriving (Typeable, Show)

instance Exception Win32Exception

-- |Actions calling out to Win32 may throw exceptions. Wrapping the action in
-- `tryWin32` will catch `Win32Exception` exceptions, but will allow any other
-- exception type to pass through.
tryWin32 :: IO a -> IO (Either Win32Exception a)
tryWin32 = try
