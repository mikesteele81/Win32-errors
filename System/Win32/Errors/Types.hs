{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Win32.Errors.Types where

import Control.Exception
import Data.Text
import Data.Typeable
import Foreign
import System.Win32.Types (DWORD)

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

