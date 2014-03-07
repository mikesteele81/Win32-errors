{-# LANGUAGE DeriveDataTypeable #-}

module System.Win32.Errors.Types where

import Control.Exception
import Data.Typeable

import Data.Text
import System.Win32 (DWORD)

data ErrCode
    = ErrorSuccess
    | ErrorOther DWORD
    deriving (Typeable, Show)

data Win32Error = Win32Error
    { function :: Text
    , errCode  :: ErrCode
    , systemMessage :: Text
    } deriving (Typeable, Show)

instance Exception Win32Error
