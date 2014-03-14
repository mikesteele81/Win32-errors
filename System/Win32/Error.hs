{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- module: System.Win32.Error
-- copyright: (c) Michael Steele, 2014
-- license: BSD3
-- maintainer: mikesteele81@gmail.com
-- stability: experimental
-- portability: Windows
--
-- This package assumes that you will be using strict `T.Text` values for string
-- handling. Consider using the following language pragma and import
-- statements:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main where
-- >
-- > import Data.Text (Text)
-- > import qualified Data.Text as T
-- > import qualified Data.Text.Foreign as T
--
-- This module is intended to be imported qualified.
--
-- > import System.Win32.Errors (ErrCode, Win32Exception)
-- > import qualified System.Win32.Errors as E
--
-- See the 'Win32Exception' type's documentation for an instructions on
-- working with functions that may throw exceptions of this type.
module System.Win32.Error
  ( Win32Exception (..)
  , tryWin32
  , toDWORD
  , fromDWORD
  , ErrCode (..)
  ) where

import qualified Data.Text as T
import System.Win32.Error.Types
