{-# LANGUAGE CPP #-}

module System.Win32.Errors.Foreign where

import Foreign
import System.Win32

#include "windows_cconv.h"

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
