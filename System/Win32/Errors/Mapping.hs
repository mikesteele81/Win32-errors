module System.Win32.Errors.Mapping
  ( mapping
  ) where

import Language.Haskell.TH (mkName, Name)

import System.Win32.Types (DWORD)

mapping :: [(DWORD, Name)]
mapping = map (fmap mkName) $
    [ -- INVALID_HANDLE_VALUE is -1 for compatibility with 16-bit windows
      (fromIntegral (-1 :: Int), "InvalidHandleValue")
    , (0x00000000, "Success")
    , (0x00000002, "FileNotFound")
    , (0x00000003, "PathNotFound")
    , (0x00000005, "AccessDenied")
    , (0x00000006, "InvalidHandle")
    , (0x000000ea, "MoreData")
    , (0x00000103, "NoMoreItems")
    , (0x00001126, "NotAReparsePoint")
    , (0x00004e25, "DhcpSubnetNotPresent")
    , (0x00004e27, "DhcpElementCantRemove")
    , (0x00004e2a, "DhcpOptionNotPresent")
    , (0x00004e2d, "DhcpJetError")
    , (0x00004e32, "DhcpNotReservedClient")
    , (0x00004e37, "DhcpInvalidRange")
    ]
