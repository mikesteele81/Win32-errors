module System.Win32.Error.Mapping
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
    , (0x0000000d, "InvalidData")
    , (0x0000000f, "InvalidDrive")
    , (0x00000010, "CurrentDirectory")
    , (0x00000012, "NoMoreFiles")
    , (0x00000078, "CallNotImplemented")
    , (0x000000ea, "MoreData")
    , (0x00000103, "NoMoreItems")
    , (0x00000420, "ServiceAlreadyRunning")
    , (0x00000422, "ServiceDisabled")
    , (0x00000424, "ServiceDoesNotExist")
    , (0x00000425, "ServiceCannotAcceptCtrl")
    , (0x00000426, "ServiceNotActive")
    , (0x00000427, "FailedServiceControllerConnect")
    , (0x00000428, "ExceptionInService")
    , (0x0000042a, "ServiceSpecificError")
    , (0x0000043b, "ServiceNotInExe")
    , (0x000006ba, "RPCSServerUnavailable")
    , (0x000006bb, "RPCSServerTooBusy")
    , (0x00001126, "NotAReparsePoint")
    , (0x00004e25, "DhcpSubnetNotPresent")
    , (0x00004e27, "DhcpElementCantRemove")
    , (0x00004e2a, "DhcpOptionNotPresent")
    , (0x00004e2d, "DhcpJetError")
    , (0x00004e32, "DhcpNotReservedClient")
    , (0x00004e33, "DhcpReservedClient")
    , (0x00004e35, "DhcpIprangeExists")
    , (0x00004e36, "DhcpReservedipExists")
    , (0x00004e37, "DhcpInvalidRange")
    , (0x00004e51, "DhcpIprangeConvIllegal")
    , (0x00004e90, "ScopeRangePolicyRangeConflict")
    , (0x00004ea1, "DhcpFoIprangeTypeConvIllegal")
    ]
