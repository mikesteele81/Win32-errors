{-# LANGUAGE OverloadedStrings #-}

module ErrorSpec where

import Test.Hspec
import Test.QuickCheck hiding (Success) -- Success is a constructor for ErrCode
import System.Win32.Error (toDWORD, fromDWORD, ErrCode(..))

-- I have made changes to TH.hs so I have made some simple tests for
-- the parts generated by TH.hs: ErrCode, toDWORD and fromDWORD.

-- errCode :: Win32Exception -> ErrCode
-- toDWORD :: ErrCode -> DWORD
-- fromDWORD :: DWORD -> ErrCode

spec :: Spec
spec = do
  describe "TH.toDWORD" $ do
    it "it converts an ErrorCode Success to DWORD 0" $ do
      d <- return $ toDWORD Success
      d `shouldBe` 0
  describe "TH.toDWORD" $ do
    it "it converts an ErrorCode AccessDenied to DWORD 5" $ do
      d <- return $ toDWORD AccessDenied
      d `shouldBe` 0x00000005
  describe "TH.fromDWORD" $ do
    it "it converts a DWORD 0 to a ErrorCode Success" $ do
      e <- return $ fromDWORD 0
      e `shouldBe` Success
  describe "TH.fromDWORD" $ do
    it "it converts a DWORD 99999 to a ErrorCode Other" $ do
      e <- return $ fromDWORD 99999
      e `shouldBe` Other 99999
  describe "toDWORD and fromDWORD" $ do
    it "are inverses i.e. toDWORD (fromDWORD d) == d" $ do
       property $ \d -> toDWORD (fromDWORD d) == d