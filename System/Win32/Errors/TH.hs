{-# LANGUAGE TemplateHaskell #-}

module System.Win32.Errors.TH
  ( genConvert
  ) where

import Language.Haskell.TH
import System.Win32 (DWORD)

import System.Win32.Errors.Types

-- | Generate the following functions:
--     toDWORD :: ErrCode -> DWORD
--     fromDWORD :: DWORD -> ErrCode
genConvert :: [(DWORD, ErrCode)] -> Q [Dec]
genConvert xs = do
    from <- genfromDWORD xs
    to <- gentoDWORD xs
    return $ from ++ to

gentoDWORD :: [(DWORD, ErrCode)] -> Q [Dec]
gentoDWORD  xs = do
    x <- newName "x"
    return [ SigD toDWORD (AppT (AppT ArrowT (ConT ''ErrCode)) (ConT ''DWORD))
           , FunD toDWORD $ Clause [ConP 'ErrorOther [VarP x]] (NormalB (VarE x)) [] : map genClause xs
           ]
  where
    toDWORD = mkName "toDWORD"
    genClause :: (DWORD, ErrCode) -> Clause
    genClause (dw, err) = Clause [ConP (nameErrCode err) []] (NormalB (LitE . litDWORD $ dw)) []

genfromDWORD :: [(DWORD, ErrCode)] -> Q [Dec]
genfromDWORD  xs = do
    x <- newName "x"
    return [ SigD fromDWORD (AppT (AppT ArrowT (ConT ''DWORD)) (ConT ''ErrCode))
           , FunD fromDWORD $ map genClause xs ++ [Clause [VarP x] (NormalB (AppE (ConE 'ErrorOther) (VarE x))) []]
           ]
  where
    fromDWORD = mkName "fromDWORD"
    genClause :: (DWORD, ErrCode) -> Clause
    genClause (dw, err) = Clause [LitP $ litDWORD dw] (NormalB (ConE . nameErrCode $ err)) []

litDWORD :: DWORD -> Lit
litDWORD = IntegerL . toInteger

nameErrCode :: ErrCode -> Name
nameErrCode = mkName . show
