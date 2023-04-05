{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module System.Win32.Error.TH
  ( genErrCode
  , gentoDWORD
  , genfromDWORD
  ) where

import Language.Haskell.TH
import System.Win32 (DWORD)

import System.Win32.Error.Mapping

errCode :: Name
errCode = mkName "ErrCode"

errOther :: Name
errOther = mkName "Other"

-- |Given something like [(undefined, "Success")], the following will be produced:
--     data ErrCode
--         = Success
--         | Other !DWORD
--         deriving (Eq, Show)
genErrCode :: Q [Dec]

#if MIN_VERSION_template_haskell(2,12,0)
genErrCode = return [DataD [] errCode []  Nothing cons [(DerivClause Nothing $ map ConT [''Eq, ''Show])]]
#elif MIN_VERSION_template_haskell(2,11,0)
genErrCode = return [DataD [] errCode []  Nothing cons (map ConT [''Eq, ''Show])]
#else
genErrCode = return [DataD [] errCode [] cons [''Eq, ''Show]]
#endif
  where
    con name = NormalC name []
#if __GLASGOW_HASKELL__ < 800
    cons = map (con . snd) mapping ++ [NormalC errOther [(IsStrict, ConT ''DWORD)]]
#else
    cons = map (con . snd) mapping ++ [NormalC errOther [(Bang NoSourceUnpackedness SourceStrict, ConT ''DWORD)]]
#endif

-- toDWORD :: ErrCode -> DWORD
-- toDWORD (ErrorOther x) = x
-- toDWORD errorSomethingElse = #
-- toDWORD errorSomethingElse = #
-- toDWORD errorSomethingElse = #
gentoDWORD :: Q [Dec]
gentoDWORD  = do
    x <- newName "x"
    return [ SigD toDWORD (AppT (AppT ArrowT (ConT errCode)) (ConT ''DWORD))
           , FunD toDWORD $ Clause [ConP errOther [] [VarP x]] (NormalB (VarE x)) [] : map genClause mapping
           ]
  where
    toDWORD = mkName "toDWORD"
    genClause :: (DWORD, Name) -> Clause
    genClause (dw, err) = Clause [ConP err [] []] (NormalB (LitE . litDWORD $ dw)) []

-- fromDWORD :: DWORD -> ErrCode
-- fromDWORD 0 = ErrorSuccess
-- fromDWORD # = ErrorSomethingElse
-- fromDWORD # = ErrorSomethingElse
-- fromDWORD # = ErrorSomethingElse
-- fromDWORD x = ErrorOther x
genfromDWORD :: Q [Dec]
genfromDWORD = do
    x <- newName "x"
    return [ SigD fromDWORD (AppT (AppT ArrowT (ConT ''DWORD)) (ConT errCode))
           , FunD fromDWORD $ map genClause mapping ++ [Clause [VarP x] (NormalB (AppE (ConE errOther) (VarE x))) []]
           ]
  where
    fromDWORD = mkName "fromDWORD"
    genClause :: (DWORD, Name) -> Clause
    genClause (dw, err) = Clause [LitP $ litDWORD dw] (NormalB (ConE  err)) []

litDWORD :: DWORD -> Lit
litDWORD = IntegerL . toInteger
