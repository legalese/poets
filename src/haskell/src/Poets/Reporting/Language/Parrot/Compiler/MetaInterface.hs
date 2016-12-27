{-# LANGUAGE TemplateHaskell, CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Compiler.MetaInterface
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides functionalitye to access
-- Poets.Reporting.Language.Parrot.Compiler.Interface
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Compiler.MetaInterface
    where

import Language.Haskell.TH as TH hiding (Match)
import Poets.Reporting.Language.Parrot.ReportLibrary


import HsSyn
import GHC
import RdrName
import OccName
import FastString

type HsName = RdrName


{-| Target Haskell expresstion type.  -}

type HsExp = LHsExpr HsName
type HsExp' = HsExpr HsName


{-| This function creates a proper Haskell expression by not attaching
any source location information -}

mkHsExp :: HsExp' -> HsExp
mkHsExp = noLoc

{-| This is the name of the module defining the interface to POETS. -}

interfaceModule :: ModuleName
interfaceModule = mkModuleName "Poets.Reporting.Language.Parrot.Compiler.Interface"


preludeLibraryImport :: ReportLibrary -> LImportDecl HsName
preludeLibraryImport ReportLibrary {libModuleName = modName}
    = noLoc ImportDecl {
#if __GLASGOW_HASKELL__ >= 702
                          ideclSafe = False,
#endif
#if __GLASGOW_HASKELL__ >= 704
                          ideclImplicit = False,
#endif
        ideclName = noLoc $ mkModuleName modName,
        ideclPkgQual = Nothing,
        ideclSource = False,
        ideclQualified = False,
        ideclAs = Nothing,
        ideclHiding = Nothing }

preludeModuleImport :: LImportDecl HsName
preludeModuleImport = noLoc ImportDecl {
#if __GLASGOW_HASKELL__ >= 702
                          ideclSafe = False,
#endif
#if __GLASGOW_HASKELL__ >= 704
                          ideclImplicit = False,
#endif
                          ideclName = noLoc $ mkModuleName "Prelude",
                          ideclPkgQual = Nothing,
                          ideclSource = False,
                          ideclQualified = True,
                          ideclAs = Nothing,
                          ideclHiding = Just (False, []) }

interfaceModuleImport :: LImportDecl HsName
interfaceModuleImport = noLoc ImportDecl {
#if __GLASGOW_HASKELL__ >= 702
                          ideclSafe = False,
#endif
#if __GLASGOW_HASKELL__ >= 704
                          ideclImplicit = False,
#endif
                          ideclName = noLoc interfaceModule,
                          ideclPkgQual = Nothing,
                          ideclSource = False,
                          ideclQualified = True,
                          ideclAs = Nothing,
                          ideclHiding = Nothing }

idInModule :: ModuleName -> String -> HsExp
idInModule moduleName name = mkHsExp $ HsVar $ mkRdrQual moduleName (mkVarOccFS $ mkFastString name)

{-| This function takes an identifier which is supposed to be defined
in the interface module referred to by 'interfaceModule' and
constructs a corresponding Haskell expression. -}

interfaceId :: String -> HsExp
interfaceId = idInModule interfaceModule

declRepFun :: String -> Q Dec
declRepFun name = do
  rhs <- [|interfaceId ('r' : name)|]
  return $ FunD (mkName $ "hs" ++ name) [Clause [] (NormalB rhs)  []]

declRepFuns :: [String] -> Q [Dec]
declRepFuns = mapM declRepFun