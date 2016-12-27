--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides functionality to typecheck and compile Parrot
-- programs.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot
    ( typeReportSrc,
      typeReportFile,
      parseProgramFile,
      parseProgram,
      toHsModule,
      repInterfaceFunName,
      reportSrcToHaskell,
      compileReportToHaskell,
      libraryFileToHaskell,
      typeLibraryFile,
      subTypePred,
      CompResult(..),
      LibCompResult(..)
    ) where

import Poets.Reporting.Language.Parrot.Compiler.Haskell
import Poets.Reporting.Language.Parrot.Typing
import Poets.Reporting.Language.Parrot.Parser hiding (parseProgram, parseProgramFile)
import qualified Poets.Reporting.Language.Parrot.Parser as P
import Poets.Reporting.Language.Parrot.Desugar
import Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
import Poets.Reporting.Language.Parrot.ReportLibrary
import Poets.Reporting.ReportInitException
import Poets.Reporting.Report
import Poets.Data.Type.Utils

import Data.Map (Map)
import qualified Data.Map as Map

{-| This is the result type of type checking and then compiling a
Parrot module into a Haskell module.  -}

data CompResult = CompResult { 
      resultName :: Maybe ReportName,
      resultDesc :: Maybe String,
      resultTags :: [String],
      resultModule :: HsModule HsName,
      resultRecords :: [ExtRecord],
      resultType :: ReportType}

{-| This is the result type of type checking and then compiling a
Parrot library module into a Haskell module.  -}

data LibCompResult = LibCompResult { 
      libResultModule :: HsModule HsName,
      libResultRecords :: [ExtRecord],
      libResultType :: Map FunId TypeScheme}

type TypeResult = (ProgramSugar, [ExtRecord], ReportType)

parseProgram :: String
             -> String
             -> Either ReportInitException ProgramSugar
parseProgram desc src = liftReportException (P.parseProgram desc src)

parseProgramFile path = liftM liftReportException (P.parseProgramFile path)

typeReportFile :: POETSRecordEnv  -- ^the record environment used for type checking
               -> Maybe ReportLibrary
               -> FilePath  -- ^the source of the Parrot module
               -> IO (Either ReportInitException TypeResult)
typeReportFile recEnv lib path = do 
  src <- readFile path
  return $ typeReportSrc' recEnv lib path src

typeReportSrc :: POETSRecordEnv  -- ^the record environment used for type checking
              -> Maybe ReportLibrary
              -> String  -- ^the source of the Parrot module
              -> Either ReportInitException TypeResult
typeReportSrc recEnv lib src = typeReportSrc' recEnv lib "" src

typeReportSrc' :: POETSRecordEnv  -- ^the record environment used for type checking
               -> Maybe ReportLibrary
               -> String  -- ^a description of the source (e.g. file path)
               -> String  -- ^the source of the Parrot module
               -> Either ReportInitException TypeResult
typeReportSrc' recEnv lib desc src = do
  prog <- parseProgram desc src
  (recs,repType) <- typeReport recEnv lib prog
  return (prog,recs,repType)

typeReport :: POETSRecordEnv
           -> Maybe ReportLibrary
           -> ProgramSugar
           -> Either ReportInitException ([ExtRecord],ReportType)
typeReport recEnv lib prog = do
  (recs,funs) <- liftReportException $ typeDecls recEnv lib (programDecls prog)
  case Map.lookup repFunName funs of
    Nothing -> fail $ "a report specification has to contain a main report function of name '"
               ++ repFunName ++ "'"
    Just typ -> do
      repType <- liftReportException $ toReportType recEnv typ
      return (recs,repType)

{-| This function parses and type checks the given Parrot module and
transforms it into a Haskell module-}

reportSrcToHaskell :: POETSRecordEnv  -- ^the record environment used for type checking
                   -> Maybe ReportLibrary
                   -> String  -- ^the name the resulting Haskell module should have
                   -> String  -- ^the source of the Parrot module
                   -> Either ReportInitException CompResult
reportSrcToHaskell recEnv lib modName src = parseProgram "" src
                                            >>= compileReportToHaskell recEnv lib modName

{-| This function type checks and then compiles the given Parrot module
into a Haskell module. -}

compileReportToHaskell :: POETSRecordEnv  -- ^the record environment used for type checking
                       -> Maybe ReportLibrary
                       -> String  -- ^the name the resulting Haskell module should have
                       -> ProgramSugar -- ^the AST of the Parrot module
                       -> Either ReportInitException CompResult
compileReportToHaskell recEnv lib modName progSugar = do
  (recs,repType) <- typeReport recEnv lib progSugar
  let progCore = desugarProgram progSugar
  mod <- liftReportException $ toHsModule lib True (programDecls progCore) modName
  return CompResult {
               resultName = programName progCore,
               resultDesc = programDesc progCore,
               resultTags = programTags progCore,
               resultModule = mod,
               resultRecords = recs,
               resultType =repType }

-- libraries --

{-| This function parses and type checks the given Parrot library file
and transforms it into a Haskell module-}

libraryFileToHaskell :: POETSRecordEnv  -- ^the record environment used for type checking
                    -> String    -- ^the name the resulting Haskell module should have
                    -> FilePath  -- ^the path to the Parrot library
                    -> String -- ^the Parrot code
                    -> IO (Either ReportInitException LibCompResult)
libraryFileToHaskell recEnv modName path code = do 
  let ast = liftReportException (parseLibrary path code)
  return $ ast >>= compileLibraryToHaskell recEnv modName


{-| This function type checks and then compiles the given Parrot
library into a Haskell module. -}

compileLibraryToHaskell :: POETSRecordEnv  -- ^the record environment used for type checking
                        -> String  -- ^the name the resulting Haskell module should have
                        -> LibrarySugar -- ^the AST of the Parrot module
                        -> Either ReportInitException LibCompResult
compileLibraryToHaskell recEnv modName libSugar = do
  (recs,funs) <- typeLibrary recEnv libSugar
  mod <- liftReportException $ toHsModule Nothing False (libraryDecls $ desugarLibrary libSugar) modName
  return LibCompResult {
               libResultModule = mod,
               libResultRecords = recs,
               libResultType = funs }

{-|
  This function parses and type checks the given Parrot library file.
-}
typeLibraryFile :: POETSRecordEnv  -- ^the record environment used for type checking
                -> FilePath  -- ^the path to the Parrot library
                -> IO (Either ReportInitException ([ExtRecord], Map FunId TypeScheme))
typeLibraryFile recEnv path = do 
  ast <- liftM liftReportException (parseLibraryFile path)
  return $ ast >>= typeLibrary recEnv 

{-|
  This function type checks the given Parrot library AST.
-}

typeLibrary :: POETSRecordEnv
            -> LibrarySugar
            -> Either ReportInitException ([ExtRecord], Map FunId TypeScheme)
typeLibrary recEnv (Library decls) = do
  (recs,funs) <- liftReportException $ typeDecls recEnv Nothing decls
  return (recs, funs)

-- misc --



subTypePred :: POETSRecordEnv -> [Record PType] -> (RecordName -> RecordName -> Bool)
subTypePred recEnv recs = subType
    where recEnv' = fmap (ann Nothing . deepInject3) recEnv
          combEnv = addRecordInfos recEnv' recs
          subType r1 r2 = either (const False) id $ isSubType combEnv r1 r2