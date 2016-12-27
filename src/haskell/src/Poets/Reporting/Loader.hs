{-# LANGUAGE ScopedTypeVariables, CPP #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Loader
-- Copyright   : 3gERP, 2009
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module allows to compile and dynamically load reports to the running
-- reporting engine.
--
--------------------------------------------------------------------------------
module Poets.Reporting.Loader
    ( unloadReport,
      loadHaskellReport,
      loadHaskellLibrary,
      hsReportModuleName,
      loadParrotReport,
      loadParrotLibrary,
      ReportInitException (..),
      module Poets.Reporting.Report
     ) where


import Poets.Logging
import Poets.Reporting.Report
import Poets.Reporting.Language.Parrot.ReportLibrary
import Poets.Reporting.Language.Parrot
import Poets.Reporting.Language.Parrot.Compiler.BasicValue
import Poets.Reporting.Language.Parrot.Typing.PolyType
import Poets.Reporting.ReportInitException
import Poets.Data.Type

import qualified Data.Map as Map
import qualified Data.Set as Set

#if __GLASGOW_HASKELL__ >= 702
import GhcMonad
#endif

#if __GLASGOW_HASKELL__ < 704
import HscTypes
import DynFlags
#endif

import GHC
import Exception
import Panic
import GHC.Paths
import Outputable

import Unsafe.Coerce
import System.FilePath
import System.Exit
import System.Directory
import Data.Maybe
import Control.Exception as C (catch)

{-|
  this function generates the haskell module name for the given report id.
-}
hsReportModuleName :: ReportId -> String
hsReportModuleName counter = "Report_" ++ show counter

disabledReportSpec :: String -> String
disabledReportSpec moduleName = "module  " ++ moduleName
                          ++ " where\n"

{-| This function loads a Parrot report module by (after type
checking) transforming it to Haskell and then compiling it. -}

loadParrotReport :: ReportId -> POETSRecordEnv -> Maybe ReportLibrary
                 -> ReportSpec -> String
                 -> IO (Either ReportInitException ReportModule)
loadParrotReport id recEnv lib spec name = 
  let modName = hsReportModuleName id in
  case reportSrcToHaskell recEnv lib modName spec  of
    Left err -> return $ Left err
    Right CompResult { resultModule = hsMod, 
                       resultType = repType,
                       resultDesc = mDesc,
                       resultTags = mTags,
                       resultRecords = recs} -> do
          repFunE <- loadHaskellReport recEnv (liftM libDir lib) repType hsMod
          case repFunE of
            Left err -> return $ Left err
            Right repFun -> return $ Right $ ReportModule {
                reportFunction = repFun,
                reportSubType = subTypePred recEnv recs,
                reportType = repType,
                reportName = name,
                reportDesc = fromMaybe "" mDesc,
                reportTags = Set.fromList mTags,
                reportSpec = spec,
                reportId = id}

{-| This function loads a Parrot library by (after type checking)
transforming it to a Haskell module. -}

loadParrotLibrary :: POETSRecordEnv -> FilePath -> String
                 -> IO (Either ReportInitException ReportLibrary)
loadParrotLibrary recEnv path code = do
  let modName = "ParrotPrelude"
  res <- libraryFileToHaskell recEnv modName path code
  case res of
    Left err -> return $ Left err
    Right LibCompResult { libResultModule = hsMod, 
                          libResultType = funs,
                          libResultRecords = recs} -> do
          modNameStrE <- loadHaskellLibrary hsMod
          case modNameStrE of
            Left err -> return $ Left err
            Right dir -> return $ Right $ ReportLibrary {
                                           libRecords = recs,
                                           libFunctions = Map.keysSet funs,
                                           libTypings = Map.fromList $ 
                                                          map fromToplevelTyping $ Map.toList funs,
                                           libModuleName = modName,
                                           libDir = dir}


{-| This function loads a Haskell module that is supposed to define a
reporting function.  -}

loadHaskellReport ::  POETSRecordEnv -> Maybe FilePath -> ReportType
                  -> HsModule RdrName -> IO (Either ReportInitException ReportFunction)
loadHaskellReport recEnv mImportPath repType hsMod = do
  reportRepository <- getTemporaryDirectory
  let modName = unLoc $ fromJust $ hsmodName hsMod
      modNameStr = moduleNameString modName
      srcFile = reportRepository </> (modNameStr ++ ".hs")
      hiFile = reportRepository </> (modNameStr ++ ".hi")
      oFile = reportRepository </> (modNameStr ++ ".o")
      handleCleanup inner = inner `gfinally` removeFiles
      removeFiles = liftIO $ mapM_
                    (\file -> C.catch (removeFile file) (const (return ()) :: IOError -> IO ()))
                    [srcFile, hiFile, oFile]
  createDirectoryIfMissing True reportRepository
  runGhc (Just libdir) $ handleErrors $ handleCleanup $  do
        dflags <- getSessionDynFlags
        let fullSpec = showPpr' dflags hsMod
        liftIO $ writeFile srcFile fullSpec
        setSessionDynFlags dflags  {optLevel=1,
                                    ghcLink = LinkInMemory,
                                    importPaths = addImportPath $ importPaths dflags}
        target <- guessTarget srcFile Nothing
        addTarget target
        r <- load LoadAllTargets
        case r of
          Failed -> return $ fail "Compilation failed"
          Succeeded ->
              do m <- findModule modName Nothing
                 setContext' [m]
                 rFunc <- compileExpr (modNameStr ++ "." ++ repInterfaceFunName)
                 return $ Right $ toBasicReportFunction recEnv repType $ unsafeCoerce rFunc
      where addImportPath paths = case mImportPath of
                                    Nothing -> paths
                                    Just path -> path : paths

#if __GLASGOW_HASKELL__ >= 704

-- This function generates the import declaration for dynamically
-- loading a Haskell module

mkImportDecl :: ModuleName -> InteractiveImport
mkImportDecl modName = IIDecl (simpleImportDecl modName)
#endif

#if __GLASGOW_HASKELL__ < 700
setContext' mods = setContext [] mods
#endif

#if __GLASGOW_HASKELL__ < 702 && __GLASGOW_HASKELL__ >= 700
setContext' mods = setContext [] (map (\m -> (m,Nothing)) mods)
#endif

#if __GLASGOW_HASKELL__ < 704 && __GLASGOW_HASKELL__ >= 702
setContext' mods = setContext mods []
#endif

#if __GLASGOW_HASKELL__ < 706 && __GLASGOW_HASKELL__ >= 704
setContext' :: GhcMonad m => [Module] -> m () -- see mkImportDecl type above
setContext' mods = setContext (map (mkImportDecl . moduleName) mods)
#endif

#if __GLASGOW_HASKELL__ >= 706
setContext' mods = setContext (map (mkImportDecl . moduleName) mods)
#endif

#if __GLASGOW_HASKELL__ < 706
showPpr' _  = showPpr
#endif
#if __GLASGOW_HASKELL__ >= 706
showPpr' dflags = showPpr dflags
#endif


{-| This function loads a library by writing it to a temporary
directory. The path to this directory is returned on success. -}

loadHaskellLibrary ::  HsModule RdrName -> IO (Either ReportInitException FilePath)
loadHaskellLibrary hsMod = do
  tmp <- getTemporaryDirectory
  let modNameStr = moduleNameString $ unLoc $ fromJust $ hsmodName hsMod
      srcFile = tmp </> (modNameStr ++ ".hs")
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let fullSpec = showPpr' dflags hsMod
    liftIO $ writeFile srcFile fullSpec
  return $ Right tmp


{-| This function handles errors that occur in a GHC monad.  -}

handleErrors :: (GhcMonad m)
             => m (Either ReportInitException a) -> m (Either ReportInitException a)
handleErrors inner = 
  handleSourceError (return . Left . ReportHaskellException) $
         ghandle (\exception ->
                  (liftIO $ debugReporting $ show exception) >>
                  case fromException exception of
                    Just (ioe :: IOException) -> return $ fail (show ioe)
                    _ -> case fromException exception of
                           Just StackOverflow ->
                               return $ fail "stack overflow: use +RTS -K<size> to increase it"
                           _ -> case fromException exception of
                                  Just srcErr -> return $ Left $ ReportHaskellException srcErr
                                  _ -> case fromException exception of
                                         Just (ex :: ExitCode) -> throw ex
                                         _ -> return $ fail (show exception) ) $
                     handleGhcException (return . Left . ReportGhcException) inner


{-|
  This function unloads the given report module from the runtime system.
-}

unloadReport :: ReportModule -> IO Bool
unloadReport ReportModule{ reportId = id} = do
  reportRepository <- getTemporaryDirectory
  let moduleName = hsReportModuleName id
      fullSpec = disabledReportSpec moduleName
      srcFile = reportRepository </> (moduleName ++ ".hs")
      hiFile = reportRepository </> (moduleName ++ ".hi")
      oFile = reportRepository </> (moduleName ++ ".o")
      handleCleanup inner = inner `gfinally` removeFiles
      removeFiles = liftIO $ mapM_
                    (\file -> C.catch (removeFile file) (const (return ()) :: IOError -> IO ()))
                    [srcFile, hiFile, oFile]
  createDirectoryIfMissing True reportRepository
  writeFile srcFile fullSpec
  runGhc (Just libdir) $ handleCleanup $  do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget srcFile Nothing
      addTarget target
      r <- load LoadAllTargets
      case r of
        Failed -> return False
        Succeeded -> return True
