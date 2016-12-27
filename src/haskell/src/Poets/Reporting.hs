--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting
-- Copyright   : 3gERP, 2009
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- Reporting engine of POETS
--
--------------------------------------------------------------------------------
module Poets.Reporting
    (
     ReportEngine,
     getEventLog,
     addReport,
     modifyReport,
     removeReport,
     getReportModule,
     getReportModules,
     queryReport,
     createReportEngine,
     stopReportEngine,
     loadPrelude,
     QueryException(..),
     module Poets.Reporting.Report
     ) where
import Poets.Reporting.Report
import Poets.Reporting.Language.Parrot.ReportLibrary
import Poets.Reporting.Loader
import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot
import Poets.EventLog
import Poets.EntityStore
import Poets.Data
import Poets.Conc

import Poets.Logging
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad
import Control.Monad.Error.Class
import Data.Maybe

{-|
  This is the type of the in memory report repository.
-}
type Reports = Map ReportName ReportModule


{-|
  This type represents a handle to a reporting engine.
-}

data ReportEngine = ReportEngine
    { repEventLog :: EventLog,
      repPrelude :: Maybe ReportLibrary,
      repReports :: IORef Reports,
      repCounter :: IORef Int,
      repLock :: Lock
    }

getEventLog :: ReportEngine -> EventLog
getEventLog = repEventLog

{-|
  This type represents exceptions that may occur when querying a report.
-}
data QueryException = ReportNotFound String
                    | IllTypedArgument String
                    | ReportFunctionError ReportError

instance Error QueryException where
    strMsg = ReportFunctionError . strMsg

instance Show QueryException where
    show (ReportNotFound msg) = msg
    show (IllTypedArgument msg) = msg
    show (ReportFunctionError err) = show err


liftIllTypedArgument :: Either String a -> Either QueryException a
liftIllTypedArgument (Right x) = Right x
liftIllTypedArgument (Left msg) = Left (IllTypedArgument msg)

liftReportFunctionError :: Either ReportError a -> Either QueryException a
liftReportFunctionError (Right x) = Right x
liftReportFunctionError (Left msg) = Left (ReportFunctionError msg)




{-|
  This function adds the given report to the reporting engine.
-}
addReport :: ReportEngine -- ^reporting engine
          -> ReportSpec
          -> IO (Either ReportInitException Bool)
addReport re spec = insertReport re spec False

{-|
  This function modifies the given report in the reporting engine.
-}
modifyReport :: ReportEngine -- ^reporting engine
             -> ReportSpec
             -> IO (Either ReportInitException Bool)
modifyReport re spec = insertReport re spec True


{-|
  This function inserts the given report to the reporting engine.
-}
insertReport :: ReportEngine -- ^reporting engine
             -> ReportSpec
             -> Bool         -- ^modify existing report?
             -> IO (Either ReportInitException Bool)
insertReport ReportEngine{ repReports = tReps,
                           repCounter = counterR,
                           repPrelude = lib,
                           repLock = lock,
                           repEventLog = log}
             spec
             modify = atomic lock $ withRecordEnv (getDataEngine log) $ \recEnv -> do
  case parseProgram "" spec of
    Left err -> return $ Left err
    Right Program {programName = Nothing } -> 
        fail $ "no name was attached to the given report"
    Right prog@Program {programName = Just name } -> do
        reps <- readIORef tReps
        mid <- 
            if modify 
              then case Map.lookup name reps of
                     Nothing -> return Nothing
                     Just ReportModule { reportId = id} ->
                         return $ Just id
              else if Map.member name reps
                   then return Nothing
                   else do id <- atomicModifyIORef counterR (\c -> (c+1,c))
                           return $ Just id
        case mid of
          Nothing -> return $ Right False
          Just id -> let modName = hsReportModuleName id
                     in case compileReportToHaskell recEnv lib modName prog of
                          Left err -> return $ Left err
                          Right CompResult {
                                      resultModule = hsMod,
                                      resultType = repType,
                                      resultRecords = recs,
                                      resultDesc = mDesc,
                                      resultTags = mTags} ->
                            do repFunE <- loadHaskellReport recEnv (liftM libDir lib) repType hsMod
                               case repFunE of
                                 Left err -> return $ Left err
                                 Right repFun ->
                                   do let report = ReportModule {
                                            reportSubType = subTypePred recEnv recs,
                                            reportFunction = repFun,
                                            reportType = repType,
                                            reportName = name,
                                            reportDesc = fromMaybe "" mDesc,
                                            reportTags = Set.fromList mTags,
                                            reportSpec = spec,
                                            reportId = id}
                                      writeIORef tReps $ Map.insert name report reps
                                      -- Write a create/update report event to
                                      -- the event log
                                      res <- (if modify then logUpdateReportEvent else logCreateReportEvent) log name (fromMaybe "" mDesc) (iVList $ map iVString mTags) spec
                                      either (return . Left . ReportLogError)
                                             (const $ return $ Right True)
                                             res
{-|
  This function removes the given report from the reporting engine.
-}
removeReport :: ReportEngine -- ^reporting engine
             -> ReportName   -- ^name of the report to remove
             -> IO Bool
removeReport ReportEngine{ repReports = tReps, repEventLog = log } name = do
  let removeRep cur
          = let mreport = Map.lookup name cur
            in case mreport of 
              Just _ -> (Map.delete name cur,mreport)
              Nothing -> (cur,mreport)
  mreport <- atomicModifyIORef tReps removeRep
  case mreport of
    Just report -> do
                 unloadReport report
                 -- Write a delete report event to the event log
                 res <- logDeleteReportEvent log name
                 either (const $ return False) (const $ return True) res
    Nothing -> return False
       
-- | This function provides a mapping containing a snapshot of the
-- currently registered report modules.
getReportModules :: ReportEngine -> IO (Map ReportName ReportModule)
getReportModules ReportEngine{repReports = reps} = readIORef reps

{-|
  This function provides the report module for the given report name. If no
  report is registered under this name 'Nothing' is returned.
-}
getReportModule :: ReportEngine -> ReportName -> IO (Maybe ReportModule)
getReportModule ReportEngine{repReports = reps} name = do
  repsVal <- readIORef reps
  return $ Map.lookup name repsVal

{-|
  This function queries the given report by supplying the given list of additional events
  as well as the given list of arguments.
-}
queryReport :: ReportEngine  -- ^the reporting engine
            -> ReportName    -- ^the name of the report that should be queried
            -> [Event]   -- ^list of additional events to supply to the reporting function
            -> [Value]   -- ^list of arguments for the report
            -> IO (Either QueryException Value)
queryReport ReportEngine{ repReports = reps, repEventLog = log} name events args = withRecordEnv (getDataEngine log) $ \recEnv -> do
  debugReporting "queryReport: prepare context"
  loggedEvents <- getEvents log
  repsCont <- readIORef reps
  entityEnv <- getEntityTypingEnv log
  estore <- getEntityStore log
  let entityTable id dt = either (Left . show) Right
                                 (latestEntityBefore estore id dt)
  debugReporting "queryReport: lookup report module"
  case Map.lookup name repsCont of
    Nothing -> return $ Left $ ReportNotFound ("Report "++name ++" is not available!")
    Just ReportModule{
               reportFunction = repFun,
               reportSubType = isSub,
               reportType = repType} -> debugReporting "queryReport: run query" >> return runQuery
      where runQuery = do
              liftIllTypedArgument (checkArgTypes recEnv entityEnv args (reportArgTypes repType))
              liftReportFunctionError (runRM (repFun args) (events ++ loggedEvents) entityTable isSub)

{-| This function checks whether each element of the given list of
values has the right type according to the given list of expected
types. If not, including the case that the two lists have different
lengths, an error message is issued through the monad. -}

checkArgTypes :: POETSRecordEnv -> EntityTypingEnv -> [Value] -> [Type] -> Either String ()
checkArgTypes recEnv entityEnv vals types = do
  let valsNum = length vals 
      typesNum = length types
  when (typesNum /= valsNum) $
       Left $ "expected " ++ show typesNum ++ " arguments but only "
              ++ show valsNum ++ " were provided"
  mapM_ check $ zip vals types
  where check (val,ty) = typeCheckValue recEnv entityEnv ty val

stopReportEngine :: ReportEngine -> IO ()
stopReportEngine _ = return ()

loadPrelude :: POETSRecordEnv -> (String,FilePath) -> IO (Maybe ReportLibrary)
loadPrelude env (prelude,path) = do
  res <- loadParrotLibrary env path prelude
  case res of
    Left err -> do putStrLn "Cannot load reporting prelude"
                   print err
                   return Nothing
    Right lib -> return $ Just lib

{-|
  This function spawns a reporting engine with the given event log and type environment.
-}
createReportEngine :: (String,FilePath) -> EventLog -> IO ReportEngine
createReportEngine prelude log = withRecordEnv (getDataEngine log) $ \recEnv -> do
  infoReporting "Starting reporting engine"
  preludeLib <- loadPrelude recEnv prelude
  repsR <- newIORef Map.empty
  counterR <- newIORef 0
  lock <- newLock
  return ReportEngine {
               repPrelude = preludeLib,
               repCounter = counterR,
               repEventLog = log,
               repReports = repsR,
               repLock = lock}