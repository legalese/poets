{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Morten Ib Nielsen, Tom Hvitved, Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements the POETS Thrift server.
--
--------------------------------------------------------------------------------
module Main where

import Prelude hiding (mapM)
import Control.Exception
import PServer.Thrift
import PoetsServer_Iface
import PoetsServer
import Reporting_Types
import Poets.Config
import Poets.Logging
import Poets.EventLog (EventLog)
import qualified Poets.EventLog as EventLog
import Thrift.Server
import Control.Monad.Error (catchError)
import Control.Monad.State hiding (mapM)
import Control.Concurrent
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import Data.List
import qualified Data.Map as Map
import Data.Traversable (mapM)
import System.Posix.Signals
import Language.Haskell.TH

import Data.Text.Lazy hiding (map, null, intercalate, filter)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Data.Set (Set)
import qualified Data.Set as Set

-- Data module
import Poets.Data as Data hiding (undefinedRecord)
import Poets.Data.Value.Render (printVal)

-- Contracts
import qualified Poets.Contracts as Contracts

-- Reporting
import qualified Poets.Reporting as Reporting

-- Rule engine
import qualified Poets.Rules as Rules

-- Thrift-server modification related
import Thrift.Transport.Handle()
import Thrift.Transport.Framed
import Thrift.Protocol.Binary
import Network

toStringList :: Vector Text -> [String]
toStringList l = map unpack $ Vector.toList l

toStringSet :: HashSet Text -> Set String
toStringSet l = Set.fromList $ map unpack $ HashSet.toList l

--------------------------------------------------------------------------------
-- The POETS thrift server
--------------------------------------------------------------------------------

--DEFINITION OF THE SERVER CONTEXT
data PoetsSession = PoetsSession {
      dataEngine :: Data.DataEngine,
      contractEngine :: Contracts.ContractEngine,
      eventLog :: EventLog,
      reportEngine :: Reporting.ReportEngine,
      ruleEngine :: Rules.RuleEngine
    }

stopPoetsSession :: PoetsSession -> IO ()
stopPoetsSession PoetsSession { 
      eventLog = eventLog,
      contractEngine = contractEngine,
      reportEngine = reportEngine,
      ruleEngine = ruleEngine } = do
  EventLog.stopLog eventLog
  Contracts.stopContractEngine contractEngine
  Reporting.stopReportEngine reportEngine
  Rules.stopRuleEngine ruleEngine

--IMPLEMENTATION OF THE SERVER
instance PoetsServer_Iface PoetsSession where
    -- Data Definitions
    addDataDefinitions PoetsSession{eventLog = log} pces' = do
      let pces = toStringList pces'
      debugCall "addDataDefinitions" pces
      res <- EventLog.logAddDataDefsEvent log pces
      either eventLogError return res

    getRecordDefinition PoetsSession{dataEngine = dataEngine} rName' = do
      let rName = unpack rName'
      debugCall "getRecordDefinition" [rName]
      Data.withRecordEnv dataEngine $ \recordEnv ->
           either (const $ undefinedRecord rName)
                  (return . toRecordDefinition)
                  (getFullRecordInfo recordEnv rName)

    getSubTypes PoetsSession{dataEngine = dataEngine} rName' = do
      let rName = unpack rName'
      debugCall "getSubTypes" [rName]
      Data.withRecordEnv dataEngine $ \recordEnv ->
             either (const $ undefinedRecord rName)
                    (return . toHashSet')
                    (Data.getSubTypes recordEnv rName)

    -- Entity Store
    createEntity PoetsSession{eventLog = log} val t' = do
      let t = unpack t'
      v <- valueDecode val
      debugCall "createEntity" [printVal v, t]
      res <- EventLog.logCreateEntityEvent log v t
      either eventLogError (return . fromIntegral) res

    updateEntity PoetsSession{eventLog = log} eId val = do
      v <- valueDecode val
      debugCall "updateEntity" [show eId, printVal v]
      res <- EventLog.logUpdateEntityEvent log (fromIntegral eId) v
      either eventLogError return res

    deleteEntity PoetsSession{eventLog = log} eId = do
      debugCall "deleteEntity" [show eId]
      res <- EventLog.logDeleteEntityEvent log (fromIntegral eId)
      either eventLogError return res

    -- Contract Engine
    createContractTemplate PoetsSession{contractEngine = ce} cDef' = do
      let cDef = unpack cDef'
      debugCall "createContractTemplate" [cDef]
      res <- Contracts.addContractDef ce cDef
      either contractsError return res

    updateContractTemplate PoetsSession{contractEngine = ce} cDef' = do
      let cDef = unpack cDef'
      debugCall "updateContractTemplate" [cDef]
      res <- Contracts.updateContractDef ce cDef
      either contractsError return res

    deleteContractTemplate PoetsSession{contractEngine = ce} name' = do
      let name = unpack name'
      debugCall "deleteContractTemplate" [name]
      res <- Contracts.deleteContractDef ce name
      either contractsError return res

    getContract PoetsSession{contractEngine = ce} cId
                whatIfTr = do
      tr' <- mapM (fromTransaction) whatIfTr
      let tr = Vector.toList tr'
      debugCall "getContract" (show cId : map printTrEvent tr)
      res <- Contracts.getContract ce tr (fromIntegral cId)
      either contractsError (return . toResidual) res

    getExpectedTransactions PoetsSession{contractEngine = ce} cId
                            whatIfTr = do
      tr' <- mapM fromTransaction whatIfTr
      let tr = Vector.toList tr'
      debugCall "getExpectedTransactions" (show cId : map printTrEvent tr)
      res <- Contracts.getExpectedTransactions ce tr (fromIntegral cId)
      either contractsError (return . Vector.fromList . toTransactionPatterns cId) res

    registerTransactions PoetsSession{contractEngine = ce} tr'' = do
      tr' <- mapM fromTransaction tr''
      let tr = Vector.toList tr'
      debugCall "registerTransactions" (map printTrEvent tr)
      res <- Contracts.registerTransactions ce tr
      either contractsError return res

    createContract PoetsSession{contractEngine = ce} metaData = do
      m <- valueDecode metaData
      debugCall "createContract" [printVal m]
      res <- Contracts.addContract ce m
      either contractsError (return . fromIntegral) res

    updateContract PoetsSession{contractEngine = ce} cId
                   metaData = do
      m <- valueDecode metaData
      debugCall "updateContract" [show cId, printVal m]
      res <- Contracts.updateContract ce (fromIntegral cId) m
      either contractsError return res

    deleteContract PoetsSession{contractEngine = ce} cId = do
      debugCall "deleteContract" [show cId]
      res <- Contracts.deleteContract ce (fromIntegral cId)
      either contractsError return res

    isConcludable PoetsSession{contractEngine = ce} cId = do
      debugCall "isConcludable" [show cId]
      res <- Contracts.isConcludable ce (fromIntegral cId)
      either contractsError return res

    concludeContract PoetsSession{contractEngine = ce} cId = do
      debugCall "concludeContract" [show cId]
      res <- Contracts.concludeContract ce (fromIntegral cId)
      either contractsError return res

    --Reporting Engine
    getReport PoetsSession{reportEngine = re} name' = do
      let name = unpack name'
      debugCall "getReport" [name]
      mReport <- Reporting.getReportModule re name
      case mReport of
        Nothing -> reportNotFoundException
        Just report -> return $ toReport report

    createReport PoetsSession{reportEngine = re} spec' = do
      let spec = unpack spec'
      debugCall "createReport" [spec]
      res <- Reporting.addReport re spec
      case res of
        Left exc -> throw $ ReportInitException $ pack $ show exc
        Right b -> return b

    updateReport PoetsSession{reportEngine = re} spec' = do
      let spec = unpack spec'
      debugCall "updateReport" [spec]
      res <- Reporting.modifyReport re spec
      case res of
        Left exc -> throw $ ReportInitException $ pack $ show exc
        Right b -> return b

    deleteReport PoetsSession{reportEngine = re} name' = do
      let name = unpack name'
      debugCall "deleteReport" [name]
      Reporting.removeReport re name

    queryReport PoetsSession{reportEngine = re} name' events args
        = do
      let name = unpack name'
      debugPServer "queryReport decoding event arguments"
      eventValues' <- mapM valueDecode events
      let eventValues = Vector.toList eventValues'
      debugPServer "queryReport decoding arguments"
      argValues' <- mapM valueDecode args
      let argValues = Vector.toList argValues'
      debugCall "queryReport" (name : map printVal (eventValues ++ argValues))
      res <- Reporting.queryReport re name eventValues argValues
      case res of
        Left err -> queryReportError err
        Right val -> return $ valueEncode val


    --Rule Engine
    queryRules PoetsSession{ruleEngine = re} query = do
      debugCall "queryRules" [show query]
      q <- qValueDecode query
      res <- Rules.query re q
      either rulesError (return . Vector.fromList . map (toHashMap' . Map.map qValueEncode)) res


-- |Write the name of the invoked service and the parameters to the debug log.
debugCall :: String -> [String] -> IO ()
debugCall name args =
    debugPServer $ name ++ ": " ++
                   if null args then "<no arguments>" else intercalate ", " args

--MAIN
main :: IO ()
main = run `catchError`
       -- Log potential errors
       (\err -> void (emergencyPServer ("Server shutting down. Reason: " ++ show err)))
    where run = do
            args <- getArgs
            case args of
              [confFile] -> do
                  confFileAbs <- canonicalizePath confFile
                  setCurrentDirectory (takeDirectory confFileAbs)
                  eConf <- parseConfigFile confFileAbs
                  case eConf of
                    Left err ->
                      fail $ "An error occurred while parsing the " ++
                             "configuration file \"" ++ confFile ++"\": " ++ err
                    Right conf -> do
                        setupLogging $ serverConfig conf
                        -- First read system ontology
                        dataEngine <- Data.createDataEngine dataDefs
                        -- Then initialize the various engines
                        eventLog <- EventLog.createLog (eventsConfig conf)
                                                       dataEngine
                        re <- Reporting.createReportEngine repPrelude eventLog
                        ce <- Contracts.createContractEngine conPrelude re
                        rue <- Rules.createRuleEngine (rulesConfig conf)
                                                      eventLog
                        -- Finally bootstrap server state from the event log
                        EventLog.bootstrapLog eventLog (callbacks re ce)
                        let port = serverPort $ serverConfig conf
                        infoPServer $ "Starting Server on port " ++ show port
                        let session = PoetsSession {
                                        dataEngine = dataEngine,
                                        contractEngine = ce,
                                        reportEngine = re,
                                        eventLog = eventLog,
                                        ruleEngine = rue
                                      }
                        serverThread <- myThreadId
                        registerSignalHandler session serverThread
                        forkIO $ poetsPrompt session serverThread
                        runFramedServer session process port
              _ -> putStrLn "Syntax: poetsserver <configfile>"
          -- Callback functions for bootstrapping the system
          callbacks re ce = EventLog.EventLogCallback{
                              EventLog.cbCreateContractDef = cb1 ce,
                              EventLog.cbUpdateContractDef = cb2 ce,
                              EventLog.cbDeleteContractDef = cb3 ce,
                              EventLog.cbCreateContract = cb4 ce,
                              EventLog.cbUpdateContract = cb5 ce,
                              EventLog.cbDeleteContract = cb6 ce,
                              EventLog.cbConcludeContract = cb7 ce,
                              EventLog.cbTransaction = cb8 ce,
                              EventLog.cbCreateReport = cb9 re,
                              EventLog.cbUpdateReport = cb10 re,
                              EventLog.cbDeleteReport = cb11 re
                            }
          cb1 ce code = do
                  res <- Contracts.addContractDef ce code
                  either (fail . show) return res
          cb2 ce code = do
                  res <- Contracts.updateContractDef ce code
                  either (fail . show) return res
          cb3 ce name = do
                  res <- Contracts.deleteContractDef ce name
                  either (fail . show) return res
          cb4 ce m = do
                  res <- Contracts.addContract ce m
                  either (fail . show) return res
          cb5 ce cId m = do
                  res <- Contracts.updateContract ce cId m
                  either (fail . show) return res
          cb6 ce cId = do
                  res <- Contracts.deleteContract ce cId
                  either (fail . show) return res
          cb7 ce cId = do
                  res <- Contracts.concludeContract ce cId
                  either (fail . show) return res
          cb8 ce cId dt tr = do
                  res <- Contracts.registerTransactions ce [(cId, dt, tr)]
                  either (fail . show) return res
          cb9 re name _ _ code = do
                  res <- Reporting.addReport re code
                  either (fail . show)
                         (\b -> unless b $
                                fail $ "Failed to create report " ++ name)
                         res
          cb10 re name _ _ code = do
                  res <- Reporting.modifyReport re code
                  either (fail . show)
                         (\b -> unless b $
                                fail $ "Failed to update report " ++ name)
                         res
          cb11 re name = do
                  b <- Reporting.removeReport re name
                  unless b $ fail $ "Failed to delete report " ++ name

runWithPrompt :: String -> (String -> IO Bool) -> IO ()
runWithPrompt prompt f = do
  putStr prompt; hFlush stdout
  eof <- isEOF
  unless eof $ do
    line     <- getLine
    continue <- f line
    when continue $ runWithPrompt prompt f

quitServer :: PoetsSession -> ThreadId -> IO ()
quitServer session serverThread = putStrLn "shutting down POETS server" >>
                                  killThread serverThread >>
                                  stopPoetsSession session

registerSignalHandler :: PoetsSession -> ThreadId -> IO ()
registerSignalHandler session serverThread = mapM_ installQuitHandler signals
    where signals = [keyboardSignal, killProcess, keyboardTermination, softwareTermination, keyboardStop]
          installQuitHandler sig = installHandler sig (Catch quitHandler) Nothing
          quitHandler = quitServer session serverThread

poetsPrompt :: PoetsSession -> ThreadId -> IO ()
poetsPrompt session serverThread = do
  putStrLn "type 'quit' in order to close the server" 
  runWithPrompt "POETS Server> " react
    where react cmd = case cmd of
                        "quit" -> quitServer session serverThread >>
                                  return False
                        _ -> putStrLn "type 'quit' in order to close the server"
                             >> return True

-- | A basic threaded binary protocol socket server.
runFramedServer :: h
               -> (h -> (BinaryProtocol (FramedTransport Handle),
                         BinaryProtocol (FramedTransport Handle)) -> IO Bool)
               -> PortNumber
               -> IO a
runFramedServer hand proc_ port = runThreadedServer binaryAccept hand proc_ (PortNumber port)
  where binaryAccept s = do
            (h, _, _) <- accept s
            framed <- openFramedTransport h
            let binpro = BinaryProtocol framed
            return (binpro, binpro)

acceptLoop :: IO t -> (t -> IO Bool) -> IO a
acceptLoop accepter proc_ = forever $
    do ps <- accepter
       forkIO $ handle (\(_ :: SomeException) -> return ())
                  (loop $ proc_ ps)
  where loop m = do { continue <- m; when continue (loop m) }

--------------------------------------------------------------------------------
-- Template Haskell code for inlining prelude definitions
--------------------------------------------------------------------------------

repPrelude :: (String,FilePath)
repPrelude = ($(runIO $ fmap (LitE . StringL) $
                        readFile "defs/reports/Prelude.rep"),
              "defs/reports/Prelude.rep")

conPrelude :: (String,FilePath)
conPrelude = ($(runIO $ fmap (LitE . StringL) $
                        readFile "defs/contracts/Prelude.csl"),
              "defs/contracts/Prelude.csl")

dataDefs :: [(String,FilePath)]
dataDefs = $(do x <- runIO (do files <- getDirectoryContents "defs/data"
                               -- Only include files with .pce extension
                               let pceFiles = map ("defs/data" </>) $
                                              filter ((== ".pce") . snd . splitExtension) files
                               mapM (\x -> do s <- readFile x; return (s,x))
                                    pceFiles)
                return $ ListE $ map (\(x,y) -> (TupE [LitE $ StringL x,
                                                       LitE $ StringL y])) x)
