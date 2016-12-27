{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DoAndIfThenElse #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the interface to the contract engine. All running
-- contracts are kept in-memory, and only contract meta data is persisted
-- in the event log, 'EventLog'. Upon restart of the server, the contracts are
-- updated based on the persisted event log, thereby omitting the need to
-- persist the state of the contract.
--
-- Currently set up to use the contract language \"CSL\".
--
--------------------------------------------------------------------------------
module Poets.Contracts
    (
     Party,
     ContractId,
     Contract(..),
     Closure(..),
     ClauseCore,
     ContractEngine,
     CError,
     ContractEngineError(..),
     CSLError(..),
     Transaction,
     TransactionEvent,
     ContractMetaData,
     TransactionPattern(..),
     Constraint,
     TransactionKind(..),
     createContractEngine,
     stopContractEngine,
     addContractDef,
     updateContractDef,
     deleteContractDef,
     getContract,
     getExpectedTransactions,
     registerTransactions,
     addContract,
     updateContract,
     deleteContract,
     Poets.Contracts.isConcludable,
     concludeContract
    ) where

import Prelude hiding (showList)
import Control.Monad.Error
import Data.Char (toLower)
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import Poets.Conc
import Poets.EventLog hiding (TypeError)
import Poets.EntityStore hiding (TypeError)
import Poets.Logging
import Poets.Data hiding (Type, Value)
import Poets.Reporting
import Poets.Contracts.Base
import Poets.Contracts.Repository hiding (getContract)
import qualified Poets.Contracts.Repository as R
import Poets.Contracts.Language.CSL hiding (getExpectedTransactions)
import qualified Poets.Contracts.Language.CSL as CSL
import Poets.Contracts.Language.CSL.Render
import Poets.Contracts.Language.CSL.Typing.TypeInferer
import Data.List (intersperse)

-- |A handle to the contract engine.
data ContractEngine = ContractEngine
    {
      -- |Contract engine mutex.
      contractEngineLock :: RWLock,
      -- |The in-memory contract repository.
      contractEngineRepository :: IORef ContractRepository,
      -- |The CSL definitions used at runtime.
      contractEngineDefinitions :: IORef Definitions,
      -- |The report engine, including the event log.
      contractEngineReportEngine :: ReportEngine
    }

-- |CSL contract repository.
type ContractRepository = Repository Closure

-- |The various errors the contract engine can produce.
type CError = ContractEngineError CSLError

-- |Create a handle to the contract engine.
createContractEngine :: (String,FilePath) -- ^Prelude definitions.
                     -> ReportEngine -- ^A handle to the report engine.
                     -> IO ContractEngine
createContractEngine prelude reportEngine = do
  infoCE "Starting contract engine"
  -- First read prelude definitions
  debugCE $ "Reading prelude definitions from " ++ show (snd prelude)
  -- The in-memory repository and definitions
  repoR <- newIORef Map.empty
  defsR <- newIORef initDefs
  (defs,recEnv) <- getDefs (getEventLog reportEngine) reportEngine defsR
  defs' <- readPreludeFile defs recEnv prelude
  writeIORef defsR defs'
  lock <- newRWLock
  debugCE "Contract engine successfully started"
  return ContractEngine{contractEngineLock = lock,
                        contractEngineRepository = repoR,
                        contractEngineDefinitions = defsR,
                        contractEngineReportEngine = reportEngine}
    where -- Shut down contract engine, and report error
          shutDown :: IO a
          shutDown = fail "Contract engine failed to start"
          -- Read CSL prelude
          readPreludeFile :: Definitions
                          -> RecEnv
                          -> (String,FilePath)
                          -> IO Definitions
          readPreludeFile defs recEnv (prelude,preludePath) = do
            case addPrelude defs recEnv preludePath prelude of
              Left err ->
                  emergencyCE (show err) >> shutDown
              Right defs' -> do
                  let typings = snd $ defPredefined defs'
                  unless (Map.null typings) (debugCE "Prelude types:")
                  mapM_ (debugCE . printType) (Map.toList typings)
                  return defs'
          -- Print an inferred type
          printType (n,tp) =
              let cs = map (\(x :&: _) -> show x)
                           (Set.toList $ typingTypeConstraints tp) in
              n ++ ": " ++
              (if null cs then
                   ""
               else
                   "(" ++ concat (intersperse ", " cs) ++ ") => ") ++
              show (typingType tp)
          initDefs :: Definitions
          initDefs =
              Definitions{defContractDefs = Map.empty,
                          defPredefined = builtInPredefined}

-- |Update report definitions by adding potentially new reports, and updating/
-- removing existing reports.
getDefs :: EventLog -> ReportEngine -> IORef Definitions
        -> IO (Definitions, RecEnv)
getDefs eventLog reportEngine defsR = withRecordEnv (getDataEngine eventLog) $ \recEnv -> do
  defs <- readIORef defsR
  store <- getEntityStore eventLog
  mods <- liftM Map.elems $ getReportModules reportEngine
  let rec = Record{
              recordName = reportsKeyword,
              recordFields = newFieldEnv $ map genField mods,
              recordExtends = Set.empty,
              recordAttributes = Set.singleton RecordIsAbstract}
  let defsPredefined = updatePredefined store mods (defPredefined defs)
  let defsRecordEnv = addRecordInfo (fmap deepInject3 recEnv) rec
  return (defs{defPredefined = defsPredefined}, defsRecordEnv)
  where genField :: ReportModule -> Field Type
        genField mod =
            Field{fieldName = repName mod,
                  fieldType = genType $ reportType mod,
                  fieldAttributes = Set.empty}
        repName :: ReportModule -> String
        repName mod = case reportName mod of
                        [] -> []
                        c:cs -> toLower c : cs
        genType :: ReportType -> Type
        genType ReportType{reportArgTypes = tps,
                           reportRetType = tp} =
            if null tps then
                -- Thunk the report if it is parameterless
                iTFunction iTUnit (deepInject3 tp)
            else
                foldr (\tp tps -> iTFunction (deepInject3 tp) tps)
                      (deepInject3 tp)
                      tps
        updatePredefined :: EntityStore
                         -> [ReportModule]
                         -> (Predefined, VarEnv)
                         -> (Predefined, VarEnv)
        updatePredefined store mods (pre, preTyping) =
            let rec events = VR{vrecordName = reportsKeyword,
                                vrecordFields =
                                    newFields $ map (wrapMod events store) mods} in
            let newTyping = Map.insert reportsKeyword
                                       (simpleTyping $ iTRecord reportsKeyword)
                                       preTyping in
            let newPreVals events = Map.insert reportsKeyword (iVRecord $ rec events)
                                                              (preVals pre events) in
            (pre{preVals = newPreVals}, newTyping)
        wrapMod :: [Event] -> EntityStore -> ReportModule -> VField Value
        wrapMod events store mod =
            VF{vfieldName = repName mod,
               vfieldValue = repFun events store mod}
        repFun :: [Event] -> EntityStore -> ReportModule -> Value
        repFun events store mod =
            let f :: Int -> [Value] -> Value
                f n args = if n == 1 then
                               iVFun $ \x -> runRep events store mod
                                                       (reverse $ x : args)
                           else
                               iVFun $ \x -> (return $ f (n - 1) (x : args))
                                                :: Either String Value
                argCount = length $ reportArgTypes $ reportType mod
            in
              if argCount == 0 then
                  -- Thunk the report if it is parameterless
                  iVFun $ \_ -> runRep events store mod []
              else
                  f argCount []
        runRep :: [Event]
               -> EntityStore
               -> ReportModule
               -> [Value]
               -> Either String Value
        runRep events store mod args =
            case mapM deepProject args of
              Nothing ->
                  throwError $ "Unable to project POETS value when running " ++
                               "report '" ++ reportName mod ++ "'"
              Just vs ->
                  either (throwError . show)
                         (return . deepInject)
                         (runReportFunction mod events store vs)

-- |Shutdown the contract engine. (Trivial, since all data is already persisted
-- in the event log.)
stopContractEngine :: ContractEngine -> IO ()
stopContractEngine _ = return ()

-- |Add a contract definition to the contract engine.
addContractDef :: ContractEngine -- ^Contract engine handle.
               -> String -- ^CSL code.
               -> IO (Either CError ())
addContractDef ce code = rwWriter lock $ do
  (defs,recEnv) <- getDefs eventLog reportEngine defsR
  let cDefs = defContractDefs defs
  case readContractDefinition defs recEnv emptyEntityTypingEnv code of
    Left err ->
        return $ throwError $ ContractLanguageError Nothing err
    Right cDef@ContractDefinition{contractDefName = name} ->
        if Map.member name cDefs then
            return $ throwError $ ContractDefAlreadyExists name
        else do
            let defs' = defs{defContractDefs = Map.insert name cDef cDefs}
            -- Add a "CreateContractDef" event to the event log
            res <- logCreateContractDefEvent eventLog
                                             name
                                             (contractDefType cDef)
                                             (contractDefDescription cDef)
                                             code
            case res of
              Left err ->
                  return $ throwError $ EventLogError err
              Right () -> do
                  -- Finally update the definitions
                  writeIORef defsR defs'
                  return $ return ()
   where lock = contractEngineLock ce
         reportEngine = contractEngineReportEngine ce
         eventLog = getEventLog reportEngine
         defsR = contractEngineDefinitions ce

-- |Update a contract definition in the contract engine.
updateContractDef :: ContractEngine -- ^Contract engine handle.
                  -> String -- ^CSL code.
                  -> IO  (Either CError ())
updateContractDef ce code = rwWriter lock $ do
  (defs,recEnv) <- getDefs eventLog reportEngine defsR
  let cDefs = defContractDefs defs
  case readContractDefinition defs recEnv emptyEntityTypingEnv code of
    Left err ->
        return $ throwError $ ContractLanguageError Nothing err
    Right cDef@ContractDefinition{contractDefName = name} ->
        if Map.notMember name cDefs then
            return $ throwError $ ContractDefNotFound name
        else do
            let defs' = defs{defContractDefs = Map.insert name cDef cDefs}
            -- Add an "UpdateContractDef" event to the event log
            res <- logUpdateContractDefEvent eventLog
                                             name
                                             (contractDefType cDef)
                                             (contractDefDescription cDef)
                                             code
            case res of
              Left err ->
                  return $ throwError $ EventLogError err
              Right () -> do
                  -- Finally update the definitions
                  writeIORef defsR defs'
                  return $ return ()
   where lock = contractEngineLock ce
         reportEngine = contractEngineReportEngine ce
         eventLog = getEventLog reportEngine
         defsR = contractEngineDefinitions ce

-- |Delete a contract definition in the contract engine.
deleteContractDef :: ContractEngine -- ^Contract engine handle.
                  -> TemplateName -- ^Name
                  -> IO  (Either CError ())
deleteContractDef ce name = rwWriter lock $ do
  (defs,_) <- getDefs eventLog reportEngine defsR
  let cDefs = defContractDefs defs
  if Map.notMember name cDefs then
      return $ throwError $ ContractDefNotFound name
  else do
    let defs' = defs{defContractDefs = Map.delete name cDefs}
    -- Add a "DeleteContractDef" event to the event log
    res <- logDeleteContractDefEvent eventLog name
    case res of
      Left err ->
          return $ throwError $ EventLogError err
      Right () -> do
          -- Finally update the definitions
          writeIORef defsR defs'
          return $ return ()
   where lock = contractEngineLock ce
         reportEngine = contractEngineReportEngine ce
         eventLog = getEventLog reportEngine
         defsR = contractEngineDefinitions ce

-- |First type check a list of transactions, then register them against the
-- in-memory repository, and perform monadic computation in the updated
-- repository. A lock is /not/ automatically acquired.
withTrs :: (MonadError (ContractEngineError CSLError) m) =>
           ContractEngine
        -> [TransactionEvent]
        -> (Definitions -> RecEnv -> [Event] -> ContractRepository -> IO (m a))
        -> IO (m a)
withTrs ContractEngine{contractEngineReportEngine = reportEngine,
                       contractEngineRepository = repoR,
                       contractEngineDefinitions = defsR} trs m = do
  let eventLog = getEventLog reportEngine
  (defs,recEnv) <- getDefs eventLog reportEngine defsR
  entityEnv <- getEntityTypingEnv eventLog
  -- First type check transactions
  case mapM_ (typeCheckTransaction recEnv entityEnv . (\(_,_,tr) -> tr)) trs of
    Left err ->
        return $ throwTypeError $ show err
    Right () -> do
        repo <- readIORef repoR
        events <- getEvents eventLog
        -- Then register transactions (from left to right)
        run events repo (reverse trs)
            where inp events = Input{inDefs = defs,
                                     inRecEnv = recEnv,
                                     inEvents = events}
                  run events repo [] =
                      m defs recEnv events repo
                  run events repo (tr:trs) =
                      case R.registerTransaction (inp events) repo tr of
                        Left err ->
                            return $ throwError err
                        Right repo' ->
                            run (trE tr : events) repo' trs
                                where trE (cId,dt,tr) = transactionEvent cId dt tr

-- |Return the textual representation of a contract at a given point in time
-- (possibly w.r.t. a set of /what if/ transactions).
getContract :: ContractEngine -- ^Contract engine handle.
            -> [TransactionEvent] -- ^/What if/ transactions.
            -> ContractId -- ^Contract ID.
            -> IO (Either CError (Contract String))
getContract ce trs cId = rwReader (contractEngineLock ce) $
  withTrs ce trs $ \defs recEnv events repo ->
      let inp = Input{inDefs = defs, inRecEnv = recEnv, inEvents = events} in
      either (return . throwError)
             (return . return . (\c -> c{contractContent = show $ clauseToDoc $ closureClause $ contractContent c}))
             (R.getContract inp repo cId)

-- |Return the  set of expected transactions for a given contract at a given
-- point in time (possibly w.r.t. a set of /what if/ transactions).
getExpectedTransactions :: ContractEngine -- ^Contract engine handle.
                        -> [TransactionEvent] -- ^/What if/ transactions.
                        -> ContractId -- ^Contract ID.
                        -> IO (Either CError [TransactionPattern])
getExpectedTransactions ce trs cId = rwReader (contractEngineLock ce) $
  withTrs ce trs $ \defs recEnv events repo -> do
    let inp = Input{inDefs = defs, inRecEnv = recEnv, inEvents = events}
    case R.getContract inp repo cId of
      Left err ->
          return $ throwError err
      Right c ->
          -- Generate set of expected transactions
          either (return . throwError . ContractLanguageError (Just cId))
                 (return . return)
                 (CSL.getExpectedTransactions inp c)

-- |Register a sequence of transactions against the contract repository. The
-- transactions are persisted in the event log, if registration succeeds.
registerTransactions :: ContractEngine -- ^Contract engine handle.
                     -> [TransactionEvent] -- ^The transactions to register.
                     -> IO (Either CError ())
registerTransactions ce trs = rwWriter lock $
  withTrs ce trs $ \_ _ _ repo -> do
    res <- logTrs trs
    case res of
      Left err ->
          return $ throwError err
      Right () -> do
          writeIORef repoR repo
          return $ return ()
    where lock = contractEngineLock ce
          eventLog = getEventLog $ contractEngineReportEngine ce
          repoR = contractEngineRepository ce
          logTrs :: [TransactionEvent] -> IO (Either CError ())
          logTrs [] = return $ return ()
          logTrs ((cId,dt,tr):trs) = do
            res <- logTransactionEvent eventLog cId dt tr
            case res of
              Left err -> return $ throwError $ EventLogError err
              Right () -> logTrs trs

-- |Instantiate a new contract based on the supplied meta data and the contract
-- template environment.
addContract :: ContractEngine -- ^Contract engine handle.
            -> ContractMetaData -- ^The meta data of the contract.
            -> IO (Either CError ContractId)
addContract ce m = rwWriter lock $
  -- We must acquire a lock in order to write to the repository
  withTrs ce [] $ \defs recEnv events repo -> do
    entityEnv <- getEntityTypingEnv eventLog
    let inp = Input{inDefs = defs, inRecEnv = recEnv, inEvents = events}
    -- First type check the contract meta data
    case typeCheckContractMetaData recEnv entityEnv m of
      Left err ->
          return $ throwTypeError err
      Right () ->
          let cId = R.generateContractId repo in
          case R.instantiate inp repo cId m of
            Left err ->
                return $ throwError err
            Right repo' -> do
                -- Add a "CreateContract" event to the event log
                res <- logCreateContractEvent eventLog cId m
                case res of
                  Left err ->
                      return $ throwError $ EventLogError err
                  Right () -> do
                      -- Finally update the repository
                      writeIORef repoR repo'
                      return $ return cId
  where lock = contractEngineLock ce
        eventLog = getEventLog $ contractEngineReportEngine ce
        repoR = contractEngineRepository ce

-- |Update a running contract based on the updated meta data and the contract
-- template environment. The update only succeeds if the updated contract is not
-- immediately breached.
updateContract :: ContractEngine -- ^Contract engine handle.
               -> ContractId -- ^The ID of the contract to update.
               -> ContractMetaData -- ^The (updated) meta data of the contract.
               -> IO (Either CError ())
updateContract ce cId m = rwWriter lock $
  -- We must acquire a lock in order to write to the repository
  withTrs ce [] $ \defs recEnv events repo -> do
    entityEnv <- getEntityTypingEnv eventLog
    let inp = Input{inDefs = defs, inRecEnv = recEnv, inEvents = events}
    -- First type check the contract meta data
    case typeCheckContractMetaData recEnv entityEnv m of
      Left err ->
          return $ throwTypeError err
      Right () ->
          -- Then check that the contract type does not change
          case R.getContract inp repo cId of
            Left err ->
                return $ throwError err
            Right c ->
                let Right tp1 = extractContractType $ contractMetaData c in
                let Right tp2 = extractContractType m in
                if tp1 /= tp2 then
                    -- We will not allow the contract type to change
                    return $ throwError $
                             ContractUpdateTypeMismatch cId tp1 tp2
                else
                    case R.update inp repo cId m of
                      Left err ->
                          return $ throwError err
                      Right repo' -> do
                          -- Add an "UpdateContract" event to the event log
                          res <- logUpdateContractEvent eventLog cId m
                          case res of
                            Left err ->
                                return $ throwError $ EventLogError err
                            Right () -> do
                                -- Finally update the repository
                                writeIORef repoR repo'
                                return $ return ()
  where lock = contractEngineLock ce
        eventLog = getEventLog $ contractEngineReportEngine ce
        repoR = contractEngineRepository ce

-- |Delete/cancel a running contract.
deleteContract :: ContractEngine -- ^Contract engine handle.
               -> ContractId -- ^The ID of the contract to delete.
               -> IO (Either CError ())
deleteContract ce cId = rwWriter lock $
  -- We must acquire a lock in order to write to the repository
  withTrs ce [] $ \_ _ _ repo -> do
    -- Add a "DeleteContract" event to the event log
    res <- logDeleteContractEvent eventLog cId
    case res of
      Left err ->
          return $ throwError $ EventLogError err
      Right () -> do
          -- Finally remove the contract from the in-memory repository
          writeIORef repoR $ R.remove repo cId
          return $ return ()
  where lock = contractEngineLock ce
        eventLog = getEventLog $ contractEngineReportEngine ce
        repoR = contractEngineRepository ce

-- |Check if a running contract is concludable at a given point in time.
isConcludable :: ContractEngine -- ^Contract engine handle.
              -> ContractId -- ^The ID of the contract to check.
              -> IO (Either CError Bool)
isConcludable ce cId = rwReader (contractEngineLock ce) $
  withTrs ce [] $ \defs recEnv events repo ->
      return $ concludable' defs recEnv events repo cId

-- |Conclude a running contract at a given point in time.
concludeContract :: ContractEngine -- ^Contract engine handle.
                 -> ContractId -- ^The ID of the contract to conclude.
                 -> IO (Either CError ())
concludeContract ce cId = rwWriter lock $ 
    -- We must acquire a lock in order to write to the repository
    withTrs ce [] $ \defs recEnv events repo ->
        -- First check if the contract is concludable
        case concludable' defs recEnv events repo cId of
          Left err ->
              return $ throwError err
          Right False ->
              return $ throwError $ ContractNotConcludable cId
          Right True -> do
              -- Then add a "ConcludeContract" event to the event log
              res <- logConcludeContractEvent eventLog cId
              case res of
                Left err ->
                    return $ throwError $ EventLogError err
                Right () -> do
                    -- Finally remove the contract from the in-memory repository
                    writeIORef repoR $ R.remove repo cId
                    return $ return ()
    where lock = contractEngineLock ce
          eventLog = getEventLog $ contractEngineReportEngine ce
          repoR = contractEngineRepository ce

-- Check if a contract is concludable
concludable' :: Definitions
             -> RecEnv
             -> [Event]
             -> Repository Closure
             -> ContractId
             -> Either CError Bool
concludable' defs recEnv events repo cId =
    let inp = Input{inDefs = defs, inRecEnv = recEnv, inEvents = events} in
    either throwError
           (either (throwError . ContractLanguageError (Just cId))
                   return .
                   (CSL.isConcludable inp))
           (R.getContract inp repo cId)

throwTypeError s =
    throwError $ ContractLanguageError Nothing $ TypeError s Nothing