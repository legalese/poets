--------------------------------------------------------------------------------
-- |
-- Module      :  PServer.Thrift
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Morten Ib Nielsen, Tom Hvitved, Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements conversion to/from Thrift types.
--
--------------------------------------------------------------------------------
module PServer.Thrift where

import Prelude hiding (mapM)
import Control.Exception
import PServer.Thrift.Encode
import PServer.Thrift.Decode
import Value_Types hiding (Record)
import Type_Types
import Data_Types as DT
import Entities_Types as ET
import Contracts_Types as CT
import Reporting_Types
import Rules_Types
import Poets_Types
import Poets.Logging
import qualified Poets.EventLog as EventLog
import qualified Poets.EntityStore as EntityStore
import Data.Maybe
import Data.Text.Lazy hiding (map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Set (Set)
import Data.Map (Map)
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap hiding (HashMap)


-- Data module
import Poets.Data as Data hiding (undefinedRecord)
import Poets.Data.Value.Render (printVal)

-- Contracts
import qualified Poets.Contracts as Contracts

-- Reporting
import qualified Poets.Reporting as Reporting

-- Rule engine
import qualified Poets.Rules as Rules



toHashMap :: (Ord k, Hashable k) => Map k v -> HashMap k v
toHashMap m = HashMap.fromList (Map.toList m)

toHashSet :: (Ord k, Hashable k) => Set k -> HashSet k
toHashSet m = HashSet.fromList (Set.toList m)

toHashSet' :: Set String -> HashSet Text
toHashSet' m = HashSet.fromList (map pack (Set.toList m))

toHashMap' :: Map String v -> HashMap Text v
toHashMap' m = HashMap.fromList (map (\(k,v)->(pack k, v)) (Map.toList m))


--------------------------------------------------------------------------------
-- Functions for throwing (thrift) exceptions
--------------------------------------------------------------------------------

rulesError :: Rules.QueryError -> IO a
rulesError e = do
  let s' = show e
  errorPServer s'
  let s = pack s'
  case e of
    Rules.RunTimeError _ -> throw $ RunTimeException s
    Rules.Inconsistent -> throw $ Inconsistent s
    Rules.NoRuleSets -> throw $ NoRuleSets s
    Rules.CannotComplete -> throw $ CannotComplete s
    Rules.PleaseSpecify qvs -> throw $ PleaseSpecify $ Vector.fromList $
                               map qValueEncode qvs

decodeError s =
    errorPServer s >> throw (Value_Types.DecodeException $ pack s)

dataError e =
  case e of
    Data.RunTimeError s ->
        throw $ RunTimeException $  pack s
    Data.ParseError s ->
        throw $ DT.ParseError $  pack $ show s
    Data.ValidationError s ->
        throw $ DT.ValidationError $  pack s

-- Forward an event log error to the client, and log it
eventLogError :: EventLog.LogError -> IO a
eventLogError e = do
  errorPServer $ show e
  case e of
    EventLog.RunTimeError s ->
        throw $ RunTimeException $ pack s
    EventLog.TypeError s ->
        throw $ TypeException $ pack s
    EventLog.EntityStoreError e ->
        entityStoreError e
    EventLog.DataDefError e ->
        dataError e

entityStoreError :: EntityStore.EntityStoreError -> IO a
entityStoreError e =
    case e of
      EntityStore.TypeError s ->
          throw $ TypeException $ pack s
      EntityStore.EntityDeleted eId ->
          throw $ ET.EntityDeleted (fromIntegral eId)
      EntityStore.EntityNotFound eId ->
          throw $ ET.EntityNotFound (fromIntegral eId)

-- Forward a contract engine error to the client, and log it
contractsError :: Contracts.CError -> IO a
contractsError e = do
  errorPServer $ show e
  case e of
    Contracts.ContractDefAlreadyExists name ->
        throw $ ContractDefExistsException (pack name)
    Contracts.ContractDefNotFound name ->
        throw $ ContractDefNotFoundException (pack name)
    Contracts.ContractNotFound cid ->
        throw $ ContractNotFoundException (fromIntegral cid)
    Contracts.TransactionTooOld cid t1 t2 ->
        throw $ TransactionTooOldException (fromIntegral cid)
                                           (encodeDateTime t1)
                                           (encodeDateTime t2)
    Contracts.ContractNotConcludable cid ->
        throw $ ContractNotConcludable (fromIntegral cid)
    Contracts.ContractUpdateTypeMismatch cId cType1 cType2 ->
        throw $ UpdateTypeMismatch (fromIntegral cId)
                                   (pack cType1)
                                   (pack cType2)
    Contracts.EventLogError e' ->
        case e' of
          EventLog.RunTimeError s ->
              throw $ RunTimeException $ pack s
          EventLog.TypeError s ->
              throw $ TypeException $ pack s
          EventLog.EntityStoreError e ->
              entityStoreError e
          EventLog.DataDefError e ->
              dataError e
    Contracts.ContractLanguageError mCId e' ->
        case e' of
          Contracts.RunTimeError err ->
              throw $ RunTimeException $ pack err
          Contracts.ContractTemplateNotFound tName ->
              throw $ TemplateNotFoundException (pack tName)
          Contracts.ContractTemplateTypeMismatch tName cType1 cType2 ->
              throw $ TemplateTypeMismatchException (pack tName) (pack cType1)
                                                    (pack cType2)
          Contracts.UnexpectedTransaction tr ->
              throw $ UnexpectedTransaction (valueEncode tr)
          Contracts.ContractBreach dt parties ->
              throw $ ContractBreach (fromIntegral $ fromJust mCId)
                                     (encodeDateTime dt)
                                     (Vector.fromList $ map encodeBreach parties)
                  where encodeBreach :: (Contracts.Party, String) 
                                     -> Breach
                        encodeBreach (party, descr) =
                            Breach (valueEncode party) (pack descr)
          Contracts.TypeError _ _ ->
              throw $ TypeException $ pack $ show e'
          Contracts.ParseError _ ->
              throw $ ParseException $ pack $ show e'
          Contracts.ClauseTemplateNotGuarded s ->
              throw $ GuardednessException $ pack s

undefinedRecord rName = do
    errorPServer $ "Record not found: " ++ rName
    throw (UndefinedRecord $ pack rName)
reportInitException s =
    errorPServer s >> throw (ReportInitException $ pack s)
reportNotFoundException =
    errorPServer "Report not found" >> throw ReportNotFoundException
reportRuntimeException s =
    errorPServer s >> throw (RunTimeException $ pack s)
reportTypeException s =
    errorPServer s >> throw (TypeException $ pack s)

queryReportError (Reporting.ReportNotFound _) = reportNotFoundException
queryReportError (Reporting.IllTypedArgument msg) = reportTypeException (show msg)
queryReportError (Reporting.ReportFunctionError err) = reportRuntimeException (show err)


--------------------------------------------------------------------------------
-- Functions for translating to/from thrift types
--------------------------------------------------------------------------------

-- Encode a POETS type in the Thrift representation
typeEncode :: Data.Type -> Type_Types.Type
typeEncode tp = let (idx,typMap) = encodeTerm tp in
                Type{type_types = toHashMap typMap,
                     type_root = idx}

-- Decode a POETS type from the Thrift representation
typeDecode :: Type_Types.Type -> IO Data.Type
typeDecode Type{type_types = typMap,
                type_root = idx} =
    either decodeError return $ decodeTyp typMap idx

-- Encode a POETS value in the Thrift representation
valueEncode :: Data.Value -> Value_Types.Value
valueEncode v = let (idx,valMap) = encodeTerm v in
                Value{value_values = toHashMap valMap,
                      value_root = idx}

-- Decode a POETS value from the Thrift representation
valueDecode :: Value_Types.Value -> IO Data.Value
valueDecode Value{value_values = valMap,
                  value_root = idx} =
    either decodeError return $ decodeVal valMap idx

-- Encode a CSL constraint in the Thrift representation
constraintEncode :: Contracts.Constraint -> Expression
constraintEncode c = let (idx,expMap) = encodeTerm c in
                     Expression{expression_expressions = toHashMap expMap,
                                expression_root = idx}

-- Encode a POETS query value in the Thrift representation
qValueEncode :: Rules.POETSQValue -> Rules_Types.QValue
qValueEncode v = let (idx,valMap) = encodeTerm v in
                QValue{qValue_values = toHashMap valMap,
                       qValue_root = idx}

-- Decode a POETS query value from the Thrift representation
qValueDecode :: Rules_Types.QValue -> IO Rules.POETSQValue
qValueDecode QValue{qValue_values = valMap,
                    qValue_root = idx} =
    either decodeError return $ decodeQVal valMap idx

-- Decode a POETS datetime value from the Thrift representation
fromDateTime :: Value_Types.DateTime -> IO Data.DateTime
fromDateTime dt = either decodeError return (decodeDateTime dt)

toRecordDefinition :: Record Data.Type -> RecordDefinition
toRecordDefinition Record{recordName = rName,
                          recordFields = fieldEnv,
                          recordExtends = superClasses,
                          recordAttributes = rAttrs} =
    RecordDefinition
    {
      recordDefinition_recordName = pack rName,
      recordDefinition_fieldsDefinitions =
          toHashMap' $ Map.map toFieldDefinition $ fieldEnvMap fieldEnv,
      recordDefinition_superClasses = toHashSet' superClasses,
      recordDefinition_recordAttributes =
          HashSet.fromList $ map toRecordAttribute $ Set.toList rAttrs
    }

toRecordAttribute :: Data.RecordAttribute -> DT.RecordAttribute
toRecordAttribute a =
    case a of
      Data.RecordIsAbstract ->
          a'{recordAttribute_basicAttr = Just DT.RecordIsAbstract}
      Data.RecordIsLocked ->
          a'{recordAttribute_basicAttr = Just DT.RecordIsLocked}
      Data.RecordIsHidden ->
          a'{recordAttribute_basicAttr = Just DT.RecordIsHidden}
    where a' = DT.RecordAttribute{recordAttribute_basicAttr = Nothing}

toFieldDefinition :: Field Data.Type -> FieldDefinition
toFieldDefinition Field{fieldName = rName,
                        fieldType = tp,
                        fieldAttributes = fAttrs} =
    FieldDefinition
    {
      fieldDefinition_fieldName = pack rName,
      fieldDefinition_fieldType = typeEncode tp,
      fieldDefinition_fieldAttributes =
          HashSet.fromList $ map toFieldAttribute $ Set.toList fAttrs
    }

toFieldAttribute :: Data.FieldAttribute -> DT.FieldAttribute
toFieldAttribute a =
    case a of
      Data.FieldOrder o ->
          a'{fieldAttribute_fieldOrder = Just $ fromIntegral o}
      Data.FieldRestriction r ->
          a'{fieldAttribute_fieldRestriction = Just $ pack r}
    where a' = DT.FieldAttribute{fieldAttribute_fieldOrder = Nothing,
                                 fieldAttribute_fieldRestriction = Nothing}

toReport :: Reporting.ReportModule -> Report
toReport Reporting.ReportModule
             {Reporting.reportType = ty,
              Reporting.reportDesc = desc,
              Reporting.reportName = name,
              Reporting.reportTags = tags,
              Reporting.reportSpec = spec
             } = Report{report_name = pack name,
                        report_type = (toReportType ty),
                        report_description = pack desc,
                        report_tags = toHashSet' tags,
                        report_spec = pack spec}

toReportType :: Reporting.ReportType -> ReportType
toReportType Reporting.ReportType{Reporting.reportArgTypes = args,
                                  Reporting.reportRetType = ret} =
      ReportType{reportType_returnType = typeEncode ret, 
                 reportType_argTypes = Vector.fromList $ map typeEncode args}

fromTransaction :: Transaction -> IO Contracts.TransactionEvent
fromTransaction Transaction{transaction_contractId = cId,
                            transaction_timeStamp = dt,
                            transaction_transactionData = d} = do
  dt' <- fromDateTime dt
  v <- valueDecode d
  return (fromIntegral cId, dt', v)

printTrEvent :: Contracts.TransactionEvent -> String
printTrEvent (cId, dt, tr) = "<" ++ show cId ++ ", "
                                 ++ show dt ++ ", "
                                 ++ printVal tr ++ ">"

toResidual :: Contracts.Contract String -> Residual
toResidual Contracts.Contract{Contracts.contractLastUpdate = dt,
                              Contracts.contractContent = c} =
    Residual{residual_currentTime = encodeDateTime dt,
             residual_residualClause = pack c}

toTransactionPatterns :: ContractId
                      -> [Contracts.TransactionPattern]
                      -> [TransactionPattern]
toTransactionPatterns cId = map (toTransactionPattern cId)

toTransactionPattern :: ContractId
                     -> Contracts.TransactionPattern
                     -> TransactionPattern
toTransactionPattern cId trP =
    TransactionPattern{
      transactionPattern_contractId = cId,
      transactionPattern_description = Just $ pack description,
      transactionPattern_transactionKind = kind,
      transactionPattern_responsible = fmap valueEncode r,
      transactionPattern_transactionType = pack transactionType,
      transactionPattern_predicate = predicate,
      transactionPattern_deadline = deadline
    }
    where description = Contracts.transactionPatternDescription trP
          (kind, r) = toTransactionKind $ Contracts.transactionPatternKind trP
          transactionType = Contracts.transactionPatternTransactionType trP
          predicate = constraintEncode $ Contracts.transactionPatternConstraint trP
          deadline = toDeadline $ Contracts.transactionPatternDeadline trP

toDeadline :: (Data.DateTime, Data.DateTime) -> Deadline
toDeadline (lower, upper) =
    Deadline{deadline_lowerLimit = encodeDateTime lower,
             deadline_upperLimit = encodeDateTime upper}

toTransactionKind :: Contracts.TransactionKind
                  -> (TransactionKind, Maybe Data.Value)
toTransactionKind (Contracts.TKObligation r) = (Obligation, Just r)
toTransactionKind Contracts.TKExternalChoice = (Permission, Nothing)
