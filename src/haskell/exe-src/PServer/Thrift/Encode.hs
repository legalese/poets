{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances,
  TemplateHaskell, TypeSynonymInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  PServer.Thrift.Encode
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements the encoding of POETS data types as Thrift data types.
-- Since Thrift does not permit recursive data types, the encoding emulates
-- trees using pointers.
--
--------------------------------------------------------------------------------
module PServer.Thrift.Encode
    (
     encodeTerm,
     encodeDate,
     encodeTime,
     encodeDateTime,
     encodeDuration
    ) where

import Prelude hiding (mapM)
import GHC.Int
import qualified Data.Vector as Vector
import Data.Text.Lazy hiding (map)
import Data.Maybe
import Data.Comp
import Data.Comp.Derive
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap hiding (HashMap)
import Poets.Data.Type as PType
import Poets.Data.Value as PValue
import Poets.Data.Value.Utils
import Poets.Contracts.Language.CSL.AST.Exp as CExp
import Poets.Contracts.Language.CSL.AST.RecordFieldName
import Type_Types as TTypes
import Value_Types as VTypes
import Rules_Types as RTypes
import Contracts_Types as CTypes
import Control.Monad.State
import Poets.Rules as QV


-- |Encoding is performed inside the state monad: the state collects the list of
-- encoded values, and the return type is the index of an encoding.
type EM e = State (Map Int32 e) Int32

-- |Add an encoded value to the list, and generate a fresh index.
addE :: e -> EM e
addE x = do
  encMap <- get
  let idx = if Map.null encMap then 0 else fst (Map.findMax encMap) + 1
  put $ Map.insert idx x encMap
  return idx

-- |The encoder algebra.
class Traversable f => Encoder f e where
    encodeAlg :: f Int32 -> EM e

$(derive [liftSum] [''Encoder])

-- |Encode a term.
encodeTerm :: Encoder f e => Term f -> (Int32, Map Int32 e)
encodeTerm e = runState (cataM encodeAlg e) Map.empty

instance Encoder PType.TypeConstant TTypes.Typ where
    encodeAlg TInt =
        addE emptyTyp{typ_typeConstant = Just Int}
    encodeAlg TBool =
        addE emptyTyp{typ_typeConstant = Just Bool}
    encodeAlg TString =
        addE emptyTyp{typ_typeConstant = Just String}
    encodeAlg TDate =
        addE emptyTyp{typ_typeConstant = Just TTypes.Date}
    encodeAlg TTime =
        addE emptyTyp{typ_typeConstant = Just TTypes.Time}
    encodeAlg TDateTime =
        addE emptyTyp{typ_typeConstant = Just TTypes.DateTime}
    encodeAlg TDuration =
        addE emptyTyp{typ_typeConstant = Just TTypes.Duration}
    encodeAlg TReal =
        addE emptyTyp{typ_typeConstant = Just Real}
    encodeAlg (TRecord rName) =
        addE emptyTyp{typ_typeRecord = Just $ pack rName}

instance Encoder PType.TypeEnt TTypes.Typ where
    encodeAlg (TEnt index) = do
      s <- get
      let rName = fromJust $ typ_typeRecord $ fromJust $ Map.lookup index s
      put $ Map.delete index s
      addE emptyTyp{typ_typeEntity = Just rName}

instance Encoder PType.TypeList TTypes.Typ where
    encodeAlg (TList idx) =
        addE emptyTyp{typ_typeList = Just idx}

emptyTyp :: TTypes.Typ
emptyTyp = Typ{typ_typeConstant = Nothing,
               typ_typeRecord = Nothing,
               typ_typeEntity = Nothing,
               typ_typeList = Nothing}

instance Encoder PValue.Val VTypes.Val where
    encodeAlg (VInt n) =
        addE emptyVal{val_intVal = Just $ fromIntegral n}
    encodeAlg (VBool b) =
        addE emptyVal{val_boolVal = Just b}
    encodeAlg (VString s) =
        addE emptyVal{val_stringVal = Just $ pack s}
    encodeAlg (VDate d) =
        addE emptyVal{val_dateVal = Just $ encodeDate d}
    encodeAlg (VTime t) =
        addE emptyVal{val_timeVal = Just $ encodeTime t}
    encodeAlg (VDateTime dt) =
        addE emptyVal{val_dateTimeVal = Just $ encodeDateTime dt}
    encodeAlg (VDuration d) =
        addE emptyVal{val_durationVal = Just $ encodeDuration d}
    encodeAlg (VReal d) =
        addE emptyVal{val_realVal = Just d}
    encodeAlg (VRecord VR{vrecordName = rName,
                          vrecordFields = fields}) =
        let fIdxMap = HashMap.fromList $ 
                      map (\(x,y)->(pack x,vfieldValue y)) $ Map.toList $ fieldsMap fields in
        addE emptyVal{val_recordVal =Just 
                          VTypes.Record{
                                     record_recordName = pack rName,
                                     record_fields = fIdxMap}}
    encodeAlg (VEnt r) =
        addE emptyVal{val_entVal = Just 
                          VTypes.Entity{
                                   entity_recordName = pack $ ventType r,
                                   entity_entPointer = fromIntegral $ ventId r}}
    encodeAlg (VList l) =
        addE emptyVal{val_listVals = Just $ Vector.fromList l}

emptyVal :: VTypes.Val
emptyVal = Val{val_intVal = Nothing,
               val_boolVal = Nothing,
               val_stringVal = Nothing,
               val_dateVal = Nothing,
               val_timeVal = Nothing,
               val_dateTimeVal = Nothing,
               val_durationVal = Nothing,
               val_realVal = Nothing,
               val_recordVal = Nothing,
               val_entVal = Nothing,
               val_listVals = Nothing}

instance Encoder PValue.Val CTypes.Exp where
    encodeAlg (VInt n) =
        addE emptyExp{exp_intExp = Just $ fromIntegral n}
    encodeAlg (VBool b) =
        addE emptyExp{exp_boolExp = Just $ b}
    encodeAlg (VString s) =
        addE emptyExp{exp_stringExp = Just $ pack s}
    encodeAlg (VDate d) =
        addE emptyExp{exp_dateExp = Just $ encodeDate d}
    encodeAlg (VTime t) =
        addE emptyExp{exp_timeExp = Just $ encodeTime t}
    encodeAlg (VDateTime dt) =
        addE emptyExp{exp_dateTimeExp = Just $ encodeDateTime dt}
    encodeAlg (VDuration d) =
        addE emptyExp{exp_durationExp = Just $ encodeDuration d}
    encodeAlg (VReal d) =
        addE emptyExp{exp_realExp = Just d}
    encodeAlg (VRecord VR{vrecordName = rName,
                          vrecordFields = fields}) =
        let fIdxMap = HashMap.fromList 
                      $ map (\(x,y) -> (pack x, vfieldValue y))
                      $ Map.toList $ fieldsMap fields in
        addE emptyExp{exp_recordExp = Just
                          VTypes.Record{
                                     record_recordName = pack rName,
                                     record_fields = fIdxMap}}
    encodeAlg (VEnt r) =
        addE emptyExp{exp_entityExp = Just
                          VTypes.Entity{
                                   entity_recordName = pack $ ventType r,
                                   entity_entPointer = fromIntegral $ ventId r}}
    encodeAlg (VList l) =
        addE emptyExp{exp_listExps = Just $ Vector.fromList l}

instance Encoder CExp.CoreExp CTypes.Exp where
    encodeAlg EVar{expVar = x} =
        case x of
          "foldr" -> addE emptyExp{exp_functionExp = Just Foldr}
          "ceil" -> addE emptyExp{exp_functionExp = Just Ceil}
          "subtractDate" -> addE emptyExp{exp_functionExp = Just SubtractDate}
          "reports" -> addE emptyExp{exp_functionExp = Just Reports}
          _ -> addE emptyExp{exp_variableExp = Just $ pack x}
    encodeAlg ELambda{expLambdaParam = var,
                      expLambdaBody = e} =
        addE emptyExp{exp_lambdaExp = Just
                          CTypes.Lambda{
                                     lambda_variable = pack var,
                                     lambda_bodyExp = e}}
    encodeAlg EApply{expApplyFn = e1,
                     expApplyArg = e2} =
        addE emptyExp{exp_applicationExp = Just
                          CTypes.Application{
                                     application_functionExp = e1,
                                     application_argumentExp = e2}}
    encodeAlg EProj{expProjFieldName = f,
                    expProjRecord = e} =
        addE emptyExp{exp_recordProjExp = Just
                          CTypes.RecordProj{
                                     recordProj_fieldName = pack f,
                                     recordProj_recordExp = e}}
    encodeAlg EUpdate{expUpdateRecord = e1,
                      expUpdateFieldName = f,
                      expUpdateValue = e2} =
        addE emptyExp{exp_recordUpdateExp = Just 
                          CTypes.RecordUpdate{
                                     recordUpdate_recordExp = e1,
                                     recordUpdate_fieldName = pack f,
                                     recordUpdate_updateExp = e2}}
    encodeAlg EBinOp{expBinOp = o,
                     expBinOpArg1 = e1,
                     expBinOpArg2 = e2} =
        addE emptyExp{exp_binaryOpExp = Just 
                          CTypes.BinaryOp{
                                     binaryOp_operator = encodeBinOp o,
                                     binaryOp_leftExp = e1,
                                     binaryOp_rightExp = e2}}
    encodeAlg EIfThenElse{expCondition = c,
                          expConditionThen = e1,
                          expConditionElse = e2} =
        addE emptyExp{exp_ifThenElseExp = Just
                          CTypes.IfThenElse{
                                     ifThenElse_conditionalExp = c,
                                     ifThenElse_thenExp = e1,
                                     ifThenElse_elseExp = e2}}
    encodeAlg ECase{expCase = c,
                    expCases = cs} =
        addE emptyExp{exp_caseExp = Just
                          CTypes.Case{
                                     case_caseExp = c,
                                     case_caseExps = Vector.fromList $ encCases cs}}
            where encCases =
                      map (\CExp.CaseExp{caseExpRecordName = rName,
                                         caseExpVar = x,
                                         caseExpBody = idx} ->
                               CTypes.CaseExp{caseExp_rName = pack rName,
                                              caseExp_variable = pack x,
                                              caseExp_body = idx})

instance Encoder VUnit CTypes.Exp where
    encodeAlg VUnit =
        addE emptyExp{exp_unitExp = Just Unit}

instance Encoder RecordFieldName CTypes.Exp where
    encodeAlg (RecordFieldName fName) =
        addE emptyExp{exp_fieldNameExp = Just $ pack fName}

emptyExp :: CTypes.Exp
emptyExp = Exp{exp_intExp = Nothing,
               exp_boolExp = Nothing,
               exp_stringExp = Nothing,
               exp_dateExp = Nothing,
               exp_timeExp = Nothing,
               exp_dateTimeExp = Nothing,
               exp_durationExp = Nothing,
               exp_realExp = Nothing,
               exp_recordExp = Nothing,
               exp_entityExp = Nothing,
               exp_listExps = Nothing,
               exp_fieldNameExp = Nothing,
               exp_recordProjExp = Nothing,
               exp_recordUpdateExp = Nothing,
               exp_binaryOpExp = Nothing,
               exp_ifThenElseExp = Nothing,
               exp_caseExp = Nothing,
               exp_variableExp = Nothing,
               exp_lambdaExp = Nothing,
               exp_applicationExp = Nothing,
               exp_functionExp = Nothing,
               exp_unitExp = Nothing}

-- |Translate a POETS datetime value to a Thrift datetime value.
encodeDateTime :: PValue.DateTime -> VTypes.DateTime
encodeDateTime dt =
    let (d,t) = dateTimeToDateTime dt in
    VTypes.DateTime{dateTime_date = encodeDate d,
                    dateTime_time = encodeTime t}

-- |Translate a POETS date value to a Thrift date value.
encodeDate :: PValue.Date -> VTypes.Date
encodeDate d =
    let (year, month, day) = dateToYearMonthDay d in
    VTypes.Date{date_year = fromIntegral year,
                date_month = fromIntegral month,
                date_day = fromIntegral day}

-- |Translate a POETS time value to a Thrift time value.
encodeTime :: PValue.Time -> VTypes.Time
encodeTime t =
    let (hour, minute, second, microsecond) = timeToHourMinSecMicroSec t in
    VTypes.Time{time_hour = fromIntegral hour,
                time_minute = fromIntegral minute,
                time_second = fromIntegral second,
                time_microsecond = fromIntegral microsecond}

-- |Translate a POETS duration value to a Thrift duration value.
encodeDuration :: PValue.VDuration Int -> VTypes.Duration
encodeDuration VD {durationSeconds = s,
                   durationMinutes = m,
                   durationHours = h,
                   durationDays = d,
                   durationWeeks = w,
                   durationMonths = mo,
                   durationYears = y} =
    VTypes.Duration{
        duration_durationSeconds = fromIntegral s,
        duration_durationMinutes = fromIntegral m,
        duration_durationHours = fromIntegral h,
        duration_durationDays = fromIntegral d,
        duration_durationWeeks = fromIntegral w,
        duration_durationMonths = fromIntegral mo,
        duration_durationYears = fromIntegral y}

encodeBinOp :: CExp.BinOp -> CTypes.BinOp
encodeBinOp o =
    case o of
      CExp.EQ -> CTypes.EQ
      CExp.LEQ -> CTypes.LEQ
      CExp.PLUS -> CTypes.PLUS
      CExp.TIMES -> CTypes.TIMES
      CExp.DIV -> CTypes.DIV
      CExp.AND -> CTypes.AND
      CExp.CONS -> CTypes.CONS
      CExp.DPLUS -> CTypes.DPLUS
      CExp.DTIMES -> CTypes.DTIMES

instance Encoder PValue.Val QVal where
    encodeAlg (VInt n) =
        addE emptyQVal{qVal_intVal = Just $ fromIntegral n}
    encodeAlg (VBool b) =
        addE emptyQVal{qVal_boolVal = Just b}
    encodeAlg (VString s) =
        addE emptyQVal{qVal_stringVal = Just $ pack s}
    encodeAlg (VDate d) =
        addE emptyQVal{qVal_dateVal = Just $ encodeDate d}
    encodeAlg (VTime t) =
        addE emptyQVal{qVal_timeVal = Just $ encodeTime t}
    encodeAlg (VDateTime dt) =
        addE emptyQVal{qVal_dateTimeVal = Just $ encodeDateTime dt}
    encodeAlg (VDuration d) =
        addE emptyQVal{qVal_durationVal = Just $ encodeDuration d}
    encodeAlg (VReal d) =
        addE emptyQVal{qVal_realVal = Just d}
    encodeAlg (VRecord VR{vrecordName = rName,
                          vrecordFields = fields}) =
        let fIdxMap = HashMap.fromList 
                      $ map (\ (x,y)-> (pack x, vfieldValue y))
                      $ Map.toList $ fieldsMap fields in
        addE emptyQVal{qVal_recordVal = Just 
                          VTypes.Record{
                                     record_recordName = pack rName,
                                     record_fields = fIdxMap}}
    encodeAlg (VEnt r) =
        addE emptyQVal{qVal_entityVal = Just
                          VTypes.Entity{
                                   entity_recordName = pack $ ventType r,
                                   entity_entPointer = fromIntegral $ ventId r}}
    encodeAlg (VList l) =
        addE emptyQVal{qVal_listVals = Just $ Vector.fromList l}

instance Encoder QueryValue QVal where
    encodeAlg(QV.QVar id) =
        addE emptyQVal{qVal_qVarVal = Just  RTypes.QVar { qVar_id = pack id } }
    encodeAlg(QV.Unknown id) =
        addE emptyQVal{qVal_unknownVal = Just RTypes.Unknown { unknown_id = pack id} }

emptyQVal :: QVal
emptyQVal = QVal{qVal_intVal = Nothing,
                 qVal_boolVal = Nothing,
                 qVal_stringVal = Nothing,
                 qVal_dateVal = Nothing,
                 qVal_timeVal = Nothing,
                 qVal_dateTimeVal = Nothing,
                 qVal_durationVal = Nothing,
                 qVal_realVal = Nothing,
                 qVal_recordVal = Nothing,
                 qVal_entityVal = Nothing,
                 qVal_listVals = Nothing,
                 qVal_qVarVal = Nothing,
                 qVal_unknownVal = Nothing
                }
