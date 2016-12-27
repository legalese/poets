--------------------------------------------------------------------------------
-- |
-- Module      :  PServer.Thrift.Decode
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements the decoding of Thrift data types to POETS data types.
-- Since Thrift does not permit recursive data types, the encoding emulates
-- trees using pointers.
--
--------------------------------------------------------------------------------
module PServer.Thrift.Decode
    (
     decodeTyp,
     decodeVal,
     decodeDateTime,
     decodeDuration,
     decodeQVal
    ) where

import Prelude hiding (mapM)
import GHC.Int
import qualified Data.Vector as Vector
import Data.Text.Lazy hiding (map)
import Data.Traversable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap hiding (HashMap)
import Poets.Data.Type as PType
import Poets.Data.Value as PValue
import Poets.Data.Value.Utils
import Type_Types
import Value_Types as VTypes
import Rules_Types
import Poets.Rules
import Control.Monad (liftM, liftM2)

decodeTyp :: Monad m => HashMap Int32 Type_Types.Typ -> Int32 -> m PType.Type
decodeTyp typMap idx =
    case HashMap.lookup idx typMap of
      Just typ ->
          case (typ_typeConstant typ,
                typ_typeRecord typ,
                typ_typeEntity typ,
                typ_typeList typ) of
            (Just Int,_,_,_) ->
                return iTInt
            (Just Bool,_,_,_) ->
                return iTBool
            (Just String,_,_,_) ->
                return iTString
            (Just Type_Types.Date,_,_,_) ->
                return iTDate
            (Just Type_Types.Time,_,_,_) ->
                return iTTime
            (Just Type_Types.DateTime,_,_,_) ->
                return iTDateTime
            (Just Type_Types.Duration,_,_,_) ->
                return iTDuration
            (Just Real,_,_,_) ->
                return iTReal
            (_,Just rName,_,_) ->
                return $ iTRecord (unpack rName)
            (_,_,Just rName,_) ->
                return $ iTEnt $ iTRecord (unpack rName)
            (_,_,_,Just idx') -> do
              -- We remove the current index from the list in order to avoid
              -- potential divergence
              typ' <- decodeTyp (HashMap.delete idx typMap) idx'
              return $ iTList typ'
            (_,_,_,_) ->
                fail "decodeTyp: Failed to decode type"
      Nothing ->
          fail $ "decodeTyp: Failed to lookup index " ++ show idx

decodeVal :: Monad m => HashMap Int32 VTypes.Val -> Int32 -> m PValue.Value
decodeVal valMap idx =
    case HashMap.lookup idx valMap of
      Just val ->
          case (val_intVal val,
                val_boolVal val,
                val_stringVal val,
                val_dateVal val,
                val_timeVal val,
                val_dateTimeVal val,
                val_durationVal val,
                val_realVal val,
                val_recordVal val,
                val_entVal val,
                val_listVals val) of
            (Just n,_,_,_,_,_,_,_,_,_,_) ->
                return $ iVInt $ fromIntegral n
            (_,Just b,_,_,_,_,_,_,_,_,_) ->
                return $ iVBool b
            (_,_,Just s,_,_,_,_,_,_,_,_) ->
                return $ iVString $ unpack s
            (_,_,_,Just d,_,_,_,_,_,_,_) ->
                liftM iVDate $ decodeDate d
            (_,_,_,_,Just t,_,_,_,_,_,_) ->
                liftM iVTime $ decodeTime t
            (_,_,_,_,_,Just dt,_,_,_,_,_) ->
                liftM iVDateTime $ decodeDateTime dt
            (_,_,_,_,_,_,Just d,_,_,_,_) ->
                liftM iVDuration $ decodeDuration d
            (_,_,_,_,_,_,_,Just r,_,_,_) ->
                return $ iVReal r
            (_,_,_,_,_,_,_,_,Just VTypes.Record{
                    record_recordName = rName,
                    record_fields = fIdxMap},_,_) -> do
                -- We remove the current index from the list in order to avoid
                -- potential divergence
                fvMap <- mapM (decodeVal $ HashMap.delete idx valMap) fIdxMap
                let vFields = map (\(fName,v) -> VF{vfieldName = unpack fName,
                                                    vfieldValue = v})
                                  (HashMap.toList fvMap)
                return $ iVRecord VR{vrecordName = unpack rName,
                                     vrecordFields = newFields vFields}
            (_,_,_,_,_,_,_,_,_,Just VTypes.Entity{
                    entity_recordName = rName,
                    entity_entPointer = id},_) ->
                return $ iVEnt VEntity{ventId = fromIntegral id,
                                          ventType = unpack rName,
                                          ventContext = Nothing}
            (_,_,_,_,_,_,_,_,_,_,Just l) -> do
                -- We remove the current index from the list in order to avoid
                -- potential divergence
                vs <- mapM (decodeVal $ HashMap.delete idx valMap) l
                return $ iVList $ Vector.toList vs
            (_,_,_,_,_,_,_,_,_,_,_) ->
                fail $ "decodeVal: Failed to decode value " ++ show idx
      Nothing ->
          fail $ "decodeVal: Failed to lookup index " ++ show idx

-- |Decode a POETS date value from a Thrift date value.
decodeDate :: Monad m => VTypes.Date -> m PValue.Date
decodeDate VTypes.Date{
                 date_year = year,
                 date_month = month,
                 date_day = day} =
    maybe (fail "decodeDate: Failed to decode date")
          return
          (createDate (fromIntegral year) (fromIntegral month)
                      (fromIntegral day))

-- |Decode a POETS time value from a Thrift time value.
decodeTime :: Monad m => VTypes.Time -> m PValue.Time
decodeTime VTypes.Time{
                 time_hour = hour,
                 time_minute = minute,
                 time_second = second,
                 time_microsecond = microsecond} =
    maybe (fail "decodeTime: Failed to decode time")
          return
          (createTime (fromIntegral hour) (fromIntegral minute)
                      (fromIntegral second) (fromIntegral microsecond))

-- |Decode a POETS datetime value from a Thrift datetime value.
decodeDateTime :: Monad m => VTypes.DateTime -> m PValue.DateTime
decodeDateTime VTypes.DateTime{dateTime_date = date,
                               dateTime_time = time} =
    liftM2 createDateTime (decodeDate date) (decodeTime time)

-- |Decode a POETS duration value from a Thrift duration value.
decodeDuration :: Monad m => VTypes.Duration -> m (PValue.VDuration Int)
decodeDuration VTypes.Duration{
                     duration_durationSeconds = s,
                     duration_durationMinutes = m,
                     duration_durationHours = h,
                     duration_durationDays = d,
                     duration_durationWeeks = w,
                     duration_durationMonths = mo,
                     duration_durationYears = y} =
    return VD{durationSeconds = fromIntegral s,
              durationMinutes = fromIntegral m,
              durationHours = fromIntegral h,
              durationDays = fromIntegral d,
              durationWeeks = fromIntegral w,
              durationMonths = fromIntegral mo,
              durationYears = fromIntegral y}

decodeQVal :: Monad m => HashMap Int32 QVal -> Int32 -> m POETSQValue
decodeQVal valMap idx =
    case HashMap.lookup idx valMap of
      Just val ->
          case (qVal_intVal val,
                qVal_boolVal val,
                qVal_stringVal val,
                qVal_dateVal val,
                qVal_timeVal val,
                qVal_dateTimeVal val,
                qVal_durationVal val,
                qVal_realVal val,
                qVal_recordVal val,
                qVal_entityVal val,
                qVal_listVals val,
                qVal_qVarVal val,
                qVal_unknownVal val
               ) of
            (Just n,_,_,_,_,_,_,_,_,_,_,_,_) ->
                return $ iVInt $ fromIntegral n
            (_,Just b,_,_,_,_,_,_,_,_,_,_,_) ->
                return $ iVBool b
            (_,_,Just s,_,_,_,_,_,_,_,_,_,_) ->
                return $ iVString $ unpack s
            (_,_,_,Just d,_,_,_,_,_,_,_,_,_) ->
                liftM iVDate $ decodeDate d
            (_,_,_,_,Just t,_,_,_,_,_,_,_,_) ->
                liftM iVTime $ decodeTime t
            (_,_,_,_,_,Just dt,_,_,_,_,_,_,_) ->
                liftM iVDateTime $ decodeDateTime dt
            (_,_,_,_,_,_,Just d,_,_,_,_,_,_) ->
                liftM iVDuration $ decodeDuration d
            (_,_,_,_,_,_,_,Just d,_,_,_,_,_) ->
                return $ iVReal d
            (_,_,_,_,_,_,_,_,Just VTypes.Record{
                    record_recordName = rName,
                    record_fields = fIdxMap},_,_,_,_) -> do
                -- We remove the current index from the list in order to avoid
                -- potential divergence
                fvMap <- mapM (decodeQVal $ HashMap.delete idx valMap) fIdxMap
                let vFields = map (\(fName,v) -> VF{vfieldName = unpack fName,
                                                    vfieldValue = v})
                                  (HashMap.toList fvMap)
                return $ iVRecord VR{vrecordName = unpack rName,
                                     vrecordFields = newFields vFields}
            (_,_,_,_,_,_,_,_,_,Just VTypes.Entity{
                    entity_recordName = rName,
                    entity_entPointer = id},_,_,_) ->
                return $ iVEnt VEntity{ventId = fromIntegral id,
                                          ventType = unpack rName,
                                          ventContext = Nothing}
            (_,_,_,_,_,_,_,_,_,_,Just l,_,_) -> do
                -- We remove the current index from the list in order to avoid
                -- potential divergence
                vs <- mapM (decodeQVal $ HashMap.delete idx valMap) l
                return $ iVList $ Vector.toList vs
            (_,_,_,_,_,_,_,_,_,_,_,Just Rules_Types.QVar{ qVar_id = id},_) ->
                return $ iQVar $ unpack id
            (_,_,_,_,_,_,_,_,_,_,_,_,Just Rules_Types.Unknown{ unknown_id = id}) ->
                return $ iUnknown $ unpack id
            (_,_,_,_,_,_,_,_,_,_,_,_,_) ->
                fail $ "decodeVal: Failed to decode value " ++ show idx
      Nothing ->
          fail $ "decodeVal: Failed to lookup index " ++ show idx
