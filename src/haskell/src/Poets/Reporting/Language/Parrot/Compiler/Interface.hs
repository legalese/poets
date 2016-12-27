{-# LANGUAGE FlexibleContexts, TypeOperators #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Compiler.Interface
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides the interface used by the Haskell code
-- generated from a Parrot program (via module
-- ''Poets.Reporting.Language.Parrot.Compiler.Haskell'').
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Compiler.Interface
    ( EReportFunction,
      EValue,
      ERValue,
      rEvents,
      rTypeOf,
      rTypeOf',
      rRefTypeOf,
      rRefTypeOf',
      rHasType,
      rString,
      rError,
      rRecord,
      rList,
      rCons,
      rFieldLookup,
      rFieldsMod,
      rFoldr,
      rIf,
      rFalse,
      rTrue,
      rEq,
      rNeq,
      rLt,
      rGt,
      rLte,
      rGte,
      rMax,
      rMin,
      rAnd,
      rOr,
      rNot,
      rPlus,
      rMinus,
      rTimes,
      rAbs,
      rSignum,
      rNegate,
      rInteger,
      rRational,
      rDiv,
      rRecip,
      rCharList,
     rChar,
     rLeft,
     rRight,
     rTuple,
     rProj,
     rUnit,
     rDate,
     rDateTime,
     rTime,
     rRef,
     rCompDateTime,
     rDuration,
     rDuration',
     rDurPlus,
     rDurMinus,
     rCase,
     rDerefNow,
     rDerefCxt
    ) where


import Poets.Data
import Poets.Reporting.Language.Parrot.Syntax
import qualified Poets.Reporting.ReportMonad as RM
import Poets.Reporting.ReportMonad (RExpr, reportTypeErrorThunk, reportTypeError, reportRuntimeError, deepEval2, eval, eval2, iThunk, dethunk, deepDethunk,liftTypeError)

import Control.Monad hiding (mapM, sequence)
import Prelude hiding (mapM, sequence)

import Text.Printf

import qualified Data.Map as Map

import Data.Traversable

type EValueSig = Val :+: ValueExt
type EValue = Term EValueSig
type ERValue = RExpr (Val :+: ValueExt)
type EReportFunction = [ERValue] -> ERValue

rChar :: Char -> ERValue
rChar = iVChar

rLeft, rRight :: ERValue -> ERValue
rLeft = iVLeft
rRight = iVRight

rTuple :: [ERValue] -> ERValue
rTuple = iVTuple

rUnit :: ERValue
rUnit = iVUnit


rProj :: (ValueExt :<: f) => Int -> RExpr f -> RExpr f
rProj pos = eval $ \ tup ->
  case proj tup of
    Just (VTuple es) -> doProj pos es
        where doProj 1 (x:_) = x
              doProj n (_:xs) = doProj (n-1) xs
              doProj _ []    = reportTypeErrorThunk $ "Cannot project " ++ show (length es)
                              ++ "-tuple to " ++ show pos ++ "-th element"
    _ -> reportTypeErrorThunk "projections can only be applied to product type values!"


rCase :: ERValue -> (ERValue -> ERValue) -> (ERValue -> ERValue) -> ERValue
rCase valm left right = (`eval` valm) $ \ val ->
  case proj val of
    Just (VLeft l) -> left l
    Just (VRight r) -> right r
    _ -> reportTypeErrorThunk "the case function can only be applied to sum type values"


rCharList :: String -> ERValue
rCharList = iVList . map (iVChar)

-- retypings --


rEvents :: ERValue
rEvents = RM.rEvents


rTypeOf ::  ERValue -> [(String ,ERValue)] -> ERValue -> ERValue
rTypeOf = RM.rTypeOf

rTypeOf' :: ERValue -> [(String ,ERValue)] -> ERValue
rTypeOf' = RM.rTypeOf'


rRefTypeOf ::  ERValue -> [(String ,ERValue)] -> ERValue -> ERValue
rRefTypeOf = RM.rRefTypeOf

rRefTypeOf' :: ERValue -> [(String ,ERValue)] -> ERValue
rRefTypeOf' = RM.rRefTypeOf'

rHasType :: ERValue -> String -> ERValue
rHasType = RM.rHasType

rString :: String -> ERValue
rString = RM.rString


rError :: ERValue -> ERValue
rError strR = iThunk $ do
  strE <- dethunk strR
  case proj strE of 
    Just (VString str) -> RM.reportUserError str
    Just (VList list)  -> mapM toChar list >>= RM.reportUserError
      where toChar e = case project e of
                         Just (VChar c) -> return c
                         _ -> RM.reportTypeError "The argument of error must be a string!"
    _ -> RM.reportTypeError "The argument of error must be a string!"

rRecord :: String -> [(String, ERValue)] -> ERValue
rRecord = RM.rRecord

rList :: [ERValue] -> ERValue
rList = RM.rList

rCons :: ERValue-> ERValue -> ERValue
rCons = RM.rCons

rFieldLookup :: ERValue -> String -> ERValue
rFieldLookup rec name = iThunk $ do
  vrec <- dethunk rec
  case proj vrec of
    Just (VRecord rec') -> 
        liftTypeError $ lookupField name rec'
    Just (VDuration dur) -> case Map.lookup name durationFieldMap of
      Nothing -> reportTypeError $ "durations do not have a field \"" ++ name ++ "\""
      Just field -> return $ iVInt $ case field of
        DurSec -> durationSeconds dur
        DurMin -> durationMinutes dur
        DurHour -> durationHours dur
        DurDay -> durationDays dur
        DurWeek -> durationWeeks dur
        DurMon -> durationMonths dur
        DurYear -> durationYears dur
    Just (VDateTime dt) 
        | name == "date" -> return $ iVDate $ fst $ dateTimeToDateTime dt
        | name == "time" -> return $ iVTime $ snd $ dateTimeToDateTime dt
        | otherwise -> case Map.lookup name dateTimeFieldMap of
          Nothing -> reportTypeError $ "date time values do not have a field \"" ++ name ++ "\""
          Just field -> let (y,mon,day,h,m,s,_) = dateTimeToComponents dt
                        in return $ iVInt $ case field of
                          TimeField f -> case f of
                            TimeSec -> s
                            TimeMin -> m
                            TimeHour -> h
                          DateField f -> case f of
                            DateDay -> day
                            DateMon -> mon
                            DateYear -> fromInteger y
    Just (VDate date) -> case Map.lookup name dateFieldMap of
          Nothing -> reportTypeError $ "date values do not have a field \"" ++ name ++ "\""
          Just field -> let (y,mon,day) = dateToYearMonthDay date
                        in return $ iVInt $ case field of
                          DateDay -> day
                          DateMon -> mon
                          DateYear -> fromInteger y
    Just (VTime time) -> case Map.lookup name timeFieldMap of
          Nothing -> reportTypeError $ "time values do not have a field \"" ++ name ++ "\""
          Just field -> let (h,m,s,_) = timeToHourMinSecMicroSec time
                        in return $ iVInt $ case field of
                          TimeSec -> s
                          TimeMin -> m
                          TimeHour -> h
    _ -> do v <- deepDethunk rec
            reportTypeError $ "record fields can only be accessed on record values!\n\
                           \field: "++ name ++ "\n\ 
                           \value: "++ show v

rFieldsMod :: ERValue -> [(String, ERValue)] -> ERValue
rFieldsMod rec mods = foldl appMod rec mods
    where appMod rec (field,new) = rFieldMod rec field new

rFieldMod :: ERValue -> String -> ERValue -> ERValue
rFieldMod rec name newVal = iThunk $ do
  vrec <- dethunk rec
  case proj vrec of
    Just (VRecord rec') -> 
        liftTypeError $ liftM iVRecord $ updateField name newVal rec'
    Just (VDuration dur) -> case Map.lookup name durationFieldMap of
      Nothing -> reportTypeError $ "durations do not have a field \"" ++ name ++ "\""
      Just field -> do val <- dethunk newVal
                       case proj val of
                         Just (VInt i) -> return $ iVDuration $ case field of
                           DurSec -> dur {durationSeconds = i}
                           DurMin -> dur {durationMinutes = i}
                           DurHour -> dur {durationHours = i}
                           DurDay -> dur {durationDays = i}
                           DurWeek -> dur {durationWeeks = i}
                           DurMon -> dur {durationMonths = i}
                           DurYear -> dur {durationYears = i}
                         _  ->  reportTypeError $ 
                                "the field \""++ name ++"\" of duration values has type Int"
    Just (VDateTime dt)
        | name == "date" -> do 
          val <- dethunk newVal
          case proj val of
            Just (VDate date) -> return $ iVDateTime $ 
                                 createDateTime date (snd $ dateTimeToDateTime dt)
            _ -> reportTypeError "the field \"date\" of datetime values has type Int"
        | name == "time" -> do 
          val <- dethunk newVal
          case proj val of
            Just (VTime time) -> return $ iVDateTime $
                                 createDateTime (fst $ dateTimeToDateTime dt) time
            _ -> reportTypeError "the field \"date\" of datetime values has type Int"
        | otherwise -> case Map.lookup name dateTimeFieldMap of
          Nothing -> reportTypeError $ "datetime values do not have a field \"" ++ name ++ "\""
          Just field -> do 
            val <- dethunk newVal
            case proj val of 
              Just (VInt i) ->
                let (y,mon,day,h,m,s,msec) = dateTimeToComponents dt
                    mk y mon day h m s = case createDateTime' y mon day h m s msec of
                      Nothing -> reportRuntimeError $ printf
                                 "\"%d-%02d-%02d %02d:%02d:%02d\" is not a well-formed date"
                                 y mon day h m s
                      Just dt' -> return $ iVDateTime dt'
                in case field of
                  TimeField f -> case f of
                    TimeSec -> mk y mon day h m i
                    TimeMin -> mk y mon day h i s
                    TimeHour -> mk y mon day i m s
                  DateField f -> case f of
                    DateDay -> mk y mon i h m s
                    DateMon -> mk y i day h m s
                    DateYear -> mk (toInteger i) mon day h m s
              _  ->  reportTypeError $ "the field \""++ name ++"\" of datetime values has type Int"
    Just (VDate date) -> case Map.lookup name dateFieldMap of
      Nothing -> reportTypeError $ "date values do not have a field \"" ++ name ++ "\""
      Just field -> do 
        val <- dethunk newVal
        case proj val of 
          Just (VInt i) -> 
            let (y,mon,day) = dateToYearMonthDay date
                mk y mon d = case createDate y mon d of 
                  Nothing -> reportRuntimeError $ printf
                             "\"%d-%02d-%02d\" is not a well-formed date" y mon d
                  Just date -> return $ iVDate date
            in case field of
              DateDay -> mk y mon i
              DateMon -> mk y i day
              DateYear -> mk (toInteger i) mon day
          _  ->  reportTypeError $ "the field \""++ name ++"\" of date values has type Int"
    Just (VTime time) -> case Map.lookup name timeFieldMap of
      Nothing -> reportTypeError $ "time values do not have a field \"" ++ name ++ "\""
      Just field -> do 
        val <- dethunk newVal
        case proj val of 
          Just (VInt i) -> 
            let (h,m,s,ms) = timeToHourMinSecMicroSec time
                mk h m s = case createTime  h m s ms of 
                  Nothing -> reportRuntimeError $ printf
                             "\"%02d:%02d:%02d\" is not a well-formed date" h m s
                  Just time' -> return $ iVTime time'
            in case field of
              TimeSec -> mk h m i
              TimeMin -> mk h i s
              TimeHour -> mk i m s
          _  ->  reportTypeError $ "the field \""++ name ++"\" of time values has type Int"
    _ -> reportTypeError "The set-field operator <~ is only allowed on record values!"

rFoldr :: (Val :<: f) => (RExpr f -> RExpr f -> RExpr f) ->  RExpr f ->  RExpr f -> RExpr f
rFoldr = RM.rFoldr

rIf :: ERValue -> ERValue -> ERValue -> ERValue
rIf = RM.rIf

rTrue, rFalse :: ERValue
rTrue = RM.rTrue
rFalse = RM.rFalse


rEq, rNeq :: ERValue -> ERValue -> ERValue
rEq = deepEval2 $ \ x y -> iVBool (x == y)
rNeq = deepEval2 $ \ x y -> iVBool (x /= y)

rLt, rGt, rLte, rGte :: ERValue -> ERValue -> ERValue
rLt = rCompare "<" (iVBool . (== LT))
rLte = rCompare "<=" (iVBool . (/= GT))
rGt = rCompare ">" (iVBool . (== GT))
rGte = rCompare ">=" (iVBool . (/= LT))
rMax, rMin :: ERValue -> ERValue -> ERValue
rMax x y = rCompare "max" (\ord -> if  ord == LT then y else x) x y
rMin x y = rCompare "min" (\ord -> if  ord == GT then y else x) x y



{-|
  This is an auxiliary function for defining the comparison operators.
-}
rCompare :: String -> (Ordering -> ERValue) -> ERValue -> ERValue -> ERValue
rCompare opname cont = eval2 $ \ d e -> case (proj d, proj e) of
    (Just d', Just e') -> case (d',e') of
      (VInt i ,VInt j) -> cont $ compare i j
      (VInt i, VReal j) -> cont $ compare (fromIntegral i) j
      (VReal i, VInt j) -> cont $ compare i (fromIntegral j)
      (VReal i, VReal j) -> cont $ compare i j
      (VDateTime i, VDateTime j) -> cont $ compare i j
      (VDuration i, VDuration j) -> cont $ compare i j
      (VList xs, VList ys) -> lCompare xs ys
      (VBool x, VBool y) -> cont $ compare x y
      (VRecord VR{vrecordName = n1, vrecordFields = f1},
       VRecord VR{vrecordName = n2, vrecordFields = f2}) ->
              case compare n1 n2 of
                EQ -> fCompare f1 f2
                ord -> cont ord
      (VEnt VEntity{ventId = id}, VEnt VEntity{ventId = id'}) -> cont $ compare id id'
      _ -> compErr
    _ -> case (proj d, proj e) of
      (Just d', Just e') -> case (d',e') of
        (VLeft x, VLeft y) -> rCompare opname cont x y
        (VLeft _, VRight _) -> cont LT
        (VRight x, VRight y) -> rCompare opname cont x y
        (VRight _, VLeft _) -> cont GT
        (VChar c, VChar d) -> cont $ compare c d
        (VTuple x, VTuple y) -> lCompare x y
        (VUnit , VUnit) -> cont EQ
        _ -> compErr
      _ -> compErr  
  where compErr = reportTypeErrorThunk $ "The given values cannot be compared using " ++ opname
        lCompare [] [] = cont EQ
        lCompare [] _  = cont LT
        lCompare _ []  = cont GT
        lCompare (x:xs) (y:ys) = rCompare opname
                                 (\ ord -> case ord of EQ -> lCompare xs ys
                                                       _ -> cont ord) 
                                 x y
        fCompare f1 f2 = fsComp fs1 fs2
            where fs1 = fieldsSorted f1
                  fs2 = fieldsSorted f2
                  fsComp [] [] = cont EQ
                  fsComp [] _ = cont LT
                  fsComp _ [] = cont GT
                  fsComp (f1:fs1) (f2:fs2) = rCompare opname 
                                             (\ord -> case ord of
                                                        EQ -> fsComp fs1 fs2
                                                        _ ->  cont ord)
                                             (vfieldValue f1) (vfieldValue f2)


rAnd, rOr :: ERValue -> ERValue -> ERValue
rAnd = RM.rAnd
rOr = RM.rOr

rNot :: ERValue -> ERValue
rNot = RM.rNot


rPlus, rMinus, rTimes :: ERValue -> ERValue -> ERValue
rPlus = RM.rPlus
rMinus = RM.rMinus
rTimes = RM.rTimes

rAbs, rSignum, rNegate :: ERValue -> ERValue
rAbs = RM.rAbs
rSignum = RM.rSignum
rNegate = RM.rNegate

rInteger :: Integer -> ERValue
rInteger = RM.rInteger


rRational :: Rational -> ERValue
rRational = RM.rRational

rRecip :: ERValue -> ERValue
rRecip = RM.rRecip


rDiv :: ERValue -> ERValue -> ERValue
rDiv = RM.rDiv

rDurPlus :: ERValue -> ERValue -> ERValue
rDurPlus = RM.rDurPlus

rDurMinus :: ERValue -> ERValue -> ERValue
rDurMinus = RM.rDurMinus

rRef :: String -> EntId -> ERValue
rRef name id = iVEnt $ VEntity name id Nothing

rDuration :: VDuration ERValue -> ERValue
rDuration = RM.rDuration

rDuration' :: ERValue -> ERValue -> ERValue -> ERValue -> ERValue -> ERValue -> ERValue -> ERValue
rDuration' s min h d w m y = RM.rDuration $ VD s min h d w m y

rTime :: ERValue -> ERValue -> ERValue -> ERValue
rTime = RM.rTime

rDate :: ERValue -> ERValue -> ERValue -> ERValue
rDate = RM.rDate

rDateTime :: ERValue -> ERValue -> ERValue -> ERValue -> ERValue -> ERValue -> ERValue
rDateTime = RM.rDateTime

rCompDateTime :: ERValue -> ERValue -> ERValue
rCompDateTime = RM.rCompDateTime

rDerefNow :: ERValue -> ERValue
rDerefNow = RM.rDerefNow
rDerefCxt :: ERValue -> ERValue
rDerefCxt = RM.rDerefCxt