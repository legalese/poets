{-# LANGUAGE
  TypeOperators,
  ImplicitParams, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Compiler.BasicValue
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides functionality to convert Parrot values into
-- basic POETS values and vice versa.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Compiler.BasicValue 
    ( fromBasicValue
    , toBasicValue
    , toBasicReportFunction)
    where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Compiler.Interface
import Poets.Reporting.Report
import Poets.Data.Render

import Poets.Data.Type.Utils

import Prelude hiding (mapM, sequence)
import Control.Monad hiding (mapM, sequence)

import Data.Traversable
import Data.Comp.Ops

toBasicReportFunction :: POETSRecordEnv -> ReportType -> EReportFunction -> ReportFunction
toBasicReportFunction env ReportType{reportRetType = retType} ef args = 
    liftRM fromBasicValue $ do
      res <- deepDethunk (ef $ map fromBasicValue args)
      let ?recEnv = env
      case toBasicValue retType res of
        Left msg -> throwError $ ReportTypeError $
          "result value of report function cannot be transformed into a basic value\nreason:" ++ msg
        Right res' -> return res'

fromBasicValue :: (FromBasicValueAlg f, Val :<: g, ValueExt :<: g) => Term f -> Term g
fromBasicValue = cata fromBasicValueAlg

class (Functor f) => FromBasicValueAlg f where
    fromBasicValueAlg :: (Val :<: g, ValueExt :<: g) => f (Term g) -> Term g

instance (FromBasicValueAlg f, FromBasicValueAlg g) => FromBasicValueAlg (f :+: g) where
    fromBasicValueAlg (Inl v) = fromBasicValueAlg v
    fromBasicValueAlg (Inr v) = fromBasicValueAlg v

instance FromBasicValueAlg Val where
    fromBasicValueAlg (VString s) = inject $ VList $ map (inject . VChar) s
    fromBasicValueAlg v = inject v

instance FromBasicValueAlg ValueExt where
    fromBasicValueAlg = inject

toBasicValue :: (ToBasicValueAlg f, Functor f, ?recEnv :: POETSRecordEnv)
             => Type -> Term f -> Either String Value
toBasicValue typ (Term v) = toBasicValueAlg typ v

toChar :: (ToBasicValueAlg f, Functor f) => Term f -> Either String Char
toChar (Term v) = toCharAlg v

class Render f => ToBasicValueAlg f where
    toBasicValueAlg :: (ToBasicValueAlg g, Functor g, ?recEnv :: POETSRecordEnv)
                    => Type -> f (Term g) -> Either String Value
    toCharAlg :: (Show a) => f a -> Either String Char

instance (ToBasicValueAlg f, ToBasicValueAlg g) => ToBasicValueAlg (f :+: g) where
    toBasicValueAlg typ (Inl v) = toBasicValueAlg typ v
    toBasicValueAlg typ (Inr v) = toBasicValueAlg typ v

    toCharAlg (Inl v) = toCharAlg v
    toCharAlg (Inr v) = toCharAlg v

instance ToBasicValueAlg Val where
    toBasicValueAlg typ (VList l) =
        case project typ of
          Just (TList typ') -> liftM (inject . VList) (mapM (toBasicValue typ') l)
          _ -> case project typ of
                 Just TString -> liftM (inject . VString) (mapM toChar l)
                 _ -> Left $ "value of type " ++ show typ ++ " cannot be coerced into a basic value"
    toBasicValueAlg typ (VRecord rec@VR{vrecordName = rname, vrecordFields = rfields}) =
                    case project typ of
                      Just (TRecord rname') -> do
                                       issub <- isSubType ?recEnv rname rname'
                                       unless issub $
                                              Left $ "trying to coerce a record value of type '"++ show rname ++"' to a record value of type '" ++ show typ ++ "' which is not a super type"
                                       fieldTypes <- getTypeFields ?recEnv rname
                                       let fieldTypes' = fieldEnvSorted fieldTypes
                                           rfields' = fieldsSorted rfields
                                       newRfields <- zipWithM run fieldTypes' rfields'
                                       return $ inject $ VRecord $
                                              rec {vrecordFields = newFields newRfields}
                              where run ft f
                                        | fieldName ft == vfieldName f
                                            = do
                                            val <- toBasicValue (fieldType ft) (vfieldValue f)
                                            return f { vfieldValue = val}
                                        | otherwise = Left $ "error while mapping the field values for record type '" ++ show rname ++ "'; field names do not match"
                      _ -> Left $ "trying to coerce a record value of type '"++ show rname ++"' but target type is '" ++ show typ ++ "'"
        
    toBasicValueAlg typ v = liftM inject $ mapM (toBasicValue typ) v

    toCharAlg v = Left $ "expected a character value but found " ++ showF v

instance ToBasicValueAlg ValueExt where
    toBasicValueAlg _ _ = Left "cannot coerce Parrot extended values to basic values"

    toCharAlg (VChar c) = return c
    toCharAlg v = Left $ "expected a character value but found " ++ showF v