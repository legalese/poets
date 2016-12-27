{-# LANGUAGE
  TypeOperators,
  FlexibleInstances,
  ScopedTypeVariables,
  FlexibleContexts, 
  PatternGuards,
  CPP #-}

-- hack around a compiler bug of GHC 7.2.1 / 7.4
#if __GLASGOW_HASKELL__ == 702 || __GLASGOW_HASKELL__ == 704 || __GLASGOW_HASKELL__ == 706
{-# OPTIONS_GHC -O0 #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.BasicType
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides functionality to convert Parrot types into
-- basic POETS types.
--
--------------------------------------------------------------------------------
module Poets.Reporting.Language.Parrot.Typing.BasicType
    (toReportType,
    fromBasicType,
    fromBasicType') where


import Poets.Reporting.Language.Parrot.Typing.TypingMonad
import Poets.Reporting.Language.Parrot.Typing.Decomp
import Poets.Reporting.Language.Parrot.Typing.TypeAlias
import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Report

import Poets.Data.Render

import Prelude hiding (mapM)

import Control.Monad.Reader hiding (mapM)

import Data.Traversable
import Data.Comp.Ops



toReportType :: POETSRecordEnv -> TypeScheme -> Either TypingErr ReportType
toReportType recEnv ty = runTypingM $ runReaderT (addErrMsg msg $ toReportTypeM ty) recEnv
    where msg = "type '" ++ show ty ++ "' is not a basic type"

type RTM = ReaderT POETSRecordEnv TypingM 

getRecEnv :: RTM POETSRecordEnv
getRecEnv = ask

toReportTypeM :: TypeScheme -> RTM ReportType
toReportTypeM (TypeScheme pos constr ty) = withPos' pos $
   if constr == []
   then do
     (args,res) <- decompFunType ty
     liftM2 ReportType (mapM toBasicType args) (toBasicType res)
   else typeErr "type contains constraints"

decompFunType :: PType -> RTM ([PType],PType)
decompFunType ty = 
    case decomp ty of
      DFun SFun [t1,t2] -> do
                     (tys,tyr) <- decompFunType t2
                     return (t1:tys,tyr)
      _ -> return ([], ty)


toBasicType :: (ToBasicTypeAlg f) => Term f -> RTM Type
toBasicType = toBasicTypeAlg . unTerm

class (Traversable f, TermStr f, Render f) => ToBasicTypeAlg f where
    toBasicTypeAlg :: (ToBasicTypeAlg g) => f (Term g) -> RTM Type
    toBasicTypeAlg t = typeErr $ "type '" ++ showF t ++ "' is not a basic type"

instance (ToBasicTypeAlg f, ToBasicTypeAlg g) => ToBasicTypeAlg (f :+: g) where
    toBasicTypeAlg (Inl v) = toBasicTypeAlg v
    toBasicTypeAlg (Inr v) = toBasicTypeAlg v

instance (ToBasicTypeAlg f) => ToBasicTypeAlg (f :&: SrcPos) where
    toBasicTypeAlg (v :&: pos) = withPos' pos (toBasicTypeAlg v)


instance ToBasicTypeAlg TypeConstant where
    toBasicTypeAlg (TRecord name) = do 
          env <- getRecEnv
          if isDefined env name 
            then return $ inject $ TRecord name
            else typeErr $ "record type '" ++ name ++ "' is not a basic record type"
    toBasicTypeAlg t = liftM inject $ mapM toBasicType t

instance ToBasicTypeAlg TypeList where
    toBasicTypeAlg (TList t)
        | DConst SChar <- decomp t = return $ inject TString
    toBasicTypeAlg t = liftM inject $ mapM toBasicType t

instance ToBasicTypeAlg TypeEnt where
    toBasicTypeAlg t = liftM inject $ mapM toBasicType t

instance ToBasicTypeAlg PTypeFun where
instance ToBasicTypeAlg PTypeConst where
instance ToBasicTypeAlg PTypeVar where

fromBasicType :: Type -> PType
fromBasicType = resolveAlias . deepInject3 . addPos
    where addPos = ann Nothing :: Type -> Term (TypeConstant :&: SrcPos :+: TypeList :&: SrcPos :+: TypeEnt :&: SrcPos)

fromBasicType' :: (Functor f) => f Type -> f PType
fromBasicType' = fmap fromBasicType 