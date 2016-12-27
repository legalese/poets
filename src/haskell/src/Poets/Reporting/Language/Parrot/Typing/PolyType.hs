{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.PolyType
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- 
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.PolyType  where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.FreeVars

import Data.Set (Set)
import qualified Data.Set as Set



data PolyType = PolyType {
      ptForall :: Set TVarId,
      ptConstrs :: [TypeConstrPos PType],
      ptType :: PType
    }


instance FreeVars PolyType TVarId where
    freeVars (PolyType univ cs ty) = (freeVars cs `Set.union` freeVars ty) `Set.difference` univ


instance BoundVars PolyType TVarId where
    boundVars PolyType {ptForall = univ} = univ

mkMonoType :: PType -> PolyType
mkMonoType s = PolyType Set.empty [] s

fromTypeScheme :: TypeScheme -> PolyType
fromTypeScheme (TypeScheme _ cs ty) = PolyType (freeVars ty) cs ty  

fromToplevelTyping :: (FunId, TypeScheme) -> (VVarId, PolyType)
fromToplevelTyping (funid, ty) = (vVarId funid, fromTypeScheme ty)