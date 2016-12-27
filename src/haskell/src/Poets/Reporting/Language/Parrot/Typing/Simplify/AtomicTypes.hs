{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeOperators, MultiParamTypeClasses, TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicTypes
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Michael werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- Utility module to handle atomic types.
--------------------------------------------------------------------------------


module Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicTypes where

import Data.Comp.Variables
import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Maybe
  
type ASubstitution = Subst AtomicTypeSigPos TVarId
type AConstraint a = AtomicTypeConstrPos a

idSubst = Map.empty
singleSubst = Map.singleton

fromAtomicSubst :: ASubstitution -> Subst TypeSigPos TVarId
fromAtomicSubst = Map.map fromAtomicTerm

fromAtomicTerm :: AtomicPType -> PType
fromAtomicTerm = deepInject3

toAtomicSubst :: Subst TypeSigPos TVarId -> ASubstitution
toAtomicSubst = Map.map (fromJust . toAtomicPType)

atomicCompSubst :: Subst AtomicTypeSigPos TVarId -> Subst AtomicTypeSigPos TVarId -> Subst AtomicTypeSigPos TVarId
atomicCompSubst s1 s2 = toAtomicSubst $ compSubst (fromAtomicSubst s1) (fromAtomicSubst s2)
--compSubst s1 s2 = fmap (appSubst s1) s2 `Map.union` s1


class AtomicApplySubst s a where
  atomicApplySubst :: s -> a -> a
  
instance AtomicApplySubst ASubstitution (AtomicTypeConstr a) where
  atomicApplySubst s (ASubType t1 t2) = ASubType (appSubst s t1) (appSubst s t2) 
  atomicApplySubst s (AHasField r n t) = 
    AHasField (appSubst s r) n (appSubst (fromAtomicSubst s) t) 
  atomicApplySubst s (AOrd t) = AOrd (appSubst s t) 
  atomicApplySubst s (AEq t) = AEq (appSubst s t) 
      
instance AtomicApplySubst ASubstitution (AtomicTypeConstrPos a) where
  atomicApplySubst s (c :&: pos) = atomicApplySubst s c :&: pos

instance (AtomicApplySubst s a) => AtomicApplySubst s [a] where
  atomicApplySubst s as = map (atomicApplySubst s) as 


class AtomicVariables a where
  atomicVariables :: a -> Map TVarId SrcPos
  
instance AtomicVariables AtomicPType where
  atomicVariables t = case atomicDecomp t of
    (AVar tvar, pos) -> Map.singleton tvar pos
    _ -> Map.empty

-- TODO: This instance does not extract the positions along with the 
-- type variables as this is currently not supported by the Decomp module.
instance AtomicVariables PType where
  atomicVariables t = Map.fromList $ map (\t -> (t, Nothing)) $ Set.toList $ variables t

instance AtomicVariables (AtomicTypeConstr a) where
  atomicVariables (ASubType s t) = atomicVariables s `Map.union` atomicVariables t 
  atomicVariables (AHasField r _ t) = atomicVariables r `Map.union` atomicVariables t 
  atomicVariables (AOrd t) = atomicVariables t
  atomicVariables (AEq t) = atomicVariables t

instance AtomicVariables (AConstraint a) where
  atomicVariables (c :&: _) = atomicVariables c
  
instance AtomicVariables a => AtomicVariables [a]  where
  atomicVariables as = Map.unions $ map atomicVariables as