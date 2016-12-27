{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, IncoherentInstances, TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.FreeVars
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides a facility to compute the set of free
-- variables of certain constructs.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.FreeVars
    (
     FreeVars(..),
     BoundVars(..),
     someFreeVarsIn
    ) where

import Poets.Reporting.Language.Parrot.Syntax

import Data.Traversable
import Data.Foldable
import Data.Comp.Variables

import qualified Data.Set as Set
import Data.Set (Set)

import Prelude hiding (foldl)

instance HasVars f v => HasVars (f :&: SrcPos) v where
    isVar (v :&: _) = isVar v

instance HasVars PTypeFun TVarId where
    isVar _ = Nothing

instance HasVars PTypeConst TVarId where
    isVar _ = Nothing

instance HasVars TypeEnt TVarId where
    isVar _ = Nothing

instance HasVars TypeList TVarId where
    isVar _ = Nothing

instance HasVars TypeConstant TVarId where
    isVar _ = Nothing

instance HasVars PTypeVar TVarId where
    isVar (TVar v) = Just v

class (Ord v) => FreeVars a v where
    freeVars :: a -> Set v

instance (Ord v, HasVars f v, Traversable f) => FreeVars (Term f) v where
    freeVars = variables

instance (Ord v, HasVars f v, Traversable f) => FreeVars (Const f) v where
    freeVars = variables'

instance (FreeVars a v, Functor g, Foldable g) => FreeVars (g a) v where
    freeVars = foldl (\s x -> s `Set.union` freeVars x) Set.empty 


instance FreeVars TypeScheme TVarId where
    freeVars _ = Set.empty


someFreeVarsIn :: (FreeVars a v) => Set v -> a -> Maybe v
someFreeVarsIn s a = case Set.toList $ Set.intersection s (freeVars a) of
                       [] -> Nothing
                       x:_ -> Just x

class (Ord v) => BoundVars a v where
    boundVars :: a -> Set v


instance BoundVars TypeScheme TVarId where
    boundVars (TypeScheme _ cs ty) = freeVars cs `Set.union` freeVars ty
