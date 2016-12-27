{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts, 
    MultiParamTypeClasses, TypeSynonymInstances, TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Michael Werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides facilities to decompose atomic type terms into 
-- simple data types  
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp
    ( ConstSym(..)
    , AtomicType(..)
    , AtomicTypePos
    , atomicDecomp
    , atomicRecomp
    , atomicVar
    ) where

import Poets.Reporting.Language.Parrot.FreeVars
import Poets.Reporting.Language.Parrot.Typing.Decomp (ConstSym(..), AtomicType(..))
import Poets.Reporting.Language.Parrot.Syntax

import Control.Monad
import Data.Comp.Derive
import qualified Data.Set as Set

type AtomicTypePos = (AtomicType, SrcPos) 

{-| This class defines an algebra for decomposing atomic terms -}

class (Functor f) => AtomicDecomp f r where
    atomicDecompAlg :: f a -> r

instance AtomicDecomp PTypeVar AtomicType where
    atomicDecompAlg (TVar var) = AVar var

instance AtomicDecomp PTypeConst AtomicType where
    atomicDecompAlg TDurationDate = AConst SDurationDate
    atomicDecompAlg TChar = AConst SChar
    atomicDecompAlg TUnit = AConst SUnit

instance AtomicDecomp TypeConstant AtomicType where
    atomicDecompAlg TInt = AConst SInt
    atomicDecompAlg TBool = AConst SBool
    atomicDecompAlg TString = AConst SString
    atomicDecompAlg TTime = AConst STime
    atomicDecompAlg TDate = AConst SDate
    atomicDecompAlg TDateTime = AConst SDateTime
    atomicDecompAlg TDuration = AConst SDuration
    atomicDecompAlg TReal = AConst SReal
    atomicDecompAlg (TRecord name) = AConst $ SRecord name

$(derive [liftSum] [''AtomicDecomp])

instance (AtomicDecomp f AtomicType) => 
         AtomicDecomp (f :&: SrcPos) AtomicType where
    atomicDecompAlg (t :&: _) = atomicDecompAlg t

instance (AtomicDecomp f AtomicType) => 
         AtomicDecomp (f :&: SrcPos) AtomicTypePos where
    atomicDecompAlg (t :&: pos) = (atomicDecompAlg t, pos)

atomicDecomp :: AtomicDecomp f r => Term f -> r
atomicDecomp (Term t) = atomicDecompAlg t

atomicVar :: (AtomicDecomp f AtomicType, MonadPlus m) => (Term f) -> m TVarId
atomicVar t = do
  AVar v <- return $ atomicDecomp t
  return v

{-| This class defines a function for recomposing values back to atomic terms -}

class AtomicRecomp a where
  atomicRecomp :: (a, SrcPos) -> AtomicPType

instance AtomicRecomp TVarId where
  atomicRecomp (tvar, pos) = inject $ TVar tvar :&: pos

instance AtomicRecomp ConstSym where
  atomicRecomp (SDurationDate, pos) = inject $ TDurationDate :&: pos
  atomicRecomp (SChar, pos) = inject $ TChar :&: pos
  atomicRecomp (SUnit, pos) = inject $ TUnit :&: pos
  atomicRecomp (SInt, pos) = inject $ TInt :&: pos
  atomicRecomp (SBool, pos) = inject $ TBool :&: pos
  atomicRecomp (SString, pos) = inject $ TString :&: pos
  atomicRecomp (SDateTime, pos) = inject $ TDateTime :&: pos
  atomicRecomp (SDate, pos) = inject $ TDate :&: pos
  atomicRecomp (STime, pos) = inject $ TTime :&: pos
  atomicRecomp (SDuration, pos) = inject $ TDuration :&: pos
  atomicRecomp (SReal, pos) = inject $ TReal :&: pos
  atomicRecomp (SRecord name, pos) = inject $ TRecord name :&: pos

instance AtomicRecomp AtomicType where
  atomicRecomp (AVar var, pos) = atomicRecomp (var, pos)
  atomicRecomp (AConst symbol, pos) = atomicRecomp (symbol, pos)
  
  
instance FreeVars (AtomicTypeConstr a) TVarId where 
  freeVars (ASubType s t) = freeVars s `Set.union` freeVars t
  freeVars (AHasField s _ t) = freeVars s `Set.union` freeVars t
  freeVars (AOrd s) = freeVars s
  freeVars (AEq s) = freeVars s
  
instance FreeVars ((AtomicTypeConstr :&: SrcPos) a) TVarId where 
  freeVars (c :&: _) = freeVars c
