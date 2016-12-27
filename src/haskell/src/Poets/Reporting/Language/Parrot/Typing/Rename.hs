{-# LANGUAGE
  TypeOperators,
  FlexibleInstances,
  FlexibleContexts,
  TypeSynonymInstances,
  IncoherentInstances #-}


--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Rename
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This modules provides function to rename the variables in a type
-- expression using fresh variables.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Rename
    (
     Rename(..),
     rename',
     renameExcept
    ) where


import Poets.Data.Type
import Poets.Reporting.Language.Parrot.FreshVar
import Poets.Reporting.Language.Parrot.Syntax

import Prelude hiding (sequence, mapM)

import Control.Monad.Reader hiding (sequence, mapM)
import Control.Monad.State hiding (sequence, mapM)

import Data.Comp.Ops
import Data.Traversable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

{-| This type represents a renaming of type variables.  -}

type VarRename = Map TVarId TVarId

{-| This type represents sets of bound variables.  -}

type BoundVars = Set TVarId

{-| This monad transformer is used to perform the renaming. -}

type RenameT m = ReaderT BoundVars (StateT VarRename m)

{-| This class represents the algebra for renaming type expressions.
-}

class Traversable f => RenameAlg f where
    renameAlg :: (MonadFreshVar TVarId m) => f a -> RenameT m (f a)

instance RenameAlg TypeConstant where
    renameAlg = return

instance RenameAlg TypeList where
    renameAlg = return

instance RenameAlg TypeEnt where
    renameAlg = return


instance RenameAlg PTypeConst where
    renameAlg = return

instance RenameAlg PTypeFun where
    renameAlg = return

instance RenameAlg PTypeVar where
    renameAlg ty@(TVar var) = do
      bound <- ask
      if Set.member var bound
        then return ty
        else do
          ren <- get
          case Map.lookup var ren of
            Nothing -> do 
               var' <- freshVar
               put $ Map.insert var var' ren
               return $ TVar var'
            Just var' -> return $ TVar var'

instance (RenameAlg f) => RenameAlg (f :&: SrcPos) where
    renameAlg (v :&: p) = liftM (:&: p) $ renameAlg v

instance (RenameAlg f, RenameAlg g) => RenameAlg (f :+: g) where
    renameAlg (Inl v) = liftM Inl $ renameAlg v
    renameAlg (Inr v) = liftM Inr $ renameAlg v

{-| Instances of this class are type expressions that allow for
renaming their free variables.  -}

class Rename a where
    {-|
      This function renames the free variables in the given object.
     -}
    rename :: (MonadFreshVar TVarId m) => a -> RenameT m a


instance (RenameAlg f) => Rename (Term f) where
    rename = appSigFunM renameAlg

instance (RenameAlg f) => Rename (Const f) where
    rename = renameAlg


instance (Rename a, Traversable f) => Rename (f a) where
    rename = mapM rename

instance (Rename a, Rename b) => Rename (a,b) where
    rename (x, y) = liftM2 (,) (rename x) (rename y)

instance Rename TypeScheme where
    rename (TypeScheme p cs ty) = liftM2 (TypeScheme p) (rename cs) (rename ty)




{-| This function renames the free variables in the given object.
-}

rename' :: (Rename a, MonadFreshVar TVarId m) => a -> m a
rename' = renameExcept Set.empty


{-| This function renames the free variables in the given object but
considers the variables provided as an argument as bound.  -}

renameExcept :: (Rename a, MonadFreshVar TVarId m) => Set TVarId -> a -> m a
renameExcept vars x = liftM fst . (`runStateT` Map.empty) . (`runReaderT` vars) $ rename x