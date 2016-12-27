{-# LANGUAGE
  TypeOperators,
  FlexibleContexts,
  GeneralizedNewtypeDeriving,
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeSynonymInstances
  #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.FreshVar
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides facilities to generate fresh (type)
-- variables. This functionality is encapsulated in the 'FreshVarT'
-- monad transformer and the 'MonadFreshVar' monad type class.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.FreshVar
    ( MonadFreshVar(..)
    , FreshVarT
    , runFreshVarT
    , freshVars
    , freshTVar
    , freshTVars  
    , freshVVar
    , freshVVars  
    ) where


import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.TypingMonad

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Equivalence.Monad

{-|
  This is the type of the state of the 'FreshVarT' monad transformer.
-}
type FreshVarState = Int

{-| This defines a monad transformer that provides a means to generate
fresh variables.  -}

newtype FreshVarT m a = FreshVarT { unFreshVarT :: StateT FreshVarState m a}
    deriving (Monad, Functor)

{-|
  This function runs the given fresh variable computation
-}

runFreshVarT :: (Monad m) => FreshVarT m a -> m a
runFreshVarT (FreshVarT m) = liftM fst $ (`runStateT` 0) m

{-| This defines a class of monads that provide an operation to
generate fresh variables.  -}

class (VarId a, Monad m) => MonadFreshVar a m where
    {-| This function generates a fresh (type or expression) 
      variable. -}
    freshVar :: m a

freshVars :: MonadFreshVar a m => Int -> m [a]
freshVars n = replicateM n freshVar

instance MonadTrans FreshVarT where
    lift = FreshVarT . lift 

instance Monad m => MonadFreshVar TVarId (FreshVarT m) where
    freshVar = FreshVarT $ do
      i <- get
      put (i+1)
      return $ TVarId "#" i

instance Monad m => MonadFreshVar VVarId (FreshVarT m) where
    freshVar = FreshVarT $ do
      i <- get
      modify (+1)
      return $ VVarId "_x" i


instance (MonadTyping m) => MonadTyping (FreshVarT m) where
    withPos pos (FreshVarT m)   =  FreshVarT (withPos pos m)
    catchTyErr (FreshVarT m) f = FreshVarT (catchTyErr m (unFreshVarT . f))
    liftTy = lift . liftTy


instance (MonadFreshVar a m, Monoid l) => MonadFreshVar a (WriterT l m) where
    freshVar = lift freshVar

instance (MonadFreshVar a m) => MonadFreshVar a (StateT s m) where
    freshVar = lift freshVar

instance (MonadFreshVar a m) => MonadFreshVar a (ReaderT r m) where
    freshVar = lift freshVar

instance (MonadFreshVar a m) => MonadFreshVar a (EquivT s c v m) where
    freshVar = lift freshVar

{-| This function is similar to 'freshVar' but instead of a plain
variable identifier it returns a type variable as an expression.  -}

freshTVar :: (PTypeVar  :<: f, MonadFreshVar TVarId m) => m (Cxt h f a)
freshTVar = liftM (inject . TVar) freshVar

{-| This function generates a list of @n@ fresh type variables, where
@n@ is the integer argument provided to this function (see 'freshTVar'). -}

freshTVars :: ((PTypeVar)  :<: f, MonadFreshVar TVarId  m)
           => Int -> m [Cxt h f a]
freshTVars n = replicateM n freshTVar

{-| This function is similar to 'freshVar' but instead of a plain
variable identifier it returns an expression variable as an expression.  -}

freshVVar :: ((ValueExt :&: SrcPos)  :<: f, MonadFreshVar VVarId m) => m (Cxt h f a)
freshVVar = liftM (inject . (:&: (Nothing :: SrcPos)) . VVar) freshVar

{-| This function generates a list of @n@ fresh variables, where @n@
is the integer argument provided to this function (see 'freshVVar'). -}

freshVVars :: ((ValueExt :&: SrcPos)  :<: f, MonadFreshVar VVarId m) => Int -> m [Cxt h f a]
freshVVars n = sequence $ replicate n freshVVar
