{-# LANGUAGE TypeOperators, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Instance
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This modules provides a procedure for deciding whether a given type
-- scheme is an instance of another type scheme.
--
-- TODO: For now this is an ad-hoc hack. This needs a proper implementation!
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Instance
    (
     isInstance,
     isInstance'
    ) where
import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.FreshVar
import Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
import Poets.Reporting.Language.Parrot.Typing.Rename
import Poets.Reporting.Language.Parrot.Typing.TypingMonad
import Poets.Reporting.Language.Parrot.Typing.Simplify.Simplify
import Poets.Reporting.Language.Parrot.Typing.Simplify.Match
import Poets.Reporting.Language.Parrot.Typing.Entailment
import Poets.Reporting.Language.Parrot.FreeVars


import Data.Comp.Matching


{-| This function decides whether the second type scheme is an
instance of the first one. The first type scheme is supposed to be
simplified, i.e. in particular it is assumed that subtyping
constraints are non-circular! All type constraints (in both type
schemes) are supposed to be atomic; if not, a corresponding error is
signalled in the typing monad. -}

isInstance :: (MonadTyping m, MonadFreshVar TVarId m) => ExtRecEnv -> TypeScheme -> TypeScheme -> m ()
isInstance recenv ts1@(TypeScheme _ cs1 t1) (TypeScheme _ cs2 t2) = do
          subst <- case matchTerm t2 t1 :: Maybe (Subst TypeSigPos TVarId)  of
                     Just s -> return s
                     Nothing -> typeErr $ "type " ++ show t2 ++ " cannot be matched against "
                                ++ show t1
          let cs2' = appSubst subst cs2
              vars = boundVars ts1
          (acs, subst') <- match cs2'
          entails' recenv vars (appSubst subst' cs1) (map fromAtomicConstraintPos acs)


isInstance' :: (MonadTyping m, MonadFreshVar TVarId m)
            => FunId -> ExtRecEnv -> TypeScheme -> TypeScheme -> m ()
isInstance' funId recEnv ts1 ts2 = run
    where run = do
          ts2' <- simplifyTypeScheme False recEnv ts2
          ts1' <- simplifyTypeScheme False recEnv ts1
          ts1'' <- rename' ts1'
          ts2'' <- rename' ts2'
          addErrMsg ("type annotation of '" ++ funId ++ 
                     "' is not an instance of its inferred type" ++ 
                     "'\n" ++ "type annotation: '" ++ show ts1'' ++ "\n" ++
                     "inferred type: '" ++ show ts2'' ++ "'")
                $ isInstance recEnv ts1'' ts2''
