{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts,
  UndecidableInstances, ScopedTypeVariables, TypeSynonymInstances, OverlappingInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.TypeAlias
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides auxiliary functions to deal with type aliases
-- such as 'String' (alias for '[Char]').
--
--------------------------------------------------------------------------------


module Poets.Reporting.Language.Parrot.Typing.TypeAlias
    (resolveAlias, forceAlias) where

import Poets.Reporting.Language.Parrot.Syntax



resolveAliasF :: (TypeList :<: f, PTypeConst :<: f, TypeConstant :<: f, Functor f)
              => Hom f f
resolveAliasF t = case proj t of
                    Just TString -> iTList iTChar
                    _ -> simpCxt t

resolveAliasF' :: (TypeList :<: f, PTypeConst :<: f, TypeConstant :<: f, Functor f', Functor f, DistAnn f p f')
               => Hom f' f'
resolveAliasF' = propAnn resolveAliasF

class ResolveAlias t where
    resolveAlias :: t -> t

instance ResolveAlias PType where
    resolveAlias = appHom resolveAliasF'

instance (ResolveAlias a, Functor f) => ResolveAlias (f a) where
    resolveAlias = fmap resolveAlias 

instance ResolveAlias TypeScheme where
    resolveAlias (TypeScheme pos cs t) = TypeScheme pos (resolveAlias cs) (resolveAlias t)



forceAlias' :: ((TypeList :&: SrcPos) :<: f, (PTypeConst :&: SrcPos) :<: f, (TypeConstant :&: SrcPos) :<: f)
              => Term f -> Term f
forceAlias' t = case project t of
                    Just (TList s :&: (p :: SrcPos)) -> 
                        case project s of
                          Just (TChar :&: (_ :: SrcPos)) -> inject $ TString :&: p
                          _ -> t
                    _ -> t


class ForceAlias t where
    forceAlias :: t -> t


instance (Functor f, (TypeConstant :&: SrcPos) :<: f, (TypeList :&: SrcPos) :<: f, (PTypeVar :&: SrcPos) :<: f, (PTypeConst :&: SrcPos) :<: f, (PTypeFun :&: SrcPos) :<: f) => ForceAlias (Term f) where
    forceAlias = cata (forceAlias' . Term)


instance (ForceAlias a, Functor f) => ForceAlias (f a) where
    forceAlias = fmap forceAlias 

instance ForceAlias TypeScheme where
    forceAlias (TypeScheme pos cs t) = TypeScheme pos (map forceAlias cs) (forceAlias t)