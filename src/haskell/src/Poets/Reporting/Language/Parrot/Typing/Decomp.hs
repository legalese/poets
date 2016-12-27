{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts #-}


--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Decomp
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides facilities to decompose a type term into a
-- head symbols and its arguments. Furthermore it provides a compact
-- representation of atomic types.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Decomp
    ( Decomp(..)
    , Decomp'(..)
    , ConstSym(..)
    , FunSym (..)
    , Sym (..) 
    , AtomicSubType
    , ASubst
    , AtomicType(..)
    , TermStr(..)
    , decomp
    , isAtomic
    , isConst
    , isVar
    , replaceArgs
    , occursCheck
    , partitionTypes
    , atomicTypeMaybe
    , atomicType
    , atomicType'
    , fromAtomicType
    , fromAtomicSubTypes
    , fromAtomicSubst
    ) where


import Poets.Data.Render
import Data.Comp.Variables hiding (isVar)
import Data.Comp.Ops
import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.TypingMonad

import Data.Map (Map)
import qualified Data.Map as Map

{-| This data type represents head symbols of (Parrot's) type term,
i.e. effectively it represents type constructors. -}

data ConstSym = SInt | SBool | SString | SDate | STime | SDateTime | SDuration | SReal | SRecord String | SChar | SUnit | SDurationDate
             deriving (Eq, Ord)

data FunSym = SList | SFun | SSum | SProd | SRef
             deriving (Eq, Ord)

instance Show ConstSym where
    show SInt = "Int"
    show SBool = "Bool"
    show SString = "String"
    show SDate = "Date"
    show SDateTime = "Datetime"
    show STime = "Time"
    show SDuration = "Duration"
    show SDurationDate = "DurationDate"
    show SReal = "Real"
    show (SRecord name) = name
    show SChar = "Char"
    show SUnit = "()"

instance Show FunSym where
    show SList = "[]"
    show SFun = "->"
    show SSum = "+"
    show SProd = ","
    show SRef = "<>"

{-| This data type represents results of a decomposition of a (Parrot)
type term. -}

data Decomp a = DVar TVarId | DConst ConstSym | DFun FunSym [a]
              deriving (Eq)


{-| This class defines the signature of the decomposition and the
replacement (generalised) algebra. The decomposition algebra defines
how to decompose a (type) term into head symbol and argument
terms. The replacement algebra defines how to replace the argument
terms of a (type) term by other (type) terms. -}

class (Functor f) => TermStr f where
    decompAlg :: f a -> Decomp a
    replaceAlg :: (Show a) => f a -> [a] -> f a

instance TermStr TypeConstant where
    decompAlg TInt = DConst SInt
    decompAlg TBool = DConst SBool
    decompAlg TString = DConst SString
    decompAlg TDateTime = DConst SDateTime
    decompAlg TTime = DConst STime
    decompAlg TDate = DConst SDate
    decompAlg TDuration = DConst SDuration
    decompAlg TReal = DConst SReal
    decompAlg (TRecord name) = DConst $ SRecord name

    replaceAlg c@TInt [] = c
    replaceAlg c@TBool [] = c
    replaceAlg c@TString [] = c
    replaceAlg c@TDateTime [] = c
    replaceAlg c@TDuration [] = c
    replaceAlg c@TReal [] = c
    replaceAlg c@TRecord {} [] = c
    replaceAlg t args = error $ "cannot replace " ++ show (length args)
                        ++ " arguments in type " ++ showF t


instance TermStr TypeEnt where
    decompAlg (TEnt ty) = DFun SRef [ty]
    replaceAlg TEnt {} [t] = TEnt t
    replaceAlg t args = error $ "cannot replace " ++ show (length args)
                        ++ " arguments in type " ++ showF t

instance TermStr TypeList where
    decompAlg (TList t) = DFun SList [t]

    replaceAlg TList {} [t] = TList t
    replaceAlg t args = error $ "cannot replace " ++ show (length args)
                        ++ " arguments in type " ++ showF t

instance TermStr PTypeVar where
    decompAlg (TVar var) = DVar var
    replaceAlg c@TVar {} [] = c
    replaceAlg t args = error $ "cannot replace " ++ show (length args)
                        ++ " arguments in type " ++ showF t

instance TermStr PTypeFun where
    decompAlg (TFun t1 t2) = DFun SFun [t1, t2]
    decompAlg (TSum t1 t2) = DFun SSum [t1, t2]
    decompAlg (TProd t1 t2) = DFun SProd [t1, t2]

    replaceAlg TFun {} [t1,t2] = TFun t1 t2
    replaceAlg TSum {} [t1,t2] = TSum t1 t2
    replaceAlg TProd {} [t1, t2] = TProd t1 t2
    replaceAlg t args = error $ "cannot replace " ++ show (length args)
                        ++ " arguments in type " ++ showF t

instance TermStr PTypeConst where
    decompAlg TDurationDate = DConst SDurationDate
    decompAlg TChar = DConst SChar
    decompAlg TUnit = DConst SUnit

    replaceAlg c@TDurationDate {} [] = c
    replaceAlg c@TChar {} [] = c
    replaceAlg c@TUnit {} [] = c
    replaceAlg t args = error $ "cannot replace " ++ show (length args)
                        ++ " arguments in type " ++ showF t


instance (TermStr f, TermStr g) => TermStr (f :+: g) where
    decompAlg (Inl t) = decompAlg t
    decompAlg (Inr t) = decompAlg t
    replaceAlg (Inl t) a = Inl (replaceAlg t a)
    replaceAlg (Inr t) a = Inr (replaceAlg t a)

instance (TermStr f) => TermStr (f :&: SrcPos) where
    decompAlg (t :&: _) = decompAlg t
    replaceAlg (t :&: pos) a = replaceAlg t a :&: pos





data Sym = FunSym FunSym 
         | ConstSym ConstSym
           deriving (Eq, Ord)

data Decomp' a = DVar' TVarId | DFun' Sym [a]


class DecompType d where
    {-| This function decomposes a given (type) term into its head symbol
      and a list of its argument terms. -}
    decomp :: (TermStr f) => Term f -> d (Term f)

    isAtomic :: d PType -> Maybe AtomicType

instance DecompType Decomp where
    decomp (Term t) = decompAlg t

    isAtomic (DVar v) = Just (AVar v)
    isAtomic (DConst sym) = Just (AConst sym)
    isAtomic _ = Nothing

instance DecompType Decomp' where
    decomp e = case decomp e of
                 DVar v -> DVar' v
                 DConst c -> DFun' (ConstSym c) []
                 DFun f args -> DFun' (FunSym f) args

    isAtomic (DVar' v) = Just (AVar v)
    isAtomic (DFun' (ConstSym sym) []) = Just (AConst sym)
    isAtomic _ = Nothing


{-| This function replaces the arguments of the given (type) term with
the given terms. -}

replaceArgs :: (TermStr f, Render f) => Term f -> [Term f] -> Term f
replaceArgs (Term t) a = Term $ replaceAlg t a

{-| This function performs the occurs check. It returns 'True' if the
given variable occurs in the given (type) term. Otherwise, it returns
'False'.  -}

occursCheck :: (TermStr f) =>  TVarId -> Term f -> Bool
occursCheck var t = case decomp t of
                      DVar v -> v == var
                      DConst _ -> False
                      DFun _ subs -> any (occursCheck var) subs

{-| This data type represents atomic data types, i.e. type constants
and type variables.  -}

data AtomicType = AVar TVarId
                | AConst ConstSym
                  deriving (Eq, Ord)

isConst :: AtomicType -> Bool
isConst AConst {} = True
isConst _ = False

isVar :: AtomicType -> Bool
isVar AVar {} = True
isVar _ = False

{-|
  This type represents a subtype constraint between atomic types.
-}
type AtomicSubType = (AtomicType,AtomicType)

{-| This type represents substitutions whose range contains only
atomic types.  -}

type ASubst = [(TVarId, AtomicType)]


instance Show AtomicType where
    show (AVar var) = show var
    show (AConst sym) = show sym

{-| This function partitions the given list of atomic types into a
list of type constants and a list of type variables. -}

partitionTypes :: [AtomicType] -> ([ConstSym],[TVarId])
partitionTypes [] = ([],[])
partitionTypes (ty: tys) = case ty of
                             AVar v -> (cs,v:vs)
                             AConst c -> (c:cs,vs)
    where (cs,vs) = partitionTypes tys


{-| This function turns atomic subtyping constraints into (general)
subtyping constraints using the given mapping from atomic subtyping
constraints to positions and atomic types to positions.  -}

fromAtomicSubTypes :: Map AtomicSubType SourcePos -> Map AtomicType SourcePos
                   -> [AtomicSubType] -> [TypeConstrPos PType]
fromAtomicSubTypes stbl ttbl = map (fromAtomicSubType' stbl ttbl)



{-| This function turn the given atomic subtyping constraint into a
(general) subtyping constraint using the given position and mapping
from atomic types to positions.  -}

fromAtomicSubType :: SrcPos -> Map AtomicType SourcePos -> AtomicSubType -> TypeConstrPos PType
fromAtomicSubType pos ptbl (s,t) = SubType (fromAtomicType' ptbl s) (fromAtomicType' ptbl t) :&: pos

{-| This function is similar to 'fromAtomicSubType' but takes a
mapping from atomic subtyping constraints to positions instead of a
single position.  -}

fromAtomicSubType' :: Map AtomicSubType SourcePos -> Map AtomicType SourcePos -> AtomicSubType -> TypeConstrPos PType
fromAtomicSubType' stbl ptbl st = fromAtomicSubType (Map.lookup st stbl) ptbl st

{-| This function turns an atomic substitution into a general
substitution using the given mapping from atomic types to positions.
-}

fromAtomicSubst :: Map AtomicType SourcePos -> ASubst -> Subst TypeSigPos TVarId 
fromAtomicSubst ptbl asubst = Map.fromList $ map fromA asubst
    where fromA (var, ty) = (var, fromAtomicType (Map.lookup ty ptbl)  ty)

{-| This function turns an atomic type into a (general) type using the
given position.  -}

fromAtomicType :: SrcPos -> AtomicType -> PType
fromAtomicType pos t = case t of
                         AVar v -> inject' $ TVar v
                         AConst SInt -> inject' TInt
                         AConst SBool -> inject' TBool
                         AConst SString -> inject' TString
                         AConst SDateTime -> inject' TDateTime
                         AConst SDate -> inject' TDate
                         AConst STime -> inject' TTime
                         AConst SDuration -> inject' TDuration
                         AConst SReal -> inject' TReal
                         AConst (SRecord n) -> inject' (TRecord n)
                         AConst SChar -> inject' TChar
                         AConst SUnit -> inject' TUnit
                         AConst SDurationDate -> inject' TDurationDate

    where inject' :: ((f :&: SrcPos) :<: TypeSigPos) => f PType -> PType
          inject' x = inject (x :&: pos)

{-| This function is similar to 'fromAtomicType' but takes a mapping
from atomic types to positions instead of a single position.  -}

fromAtomicType' :: Map AtomicType SourcePos -> AtomicType -> PType
fromAtomicType' ptbl t = fromAtomicType (Map.lookup t ptbl) t


atomicTypeMaybe :: (TermStr f) => Term f -> Maybe AtomicType
atomicTypeMaybe ty = case decomp ty of
                  DVar var -> return $ AVar var
                  DConst sym -> return $ AConst sym
                  DFun {} -> Nothing
                         
{-| This function turns a (general) type into a atomic type, provided
the given type is actually not a compound type. If it is a compound
type, a corresponding error is generated in the typing monad.  -}

atomicType :: (MonadTyping m, TermStr f, Render f) => String -> Term f -> m AtomicType
atomicType from ty = case atomicTypeMaybe ty of
                  Just a -> return a
                  Nothing -> typeErr $ "internal error ["++from++"]: atomic type expected, instead found '" ++ show (renderTerm ty) ++ "'"

{-| This function is similar to 'atomicType' but throws a (Haskell)
error instead of generating an explicit error in a monad. -}

atomicType' :: (TermStr f, Render f) => String -> Term f -> AtomicType
atomicType' from ty = case runTypingM (atomicType from ty) of
                   Left err -> error $ show err
                   Right res -> res
