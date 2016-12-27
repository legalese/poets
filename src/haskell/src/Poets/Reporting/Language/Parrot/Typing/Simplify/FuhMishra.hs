{-# LANGUAGE TypeOperators, FlexibleInstances, TupleSections, ScopedTypeVariables, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Simplify.FuhMishra
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Michael Werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements two functions described in a paper by 
-- You-Chin Fuh & Prateek Mishra
-- /Polymorphic Subtype Inference: Closing the Theory-Practice Gab/, 
-- 1989
-- 
-- The first 'entailmentReduction' tries to reduce the subtyping 
-- relations to an equivalent smaller form.  
-- 
-- The second 'lazy' removes subtype constraints which is there 
-- only to allow one to do a up-case on the associated type .
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Simplify.FuhMishra
       ( -- * Entailment reduction skeleton
         fuhMishraReduction
         
         -- * G Reduction - Entailment reduction
         , entailmentReduction
         , gSubsumed
           
         -- * S Reduction - Lazy instantiation
         , lazy
         , sSubsumed
       ) where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicTypes 
import Poets.Reporting.Language.Parrot.Typing.Simplify.SubtypeConstraints
import Poets.Reporting.Language.Parrot.Typing.Simplify.FieldConstraints
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp 
import Poets.Reporting.Language.Parrot.FreeVars
import Data.Comp.Variables
import Data.Comp.Ops
import Data.Graph
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad


type Entails = PType -> FSubsumed -> Lookup -> Graph -> Vertex -> Vertex -> Maybe (TVarId, AtomicPType)
type Lookup = Vertex -> AtomicPType
type FSubsumed = AtomicPType -> AtomicPType -> Bool



                                
{----------------------------------------------------------
 -- Entailment Reduction Skeleton
 ----------------------------------------------------------}

{-| Generic entailment reduction function that simplifies a list of 
subtype constraints according to the given entailment function -}

fuhMishraReduction :: Entails -> PType -> [ASubtypeConstraint] -> 
                      [AFieldConstraint] -> 
                     ([ASubtypeConstraint], ASubstitution)
fuhMishraReduction entails t scs fcs = 
  (map fromEdge (edges graph'), substitution)
    where
        (substitution, graph') = fix reduce graph Map.empty
        reduce :: (ASubstitution, Graph) -> Maybe (ASubstitution, Graph)
        reduce (subst, graph) = 
          let vs = vertices' graph
              ps = [(a, b) | a <- vs, b <- vs, a /= b]
              t' = appSubst (fromAtomicSubst subst) t
              fcs' = map (\(r,n,t,p) -> (appSubst subst r, n, appSubst (fromAtomicSubst subst) t, p)) fcs
              fSubsumed = fSubsumed' fcs'
              f (a, b) = do 
                sub <- entails t' fSubsumed lookupNode graph a b
                let r = (uncurry Map.singleton sub,  mergeVertices graph a b)
                return r
          in msum $ map f ps
        (graph, lookup, _) = createGraph scs
        lookupNode = (\(x,_,_)->x) . lookup
        fix f graph substitution = case f (substitution, graph) of
            Just (s, graph) -> fix f graph (compSubst s substitution)
            Nothing -> (substitution, graph)
        fromEdge :: Edge -> ASubtypeConstraint
        fromEdge (v1, v2) = 
          let (s, t) = (lookupNode v1, lookupNode v2)
          in case List.find (\(s', t', _)-> (s,t)==(s', t')) scs of
            Just s -> s
            Nothing -> (s, t, Nothing)
            
{----------------------------------------------------------
 -- G Etailment Reduction
 ----------------------------------------------------------}
        
{-| This simplification function tries to collapse as many subtype 
constraints as possible according to the 'gSubsumed' function. 
An example could be to replace @a < b < c@ with @a < b@ and then map 
@c -> b@ in the returned substitution. All free variables from the 
given type and the non subtype constraints are protected from 
being substituted.
-} 

entailmentReduction :: PType -> ([AConstraint a], ASubstitution) -> 
                       ([AConstraint a], ASubstitution)
entailmentReduction t (cs, subst1) = 
  let (subs, others) = splitSubtypings cs
      (fields, _) = splitFieldConstraints others
      (subs', subst2) = 
        fuhMishraReduction gSubsumed t subs fields
      cs' = others ++ map fromSubtypeConstraint subs'
      subst = subst1 `compSubst` subst2
  in (atomicApplySubst subst cs', subst)


gSubsumed :: Entails
gSubsumed t fSubsumed lookup graph nodeA nodeB = do
  let protected = freeVars t
  let a = lookup nodeA 
  varA <- atomicVar a
  guard $ not (Set.member varA  protected)
  let b = lookup nodeB 
  --I have removed this constraint, it is not required by the FM paper. -Werk
  _ <- atomicVar b  
  let graphT = transposeG graph
  let above = reaching graphT
  let below = reaching graph
  let subtypeOf x y = Set.member x (above y) 
  let isDowncast = nodeB `subtypeOf` nodeA
  guard $ isDowncast || fSubsumed b a
  guard $ 
    Set.delete nodeA (above nodeA) `Set.isSubsetOf` above nodeB &&
    Set.delete nodeA (below nodeA) `Set.isSubsetOf` below nodeB
  return (varA, b)
        
{----------------------------------------------------------
 -- S Etailment Reduction - Lazy instantiation
 ----------------------------------------------------------}
                                
{-| This type represents subtyping variance. -}
data Variance = CoVariance | ContraVariance | BiVariance | Invariance deriving (Eq, Show)

infixr 5 +++

{-| Given the variances on a type variable @A@ in @S1@ and @S2@ where @S1@ 
and @S2@ are subexpressions in the type @T@ both occuring in co-variant 
position, this function calculates the variance on @A@ in @T@. This 
corresponds to the union operation had we used set representation of 
'Variance' as done in the Fuh & Mishra paper. 
-}

(+++) :: Variance -> Variance -> Variance
CoVariance +++ CoVariance = CoVariance
ContraVariance +++ ContraVariance = ContraVariance
a +++ Invariance = a
Invariance +++ b = b
_ +++ BiVariance = BiVariance
BiVariance +++ _ = BiVariance
CoVariance +++ ContraVariance = BiVariance
ContraVariance +++ CoVariance = BiVariance

        
inverseVariance :: Variance -> Variance  
inverseVariance CoVariance = ContraVariance
inverseVariance ContraVariance = CoVariance
inverseVariance BiVariance = BiVariance
inverseVariance Invariance = Invariance
                                  

{-| This algebra defines the variance on the type variable in f which 
is supposed to be a type expression. -}

class Functor f => VarianceAlg f where
    varianceAlg :: TVarId -> f Variance -> Variance
    
instance VarianceAlg PTypeVar where
    varianceAlg a (TVar b) | a == b = CoVariance
    varianceAlg _ (TVar _) = Invariance

instance VarianceAlg TypeEnt where
    varianceAlg _ (TEnt v) = v
    
instance VarianceAlg PTypeFun where
    varianceAlg _ (TFun v1 v2) = inverseVariance v1 +++ v2
    varianceAlg _ (TSum v1 v2) = v1 +++ v2
    varianceAlg _ (TProd v1 v2) = v1 +++ v2

instance VarianceAlg TypeList where
    varianceAlg _ (TList v)  = v

instance VarianceAlg PTypeConst where
    varianceAlg _ _ = Invariance

instance VarianceAlg TypeConstant where
    varianceAlg _ _ = Invariance

instance (VarianceAlg f, VarianceAlg g) => VarianceAlg (f :+: g) where
    varianceAlg a (Inl f) = varianceAlg a f
    varianceAlg a (Inr g) = varianceAlg a g
    
instance VarianceAlg f => VarianceAlg (f :&: SrcPos) where
    varianceAlg a (f :&: _) = varianceAlg a f

{-| Uses 'varianceAlg' extract the variance of a 'Term' -}

variance :: (VarianceAlg f) => TVarId -> (Term f) -> Variance 
variance a = cata (varianceAlg a)

{-| Remove constraints whichs single purpose is to allow the given 
type to be a super type of its \"actual\" type. For instance, the 
infered type of the identity function might be @b < a, a < c => b -> c@.
This function substitutes @b@ and @c@ back to the  \"actual\" type @a@
making it implicit that an instantiation of this type are allowed to 
go back to use the super type.
-}

lazy :: PType -> ([AConstraint a], ASubstitution) ->  
        ([AConstraint a], ASubstitution)
lazy t (cs, subst1) =
  let (subs, others) = splitSubtypings cs
      (fields, _) = splitFieldConstraints others
      t' = appSubst (fromAtomicSubst subst1) t
      (subs', subst2) = fuhMishraReduction sSubsumed t' subs fields
      cs' = others ++ map fromSubtypeConstraint subs'
      subst = subst1 `compSubst` subst2
  in (atomicApplySubst subst cs', subst)


sSubsumed :: Entails
sSubsumed t fSubsumed lookup graph nodeA nodeR = do
  let a = lookup nodeA 
  varA <- atomicVar a
  let r = lookup nodeR 
  let graphT = transposeG graph
  let above = reaching graphT
  let below = reaching graph
  let subtypeOf x y = Set.member x (above y) 

  guard $ 
    (nodeR `subtypeOf` nodeA && variance varA t == CoVariance &&
     Set.delete nodeA (below nodeA) `Set.isSubsetOf` below nodeR)
    ||
    (nodeA `subtypeOf` nodeR && variance varA t == ContraVariance &&
     Set.delete nodeA (above nodeA) `Set.isSubsetOf` above nodeR &&
     fSubsumed r a
    )
  return (varA, r)

fSubsumed' :: [AFieldConstraint] -> AtomicPType -> AtomicPType -> Bool
fSubsumed' fcs x y = fieldConstraints fcs y `Set.isSubsetOf` fieldConstraints fcs x
  
  
fieldConstraints :: [AFieldConstraint] -> AtomicPType -> Set (String, PType)
fieldConstraints fcs a =
  Set.fromList $ map (\(_, n, t, _) -> (n, t)) $ List.filter (\(r, _, _, _) -> r == a) fcs


