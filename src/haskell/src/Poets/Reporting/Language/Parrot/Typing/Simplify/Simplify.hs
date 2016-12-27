{-# LANGUAGE TupleSections, ScopedTypeVariables, FlexibleContexts, Rank2Types #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Simplify.Simplify
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Michael Werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements functions for simplifying type schemes.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Simplify.Simplify
    ( simplifyTypeScheme
    -- The rest is exposed for test use only
    , removeTautologies
    , mergeFieldConstraints
    , fromFieldConstraint
    , fuhMishraReduction
    , gSubsumed
    , satisfiable
    , instances
    , isTautology
    ) where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.TypingMonad
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicTypes 
import Poets.Reporting.Language.Parrot.Typing.Simplify.SubtypeConstraints
import Poets.Reporting.Language.Parrot.Typing.Simplify.FieldConstraints
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp 
import Poets.Reporting.Language.Parrot.Typing.Simplify.FuhMishra
import Poets.Reporting.Language.Parrot.Typing.Simplify.Match
import Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
import Poets.Reporting.Language.Parrot.FreshVar
import Poets.Data.Type.Utils
import Data.Comp.Variables
import Data.Comp.Unification as U
import Data.Maybe
import Data.Graph.SCC
import Data.Graph
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Writer.Lazy


simplifyTypeScheme :: (MonadTyping m, MonadFreshVar TVarId m)
                      => Bool -> ExtRecEnv -> TypeScheme -> m TypeScheme
simplifyTypeScheme doLazy recEnv s = do   
  s <- simplifyTypeScheme' doLazy recEnv s
  s <- simplifyTypeScheme' doLazy recEnv s
  s <- simplifyTypeScheme' doLazy recEnv s
  return s

{-
--fixM :: (Eq a, Monad m) => (a -> m a) -> a -> m a


fixM f a0 = do 
  a1 <- f a0  
  if (a0 == a1) then return a0 else fix f a1                       
-}


simplifyTypeScheme' :: (MonadTyping m, MonadFreshVar TVarId m)
                   => Bool -> ExtRecEnv -> TypeScheme -> m TypeScheme
simplifyTypeScheme'  doLazy recEnv (TypeScheme pos cs t) = do
  (acs, subst) <- atomify cs 
  t <- return $ appSubst subst t
  (acs, asubst) <- simplifyAtomic doLazy recEnv t acs 
--  (acs, subst) <- mergeFieldConstraints acs
  subst <- return $ compSubst subst (fromAtomicSubst asubst)
  t <- return $ appSubst subst t 
  cs <- return $ map fromAtomicConstraintPos acs
  let s = (TypeScheme pos cs t)
  satisfiableOrFail 100000 recEnv acs s
  return s


simplifyAtomic :: (MonadTyping m, MonadFreshVar TVarId m) => 
               Bool -> ExtRecEnv -> PType -> [AConstraint a] ->  
               m ([AConstraint a], ASubstitution)
simplifyAtomic True recEnv t cs = 
    liftS (entailmentReduction t) (cs,Map.empty)
      >>= removeTautologies recEnv
      >>= liftS removeDuplicates
      >>= liftS (lazy t)
      >>= liftS (prettyTypeVariables t)
simplifyAtomic False recEnv t cs = 
  combineSccs cs
    >>= removeTautologies recEnv
    >>= liftS removeDuplicates
    >>= liftS (prettyTypeVariables t)
  

-- TODO: Are duplicated field constraints a problem? 
--       What if the duplicates have different positions?
mergeFieldConstraints :: (MonadTyping m, MonadFreshVar TVarId m) => 
                         [AConstraint a] -> m ([AConstraint a], Subst TypeSigPos TVarId)
mergeFieldConstraints cs = do
  let ps = partition fcs
  pairs <- execWriterT (mapM_ tryMerge ps)
  let (rejected, nonAtomicSubsts) = unzip pairs
  -- TODO position sensitive
  let fcs' = Set.toList $ Set.fromList fcs Set.\\ Set.fromList rejected 
  let cs' = map fromFieldConstraint fcs' :: [AConstraint a]
  let nonAtomicSubst = foldl compSubst Map.empty nonAtomicSubsts :: Subst TypeSigPos TVarId
  let nonAtomicCs = map fromAtomicConstraintPos (cs' ++ others)
  let nonAtomicCs' = appSubst nonAtomicSubst nonAtomicCs
  (cs, subst) <- atomify nonAtomicCs'
  return (cs, compSubst subst nonAtomicSubst)
  where
    partition = List.groupBy fieldEq
    fieldEq a b = fieldName a == fieldName b
    fieldName (_, n, _, _) = n
    
    (fcs, others) = splitFieldConstraints cs
    (subtypeConstraints, _) = splitSubtypings others
    subtypeOf' = subtypeOf subtypeConstraints
    tryMerge p = mapM_ unifyDiscard [(a,b) | a <- p, b <- p, a /= b] 
    
    unifyDiscard :: (MonadTyping m1) => (AFieldConstraint, AFieldConstraint) -> 
                    WriterT [(AFieldConstraint, Subst TypeSigPos TVarId)] m1 ()
    unifyDiscard (c1@(r1,_,t1,_), (r2,_,t2,_)) =  
      when (subtypeOf' r1 r2) $ do
        let substM = unify [(t1, t2)]
        nonAtomicSubst <- liftUnifErr substM
        tell [(c1, nonAtomicSubst)]



removeTautologies :: (Monad m) => ExtRecEnv -> ([AConstraint a], ASubstitution) -> m ([AConstraint a], ASubstitution)
removeTautologies recEnv p = return $ fix (removeTautologies' recEnv) p
  where 
    fix :: Eq a => (a -> a) -> a -> a
    fix f a0 = 
      let a1 = f a0  
      in if (a0 == a1) then a0 else fix f a1 


removeTautologies' :: ExtRecEnv -> ([AConstraint a], ASubstitution) -> ([AConstraint a], ASubstitution)
removeTautologies' recEnv (cs, subst) = do
  let (cs', subst') = run cs
  (atomicApplySubst subst' cs', subst' `atomicCompSubst` subst)
  where 
    run :: [AConstraint a] -> ([AConstraint a], ASubstitution)
    run [] = ([], idSubst)
    run (c:cs) = let (cs', s) = run cs
                 in case isTautology recEnv (atomicApplySubst s c) of
                   Just s' -> (cs', s `atomicCompSubst` s')
                   Nothing -> (c:cs', s)


isTautology :: ExtRecEnv -> AConstraint a -> Maybe ASubstitution
isTautology _ (ASubType s t :&: _) | s == t = Just idSubst
isTautology recEnv (ASubType s t :&: _) = 
  case (atomicDecomp s, atomicDecomp t) of
    (AConst SInt, AConst SReal) -> Just idSubst
    (AConst SDate, AConst SDurationDate) -> Just idSubst
    (AConst SDuration, AConst SDurationDate) -> Just idSubst
    (AConst (SRecord r1), AConst (SRecord r2)) -> 
     if recordSubtypeOf recEnv r1 r2 
     then Just idSubst
     else Nothing
    _ -> 
      -- Domain specific tautologies
      case (atomicDecomp s, atomicDecomp t) of
        (AVar var, AConst SBool) -> Just $ singleSubst var t
        (AConst SBool, AVar var) -> Just $ singleSubst var s
        (AVar var, AConst SInt) -> Just $ singleSubst var t
        (AConst SReal, AVar var) -> Just $ singleSubst var s
        (AVar var, AConst SDate) -> Just $ singleSubst var t
        (AVar var, AConst SDuration) -> Just $ singleSubst var t
        (AConst SDurationDate, AVar var) -> Just $ singleSubst var s
        _ -> Nothing

isTautology recEnv (AHasField r field t :&: _) = 
  case atomicDecomp r of
    AConst (SRecord n) -> 
      case getTypeField recEnv n field of
        Left (_ :: String) -> Nothing
        Right Field{ fieldType = ty} 
          | ty == t -> Just idSubst
          | otherwise -> Nothing
    _ -> Nothing

isTautology _ (AOrd t :&: _) = 
  case atomicDecomp t of
    AConst SUnit -> Just idSubst
    AConst SBool -> Just idSubst
    AConst SInt -> Just idSubst
    AConst SReal -> Just idSubst
    AConst SChar -> Just idSubst
    AConst SDate -> Just idSubst
    AConst SDuration -> Just idSubst
    AConst SDurationDate -> Just idSubst
    AConst SString -> Just idSubst
    _ -> Nothing
isTautology recEnv (AEq t :&: p) = 
  case atomicDecomp t of
    AConst (SRecord _) -> Just idSubst
    _ -> isTautology recEnv (AOrd t :&: p) 

{-| Remove duplicated constraints based on equality. -}
    
removeDuplicates :: ([AConstraint a], ASubstitution) -> ([AConstraint a], ASubstitution)
removeDuplicates (cs, subst) = (List.nub cs, subst)
                                                                                         
{-| Rename type variables to use roman letters or consecutive number if 
more that 26 variables are needed. -}

prettyTypeVariables :: PType -> ([AConstraint a], ASubstitution) -> 
                       ([AConstraint a], ASubstitution)
prettyTypeVariables t (cs, subst) = 
  let t' = appSubst (fromAtomicSubst subst) t
      varsCS = atomicVariables cs
      varsT = atomicVariables t' -- TODO positions are lost here!
      vars = Map.unionWith mplus varsCS varsT
      latinVars = [[c] |c <- ['a'..'z']]
      intVars = ["$" ++ show i | i <- [1,2..]] 
      newVars = if Map.size vars <= length latinVars then latinVars else intVars
      zips = Map.toList vars `zip` newVars
      substList = foldl (\l ((tvar, pos), new) -> (tvar, atomicRecomp (AVar $ TVarId new 0, pos)):l) [] zips
      renameSubst = Map.fromList substList
      cs' = atomicApplySubst renameSubst cs
      subst' = renameSubst `atomicCompSubst` subst
  in (cs', subst')


combineSccs :: MonadTyping m => [AConstraint a] -> m ([AConstraint a],ASubstitution)
combineSccs cs = do 
  let (subs, others) = splitSubtypings cs
  (subs',subst) <- combineSccs' subs
  return (map fromSubtypeConstraint subs' ++ others, subst)
                     

{-| This function takes a list of atomic subtyping constraints and
removes circles from the constraints (i.e. something of the form t1 <
t2 < .. < tn < t1) by equating the types involved in a
circle. This is implemented by generating the graph of the subtyping
constraints via 'nodes' and then computing the SCCs of the graph. -}

combineSccs' :: (MonadTyping m) => [ASubtypeConstraint] -> m ([ASubtypeConstraint],ASubstitution)
combineSccs' subs = do
  let edges = sccGraph graph
  (_, subs', subst') <- traverse edges
  return (subs', subst')
    where (graph, ret, _) = createGraph subs
          retNode v = let (n,_,_) = ret v in n
          loopupPos :: [(AtomicPType, AtomicPType)] -> SrcPos
          loopupPos keys = 
            let acc ps (s, t, p) = if isJust p && (s, t) `elem` keys then p:ps else ps
            in case foldl acc [] subs of
              [] -> Nothing
              p:_ -> p
          traverse :: (MonadTyping m) => [(SCC  Int, Int, [Int])]
                   -> m (Map Int (AtomicPType, [AtomicPType]), [ASubtypeConstraint], ASubstitution)
          traverse [] = return (Map.empty, [], Map.empty)
          traverse ((lscc,key,chs):edges) = do 
            (register,subs,subst) <- traverse edges
            let connectedNodes = map retNode (flattenSCC lscc)
            (t,subst') <- single connectedNodes
            let bind i = let (choosen, all) = fromJust $ Map.lookup i register
                             keys = map (\s -> (t, s)) all
                             pos = loopupPos keys
                         in (t, choosen, pos) 
                chs' = filter (/= key) chs
                subs' = map bind chs'
            return (Map.insert key (t, connectedNodes) register,subs'++subs, subst' `compSubst` subst)

{-| This function takes a list of atomic types and tries to equate
them. This only works if there is only at most one type constant among
the atomic types. The function returns the single atomic type, the
input types are equated to, and the corresponding atomic substitution
that realises the equating of the types. If equating the types is not
possible, i.e. there are at least two type constants involved, a
corresponding error is generated. -}

single :: (MonadTyping m) => [AtomicPType] -> m (AtomicPType, ASubstitution)
single [t] = return (t, Map.empty)
single ts = case cs of
  (c1,p1):(c2,_):_ -> withPos' p1 $ typeErr ("Incompatible types " ++ show c1 ++ " and  " ++ show c2)
  [c] -> let t = atomicRecomp c
             bind (v, _) = (v, t)
         in return (t, Map.fromList $ map bind vs)
  [] -> let (var:vs') = vs
            t = atomicRecomp var
            bind (v, _) = (v, t)
        in return (t, Map.fromList $ map bind vs')
  where (cs,vs) = splitBy typeVarTagger ts
    
{-| This function tags an atomic type by Right if it is a type constants
and by Left otherwise. -}

typeVarTagger :: AtomicPType -> Either (ConstSym, SrcPos) (TVarId, SrcPos)
typeVarTagger t = case atomicDecomp t of
  (AVar tvar, pos) -> Right (tvar, pos)
  (AConst symbol, pos) -> Left (symbol, pos)


{-| Splits a list based on a tag function -}

splitBy :: (a -> Either b c) -> [a] -> ([b],[c])
splitBy tagger as = foldl f ([], []) as
  where
    f (bs, cs) a = case tagger a of
      Left b -> (b:bs, cs)
      Right c -> (bs, c:cs)


liftS :: (Monad m) => (a -> a) -> a -> m a 
liftS simplification input = return $ simplification input



recordSubtypeOf :: Eq e => RecordEnv e -> RecordName -> RecordName -> Bool
recordSubtypeOf recEnv a b = either (const False) id (isSubType recEnv a b)

satisfiableOrFail :: (MonadTyping m) => 
                     Int -> ExtRecEnv -> [AConstraint a] -> TypeScheme -> m ()
satisfiableOrFail maxTries recEnv cs s@(TypeScheme pos _ _)= do
  let varCount = Map.size $ atomicVariables cs
  let symbolCount = length $ groundSymbols recEnv
  let combinations = symbolCount^varCount
  return ()
  -- when (combinations > maxTries) $ withPos' pos $ typeErr $ 
  --   "Brute force satisfiability check would take too long for the followin typing:\n "++
  --    show s ++ "\n"++
  --    "With "++ show symbolCount ++" ground symbols and "++ show varCount ++ 
  --    " type variables there are "++ show symbolCount ++"^"++ show varCount ++ 
  --    " = " ++ show combinations ++ " instantiations to check."
  -- unless (satisfiable recEnv cs) $ withPos' pos $ typeErr $
  --   "The following typing has no valid instances:\n" ++
  --   show s ++ "\n"
  
  
satisfiable :: ExtRecEnv -> [AConstraint a] -> Bool
satisfiable recEnv cs = 
  let vars = Map.keys $ atomicVariables cs
      symbols = groundSymbols recEnv
      alpha = map ((flip $ curry atomicRecomp) Nothing) symbols
      substitutions = map Map.fromList $ allSubstitutions vars alpha
      instances = map ($ cs) (map atomicApplySubst substitutions) 
      checkInstance cs = all (isJust . isTautology recEnv) cs
  in any checkInstance instances

groundSymbols :: ExtRecEnv -> [ConstSym]
groundSymbols recEnv =
  let recNames = map recordName $ recordEnvList recEnv
  in [SInt, SBool, SString, SDate, SDuration, 
      SReal, SChar, SUnit, SDurationDate] ++ 
     map SRecord recNames


instances :: ExtRecEnv -> [AConstraint a] -> [[AConstraint a]]
instances recEnv cs = 
  let vars = Map.keys $ atomicVariables cs
      recNames = map recordName $ recordEnvList recEnv
      symbols = [SInt, SBool, SString, SDate, SDuration, 
                 SReal, SChar, SUnit, SDurationDate] ++ 
                map SRecord recNames
      alpha = map ((flip $ curry atomicRecomp) Nothing) symbols
      substitutions = map Map.fromList $ allSubstitutions vars alpha
      instances = map ($ cs) (map atomicApplySubst substitutions) 
  in instances


{-| Generate all |alpha|^|vars| substitutions usgin the given 
variables to symbols in the alphabet-}

allSubstitutions :: [a] -> [b] -> [[(a,b)]]
allSubstitutions vars alpha = 
  let words = (kWords (length vars) alpha)
  in map (\word -> zip vars word) words


  
  
{-| Find all sequences of length @k@ usen the given list as alphabet. -}
kWords = kWords' []

kWords' :: [a] -> Int -> [a] -> [[a]]
kWords' res 0 _ = return res
kWords' res k alpha = do
  a <- alpha
  kWords' (a:res) (k-1) alpha

