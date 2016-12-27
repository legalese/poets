{-# LANGUAGE
  TypeOperators,
  FlexibleInstances,
  TupleSections,
  GeneralizedNewtypeDeriving,
  RankNTypes,
  FlexibleContexts,
  TemplateHaskell,
  ImpredicativeTypes
  #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Simplify.Match
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Michael Werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements a match algorithm according to Fuh and
-- Mishra [Theor. Comp. Sci. 73 (1990) 155-175]
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Simplify.Match
    ( atomify
    , match
    , MatchM
    ) where

import Poets.Reporting.Language.Parrot.FreshVar
import Poets.Reporting.Language.Parrot.FreeVars
import Poets.Reporting.Language.Parrot.Typing.TypingMonad
import Poets.Reporting.Language.Parrot.Typing.Decomp
import Poets.Reporting.Language.Parrot.Syntax

import Data.Comp.Variables
import Data.Comp.Derive

import Prelude hiding (sequence, mapM)

import Control.Monad hiding (sequence, mapM)
import Control.Monad.Reader hiding (sequence, mapM)
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Writer hiding (sequence, mapM)

import Data.Equivalence.Monad
import Data.List
import Data.Traversable
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

{-|
  This data type represents subtyping constraints.
-}
data SubTypeConstr e = SubTypeConstr e e

$(derive [makeFunctor] [''SubTypeConstr])


{-| This data type represents subtyping constraints annotated with a
source position.  -}

type SubTypeConstrPos = SubTypeConstr :&: SrcPos


{-| This is the type of the over which we build equivalence classes
for the match algorithm -}

type EquivValue = AtomicType

{-| This type represents the information we attach to each equivalence
class. The first component is the set of variables in the equivalence
class (i.e. the equivalence class minus the set of constants) and the
second component is some type constant in the equivalence class or
@Nothing@ if the equivalence class just contains variables. -}

type EquivDesc = (Set TVarId, Maybe ConstSym)

{-| This function is used to combine the descriptors of two
equivalence classes, which are combined.  -}

combineEquivDesc :: EquivDesc -> EquivDesc -> EquivDesc
combineEquivDesc (xs,c1) (ys,c2) = (xs `Set.union` ys, c1 `mplus` c2)

{-| This function is used to create the descriptor of a singleton
equivalence class.  -}

makeEquivDesc :: EquivValue -> EquivDesc
makeEquivDesc (AConst c)  = (Set.empty, Just c)
makeEquivDesc (AVar v)  = (Set.singleton v, Nothing)

{-| This is the type of the log of the matching monad 'MatchM'. It
represents the list of atomic subtyping constraints generated during
the match algorithm (without the substitution being applied to it).
-}

type MatchLog = [SubTypeConstrPos PType]

{-| This is the type of the state of the matching monad 'MatchM'. It
contains the accumulated substitution and a working set of subtyping
constraints that still need to be processed.  -}

data MatchState = MatchState { msSubst :: Subst TypeSigPos TVarId,
                               msWorkingSet :: [SubTypeConstrPos PType]}
{-|
  This is the type of the environment for the matching monad 'MatchM'.
-}
type MatchEnv = SrcPos

{-| The matching monad is used to implement the matching algorithm. It
keeps track of a working set of subtype constraints that still need to
be processed, the substitution that has been accumulated so far, a
list of atomic subtype constraints that have been generated by the
matching algorithm, and an equivalence relation over atomic
types. Moreover, it also maintains the position information of the
constraint currently working on. -}

type MatchM a = forall s m . (MonadTyping m, MonadFreshVar TVarId m) => MatchT s m a

type MatchT s m  = EquivT s EquivDesc EquivValue 
    (ReaderT MatchEnv (StateT MatchState (WriterT MatchLog m)))


{-| This function runs the 'MatchT' monad transformer. It takes a list
of subtyping constraints to initialise the working set, and returns
the accumulated substitution and a list of the generated atomic
subtyping constraints (with the substitution already applied to
them). -}

runMatchT :: (MonadTyping m, MonadFreshVar TVarId m)
          => [SubTypeConstrPos PType]
          -> (forall s . MatchT s m ())
          -> m ([SubTypeConstrPos PType], Subst TypeSigPos TVarId)
runMatchT initWS
    = liftM arr .
      runWriterT .
      (`runStateT` state) .
      (`runReaderT` Nothing) .  
      runEquivT makeEquivDesc combineEquivDesc
    where arr (((),MatchState{msSubst=subst}),cs) = (cs,subst)
          state = MatchState {msSubst = Map.empty, msWorkingSet = initWS }





{-| This function adds a subtyping constraint to the constraint store.
-}

addSubConstr :: PType -> PType -> MatchM ()
addSubConstr t1 t2 = do
  pos <- getConstrPos
  tell [SubTypeConstr t1 t2 :&: pos]

{-| This function is similar to 'addSubConstr' but it swaps the
direction of the subtyping constraint if the variance argument is
'Constravariant'.  -}

addSubConstr' :: Variance -> PType -> PType -> MatchM ()
addSubConstr' Covariant s t = addSubConstr s t
addSubConstr' Contravariant s t = addSubConstr t s

{-|
  This function returns the current working set.
-}

getWorkingSet :: MatchM [SubTypeConstrPos PType]
getWorkingSet = liftM msWorkingSet get


{-| This function sets the working set.  -}

putWorkingSet :: [SubTypeConstrPos PType] -> MatchM ()
putWorkingSet ws = do 
  st <- get
  put st {msWorkingSet = ws}

{-| This function returns the current value of the accumulated
substitution.  -}

getSubst :: MatchM (Subst TypeSigPos TVarId)
getSubst = liftM msSubst get

{-| This function sets the current value of the accumulated
substitution.  -}

putSubst :: Subst TypeSigPos TVarId -> MatchM ()
putSubst ws = do 
  st <- get
  put st {msSubst = ws}

{-| This function takes a 'MatchM' monad depending on a subtyping
constraint and runs it with the next subtyping constraint in the
working set and afterwards runs the 'MatchM' monad given as the second
argument. If the working set is empty, this function does nothing. -}

nextConstr :: (SubTypeConstr PType -> MatchM ()) -> MatchM () -> MatchM ()
nextConstr f cont = do
  ws <- getWorkingSet
  case ws of
    [] -> return ()
    (c :&: pos) :cs -> 
        putWorkingSet cs >>
        withConstrPos pos (f c) >>
        cont

{-| This function adds the given list of subtyping constraints to the
working set. -}

addToWorkingSet :: [SubTypeConstr PType] -> MatchM ()
addToWorkingSet cs = do
  ws <- getWorkingSet
  pos <- getConstrPos
  let cs' = map (:&: pos) cs
  putWorkingSet $ cs' ++ ws




{-| This function runs the given 'MatchM' monad in the context of the
given position. This is meant to be the position information of a
subtyping constraint which is then used when constructing the atomic
subtyping constraints obtained from it. -}

withConstrPos :: SrcPos -> MatchM a -> MatchM a
withConstrPos pos m = local change m
    where change oldPos = pos `mplus` oldPos

{-| This function obtains the current constraint's position.  -}

getConstrPos :: MatchM SrcPos
getConstrPos = ask


{-| This function implements the match algorithm. It takes a list of
type constraints, and returns the most general matching substitution
along with a modified list of constraints with the substitution
applied to it and where the subtyping constraints were replaced by
their corresponding atomic subtyping constraints. -}

atomify :: (MonadTyping m, MonadFreshVar TVarId m) => 
           [TypeConstrPos PType] -> m ([AtomicTypeConstrPos a], Subst TypeSigPos TVarId)
atomify = match

match :: (MonadTyping m, MonadFreshVar TVarId m) => 
         [TypeConstrPos PType] -> m ([AtomicTypeConstrPos a], Subst TypeSigPos TVarId)
match cs = do
  let (subtypeConstraints, othersConstraints) = filterSub cs
  (subtypeConstraints, substitution) <- match' subtypeConstraints
  othersConstraints <- return $ appSubst substitution othersConstraints
  atomicConstraints1 <- atomifyOthers othersConstraints
  let atomicConstraints2 = map fromSub subtypeConstraints
  return (atomicConstraints1 ++ atomicConstraints2, substitution) 
    where filterSub [] = ([],[])
          filterSub (c : cs) 
              = let (sts,ocs) = filterSub cs
                in case c of
                     SubType t1 t2 :&: p -> ((SubTypeConstr t1 t2 :&: p) : sts, ocs)
                     oc -> (sts, oc:ocs)
          fromSub :: SubTypeConstrPos AtomicPType -> AtomicTypeConstrPos a
          fromSub ((SubTypeConstr s t :&: pos)) = ASubType s t :&: pos


{-| This function implements the match algorithm. It takes a list of
subtype constraints, and returns the most general matching substitution
along with a modified list of constraints with the substitution
applied to it and where the subtyping constraints were replaced by
their corresponding atomic subtyping constraints. -}

match' :: (MonadTyping m, MonadFreshVar TVarId m)
      => [SubTypeConstrPos PType] -> m ([SubTypeConstrPos AtomicPType], Subst TypeSigPos TVarId)
match' cs = do (cs,subst) <- runMatchT cs matchM
               return (concatMap (decompSubTypeConstr subst) cs, subst)

decompSubTypeConstr :: Subst TypeSigPos TVarId -> SubTypeConstrPos PType -> [SubTypeConstrPos AtomicPType]
decompSubTypeConstr subst (SubTypeConstr s t :&: pos)
    = run (appSubst subst s) (appSubst subst t)
    where run s t = case (decomp s, decomp t) of
                        (DFun SFun [s1,s2], DFun SFun [t1,t2]) ->
                            run s2 t2 ++ run t1 s1
                        (DFun _ args1, DFun _ args2) -> 
                            concat $ zipWith run args1 args2
                        _ -> 
                          let Just s' = toAtomicPType s
                              Just t' = toAtomicPType t
                          in [SubTypeConstr s' t' :&: pos]
                    


{-| This is the implementation of the match algorithm within the
'MatchM' monad.  -}

matchM :: MatchM ()
matchM = nextConstr matchM' matchM


{-| This function executes a the match algorithm on a single subtyping
constraint.  -}

matchM' :: SubTypeConstr PType -> MatchM ()
matchM' (SubTypeConstr t1 t2)
    = case (isAtomic d1, isAtomic d2) of
        (Just a1, Just a2) -> atomic a1 a2
        _ -> case (d1,d2) of
               (DVar' v1, _) -> expansion Covariant v1 t2
               (_ , DVar' v2) -> expansion Contravariant v2 t1
               (DFun' s1 args1, DFun' s2 args2) -> decomposition s1 args1 s2 args2
                    
    where atomic :: AtomicType -> AtomicType -> MatchM ()
          atomic a1 a2 = equate a1 a2 >> addSubConstr t1 t2
          decomposition :: Sym -> [PType] -> Sym -> [PType] -> MatchM ()
          decomposition s1 args1 s2 args2 
              | s1 /= s2 = subtypeErr Covariant t1 t2 "'; the types are structurally distinct"
              | s1 == FunSym SFun = let [t11,t12] = args1
                                        [t21,t22] = args2
                                    in addToWorkingSet [SubTypeConstr t21 t11, SubTypeConstr t12 t22]
              | otherwise = addToWorkingSet $ zipWith SubTypeConstr args1 args2
          
          expansion va var t = do
            (vars,const) <- classDesc (AVar var)
            case const of 
              Just c -> subtypeErr va (fromAtomicType Nothing (AConst c)) t
                        "; the types are structurally distinct"
              Nothing -> case someFreeVarsIn vars t of
                           Just var' -> subtypeErr va (fromAtomicType Nothing (AVar var')) t $
                                        "; variable '" ++ show var' ++
                                       "' occurs in compound type '" ++ show t ++ "'"
                           Nothing -> do
                                 let vars' = Set.toList vars
                                     n = length vars'
                                 ts <- buildBinding va n t
                                 putBindings $ zip vars' ts
          (d1,d2) = (decomp t1, decomp t2) 

{-| This function calls 'typeErr' but with the current constraint's
position as context.  -}

typeErr' :: String -> MatchM a
typeErr' msg = getConstrPos >>= flip withPos' (typeErr msg)

{-| This function reports an error that states that the given
subtyping constraint cannot be satisfied. The subtyping constraint @t1
< t2@ if the variance argument is 'Covariant' and vice versa
otherwise, where @t1@, @t2@ are the first and the second type argument
to this function. -}

subtypeErr :: Variance -> PType -> PType -> String -> MatchM ()
subtypeErr v t1 t2 msg = typeErr' $ "'" ++ show s1 ++
                         "' cannot be a subtype of '"++ show s2 ++ "'"  ++ msg
    where (s1,s2) = case v of
                     Covariant -> (t1,t2)
                     Contravariant -> (t2,t1)


data Variance = Contravariant | Covariant

{-| This function reverses the given variance.  -}

revVar :: Variance -> Variance
revVar Contravariant = Covariant
revVar Covariant = Contravariant


{-| This function takes a type and builds a list of types of the same
shape but with atomic types replaced by a fresh variable. The
generated types are supposed to the constrained to be subtypes of the
given type if the variance argument is 'Covariant' and vice versa
otherwise. The corresponding atomic subtyping constraints are added to
the constraint store plus according equations in the equivalence
relation between atomic types. The integer argument determines how
many types to generate, i.e. the length of the returned list of
types. -}

buildBinding :: Variance -> Int -> PType -> MatchM [PType]
buildBinding va num t = case decomp t of 
                      DVar v -> bVar' (AVar v)
                      DFun SFun [t1, t2] -> do
                        t1' <- buildBinding' (revVar va)  t1
                        t2' <- buildBinding' va t2
                        return $ map (replaceArgs t) $ zipWith (\ x y -> [x,y]) t1' t2'
                      DFun _ args -> do
                        args' <- mapM (buildBinding' va) args
                        return $ map (replaceArgs t) (transpose args')
                      DConst s -> bVar' (AConst s)
    where bVar a = do
            v <- liftM (ann Nothing)freshTVar
            equate a (atomicType' "buildBinding" v)
            addSubConstr' va v t
            return v
          bVar' a = replicateM num $ bVar a
          buildBinding' va = buildBinding va num


{-| This function adds a binding to the current substitution.  -}

putBindings :: [(TVarId, PType)] -> MatchM ()
putBindings bind = do
  s <- getSubst
  putSubst (subst `compSubst` s)
  ws <- getWorkingSet
  putWorkingSet (appSubst subst ws)
      where subst = Map.fromList bind
            
            
{-| This function atomify non subtype constraints
-}

atomifyOthers :: (MonadTyping m) => [TypeConstrPos PType] -> m [AtomicTypeConstrPos a]
atomifyOthers cs = liftM concat $ mapM atomifyOther cs


atomifyOther :: (MonadTyping m) =>
           TypeConstrPos PType -> m [AtomicTypeConstrPos a]
atomifyOther (HasField record field t :&: pos) = withPos' pos $
  case decomp record of
    DVar tvar -> do 
      let a = inject (TVar tvar :&: pos) :: AtomicPType
      return [AHasField a field t :&: pos]
    DConst (SRecord name) -> do
      let a = inject (TRecord name :&: pos) :: AtomicPType
      return [AHasField a field t :&: pos]
    _ -> typeErr $ "Field constraint was applied to the non-record type '" 
         ++ show record ++ "'"
atomifyOther (Ord t :&: pos) = withPos' pos $ do
  when (containsFunction t) $
    typeErr $ "Type '"++ show t ++ "' contains a function type and is therefore not ordered" 
  when (containsRecord t) $
    typeErr $ "Type '"++ show t ++ "' contains a record type and is therefore not ordered" 
  let fvs = Set.toList (freeVars t) :: [TVarId] -- TODO freeVars should also return positions!
  let ord tvar = AOrd (inject $ TVar tvar :&: (Nothing :: SrcPos)) :&: pos -- TODO variable position is lost!
  return $ map ord fvs
atomifyOther (Eq t :&: pos) = withPos' pos $ do 
  when (containsRecord t) $
    typeErr $ "Type '"++ show t ++ "' contains a record type and cannot have equality" 
  let fvs = Set.toList (freeVars t) :: [TVarId]
  let eq tvar = AEq (inject $ TVar tvar :&: (Nothing :: SrcPos)) :&: pos -- TODO variable position is lost!
  return $ map eq fvs
atomifyOther (SubType {} :&: _) = error "Subtype constraint was not expected"
            

containsFunction :: PType -> Bool
containsFunction t = 
  case decomp t of
    DVar _ -> False
    DConst _ -> False
    DFun SFun _ -> True
    DFun _ ts -> or (map containsFunction ts)

containsRecord :: PType -> Bool
containsRecord t = 
  case decomp t of
    DVar _ -> False
    DConst (SRecord _) -> True
    DConst _ -> False
    DFun _ ts -> or (map containsFunction ts)
                                    