{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Entailment
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This modules provides a procedure for deciding entailment of type
-- constraints.
--
-- TODO: For now this is an ad-hoc hack. This needs a proper implementation!
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Entailment
    (entails
     ,entails'
    ) where
import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
import Poets.Reporting.Language.Parrot.Typing.Decomp
import Poets.Reporting.Language.Parrot.Typing.TypingMonad

import Poets.Data.Type.Utils hiding (isSubType)

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


{-| This type represents a nested table that indicates the fields of a
record type and their types.  -}

type HasFieldRel = Map AtomicType (Map FieldName PType)

{-|
  This type represents the "ordered" type predicate
-}

type OrdPred = Set TVarId


{-|
  This type represents the type predicate with "equality"
-}

type EqPred = Set TVarId

{-|
  This type represents a subtyping predicate
-}

type SubTypeRel = Map AtomicType (Set AtomicType)

{-| This data type represents the lookup table used for deciding the
entailment of type constraints.  -}

data Table = Table { superTypeRel ::SubTypeRel,
                     subTypeRelTgt ::SubTypeRel, -- subtypes according to the target constraints
                     ordPred :: OrdPred,
                     eqPred :: EqPred,
                     hasFieldRel :: HasFieldRel,
                     constrVars :: Set TVarId,
                     recordEnv :: ExtRecEnv}

{-| This monad transformer constructs monads maintaining a table used
for deciding type constraint entailment.  -}

type TableT = StateT Table

{-| This function decides whether (an instance of) the first list of
constraints entails the second one, given the provided record
environment. If this is the case the result is equivalent to @return
()@. Otherwise, a corresponding error is generated. -}

entails :: (MonadTyping m) => [TypeConstrPos PType] -> [TypeConstrPos PType] -> TableT m ()
entails cs1 cs2 = mkTable cs1 >>
                  mkTableTgt cs2 >>
                  checkSubConstr >>
                  mapM_ checkConstr cs2

{-| This function is similar to 'entails' but gets rid of the 'TableT'
monad transformer in the result.  -}

entails' :: (MonadTyping m) => ExtRecEnv -> Set TVarId -> [TypeConstrPos PType] -> [TypeConstrPos PType] -> m ()
entails' env vars cs1 cs2 = liftM fst $ runTableT env vars (entails cs1 cs2)

checkSubConstr :: (MonadTyping m) => TableT m ()
checkSubConstr = do
  tbl <- liftM subTypeRelTgt get
  tys <- filterM (liftM not . isUnconstrVar) $ Map.keys tbl
  let search ty = case Map.lookup ty tbl of
                    Nothing -> return Set.empty
                    Just s -> do
                      (uncst, constr) <- partitionM isUnconstrVar (Set.toList s)
                      res <- mapM search uncst
                      return $ Set.unions $ Set.fromList constr : res
      check ty = do
        subs <- liftM Set.toList $ search ty
        mapM_ (`isSubType` ty) subs
  mapM_ check tys
        
  
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ [] = return ([],[])
partitionM p (x:xs) = do res <- p x
                         (yes,no) <- partitionM p xs
                         return $ if res then (x:yes,no)
                                         else (yes,x:no)

checkConstr :: (MonadTyping m) =>  TypeConstrPos PType -> TableT m ()
checkConstr (HasField t1 field t2 :&: p) = withPos' p $ hasField' t1 field t2
checkConstr (Ord t :&: p) = withPos' p $ isOrd' t
checkConstr (Eq t :&: p) = withPos' p $ isEq' t
checkConstr (SubType {} :&: _) = return ()



hasField' :: (MonadTyping m) => PType -> FieldName -> PType -> TableT m ()
hasField' t1 n t2 = do 
  t1' <- atomicType "hasField" t1
  hasField t1' n t2

hasField :: (MonadTyping m) => AtomicType -> FieldName -> PType -> TableT m ()
hasField t1 field t2  = do
  uncst <- liftM2 (||) (isUnconstrVar t1) (isUnconstrVar' t2)
  if uncst
    then allowsField t1 field t2
    else do
      tbl <- liftM hasFieldRel get
      case Map.lookup t1 tbl of
        Nothing -> typeErr $ "type '" ++ show t1 ++ "' is not a record type"
        Just tbl' ->
            case Map.lookup field tbl' of
              Nothing ->
                  case t1 of
                    AVar _ -> typeErr $ "type '" ++ show t1 ++ "' has no field '" ++ field ++ "'"
                    AConst (SRecord _) -> typeErr $ "record '" ++ show t1
                                          ++ "' has no field '"++ field ++ "'"
                    _ -> typeErr $ "type '" ++ show t1 ++ "' is not a record type"
              Just t2' -> do
                  unless (t2 == t2') $
                    typeErr $ "field '" ++ field ++ "' of type '" ++ show t1
                           ++ "' has type '" ++ show t2' ++ "', not type '" ++ show t2 ++ "'"

isEq' :: (MonadTyping m) => PType -> TableT m ()
isEq' t =  atomicType "isEq" t >>= isEq

isEq :: (MonadTyping m) => AtomicType -> TableT m ()
isEq (AConst _) = return ()
isEq (AVar v) = do
  tbl1 <- liftM ordPred get
  tbl2 <- liftM eqPred get
  constr <- isConstrVar v
  if constr
    then unless (v `Set.member` tbl1 || v `Set.member` tbl2) $ 
         typeErr $ "type '" ++ show v ++ "' does not have equality"
    else allowsEq v

isOrd' :: (MonadTyping m) => PType -> TableT m ()
isOrd' t =  atomicType "isOrd" t >>= isOrd

isOrd :: (MonadTyping m) => AtomicType -> TableT m ()
isOrd (AConst _) = return ()
isOrd (AVar v) = do
  tbl <- liftM ordPred get
  constr <- isConstrVar v
  if constr
    then unless (v `Set.member` tbl) $ typeErr $ "type '" ++ show v ++ "' is not ordered"
    else allowsOrd v

isSubType :: (MonadTyping m) => AtomicType -> AtomicType -> TableT m ()
isSubType ty1 ty2 = do
      tbl <- liftM superTypeRel get
      unless (search tbl ty1 ty2) $
        typeErr $ "'" ++ show ty1 ++ "' is not a subtype of '" ++ show ty2 ++ "'"
    where search tbl t1 t2 
              | t1 == t2 = True
              | otherwise = case Map.lookup t1 tbl of
                              Nothing -> False
                              Just s -> Set.member t2 s
                                        || any (\ x-> search tbl x t2) (Set.toList s)

allowsField :: (MonadTyping m) => AtomicType -> FieldName -> PType -> TableT m ()
allowsField ty fname ftype = do
  subs <- liftM Set.toList $ getGreatestSubTypeConstants ty
  env <- liftM recordEnv get
  let hasField (SRecord rname) = 
        case getTypeFields env rname of
          Left _ -> typeErr $ "record type '" ++ rname ++ "' is not defined"
          Right fenv -> case getFieldInfo fenv fname of
            Left _ -> typeErr $ "type '" ++ show ty ++
                      "' cannot be instantiated to a type with field '" ++ fname ++ "'"
            Right Field{fieldType = dftype} -> do 
                   succ <- tryMatch ftype dftype
                   unless succ $
                      typeErr $ "type '" ++ show ty ++
                      "' cannot be instantiated to a type with field '" ++ fname ++ 
                      "' of type '" ++ show ftype ++ "'"
      hasField SDateTime | fname == "date"
                  = do succ <- tryMatch ftype (inject (TDate :&: (Nothing :: SrcPos)))
                       unless succ $
                         typeErr $ show ty ++
                          "' cannot be instantiated to a type with field '" ++ fname ++ 
                          "' of type '" ++ show ftype ++ "'"
      hasField SDateTime | fname == "time"
                  = do succ <- tryMatch ftype (inject (TTime :&: (Nothing :: SrcPos)))
                       unless succ $
                         typeErr $ show ty ++
                          "' cannot be instantiated to a type with field '" ++ fname ++ 
                          "' of type '" ++ show ftype ++ "'"
      hasField ty | ty `elem` [SDuration,SDate, SDateTime,STime , SDurationDate]
                  = do  succ1 <- tryMatch ftype (inject (TInt :&: (Nothing :: SrcPos)))
                        let succ = succ1 && case ty of
                              SDuration     -> fname `Set.member` durationFieldSet
                              SDateTime     -> fname `Set.member` dateTimeFieldSet
                              SDate         -> fname `Set.member` dateFieldSet
                              STime         -> fname `Set.member` timeFieldSet
                              SDurationDate -> fname `Set.member` durationTimeFieldSet
                              _             -> False
                        unless succ $
                         typeErr $ show ty ++
                          "' cannot be instantiated to a type with field '" ++ fname ++ 
                          "' of type '" ++ show ftype ++ "'"
      hasField ty = typeErr $ "type '" ++ show ty ++
                      "' cannot be instantiated to a record type"
  mapM_ hasField subs

allowsOrd :: (MonadTyping m) => TVarId -> TableT m ()
allowsOrd _ = return ()
  

allowsEq :: (MonadTyping m) => TVarId -> TableT m ()
allowsEq _ = return ()

getGreatestSubTypeConstants :: (MonadTyping m) => AtomicType -> TableT m (Set ConstSym)
getGreatestSubTypeConstants (AConst c) = return $ Set.singleton c
getGreatestSubTypeConstants (AVar var) = do
  tbl <- liftM  subTypeRelTgt get
  let search var = case Map.lookup (AVar var) tbl of
                    Nothing -> Set.empty
                    Just s -> Set.unions $ Set.fromList consts : res
                        where subs = Set.toList s
                              (consts,vars) = partitionTypes subs
                              res = map search vars
  return $ search var
          
            


addField' :: (MonadTyping m) => PType -> FieldName -> PType -> TableT m ()
addField' t1 f t2 = join $ liftM3 addField (atomicType "addField" t1) (return f) (return t2)

addField :: (MonadTyping m) => AtomicType -> FieldName -> PType -> TableT m ()
addField t1 field t2 = modify change
    where change tbl@Table{hasFieldRel = rel} = 
              case Map.lookup t1 rel of 
                Nothing -> tbl {hasFieldRel = Map.insert t1 (Map.fromList [(field,t2)]) rel}
                Just fields -> tbl {hasFieldRel = Map.insert t1 (Map.insert field t2 fields) rel}

addOrd' :: (MonadTyping m) => PType -> TableT m ()
addOrd' t = join $ liftM addOrd (atomicType "addOrd" t) 

addOrd :: (MonadTyping m) => AtomicType -> TableT m ()
addOrd (AConst _) = return ()
addOrd (AVar v) = modify change
    where change tbl@Table{ordPred = ord} = tbl {ordPred = Set.insert v ord}

addEq' :: (MonadTyping m) => PType -> TableT m ()
addEq' t = join $ liftM addEq (atomicType "addEq" t) 

addEq :: (MonadTyping m) => AtomicType -> TableT m ()
addEq (AConst _) = return ()
addEq (AVar v) = modify change
    where change tbl@Table{eqPred = eq} = tbl {eqPred = Set.insert v eq}


addSubtype' :: (MonadTyping m) => PType -> PType -> TableT m ()
addSubtype' t1 t2 = join $ liftM2 addSubtype (atomicType "addSubtype" t1) (atomicType "addSubtype" t2)

addSubtype :: (MonadTyping m) => AtomicType -> AtomicType -> TableT m ()
addSubtype t1 t2 = modify change
    where change tbl@Table{superTypeRel = super, hasFieldRel = frel}
              = let newSuperRel = 
                        case Map.lookup t1 super of
                          Nothing -> Map.insert t1 (Set.singleton t2) super
                          Just s ->  Map.insert t1 (Set.insert t2 s) super
                    newFrel = case Map.lookup t2 frel of
                                Nothing -> frel
                                Just tbl ->
                                    case Map.lookup t1 frel of
                                      Nothing -> Map.insert t1 tbl frel
                                      Just tbl' -> Map.insert t1  (tbl `Map.union` tbl') frel
                in tbl {superTypeRel = newSuperRel, hasFieldRel = newFrel}

isUnconstrVar :: (MonadTyping m) => AtomicType -> TableT m Bool
isUnconstrVar t = case t of
                   AVar v -> liftM not $ isConstrVar v
                   _ -> return False

isUnconstrVar' :: (MonadTyping m) => PType -> TableT m Bool
isUnconstrVar' t = case project t  of
                   Just(TVar v :&: x) -> liftM not $ isConstrVar v
                       where _ = x :: SrcPos
                   _ -> return False
tryMatch :: (MonadTyping m) => PType -> PType -> TableT m Bool
tryMatch ftype dftype =
    case (decomp ftype, decomp dftype) of
      (DVar v, dc) -> do 
        con <- isConstrVar v
        return (not con || dc == DVar v)
      (DConst d1, DConst d2) -> return (d1 == d2)
      (DFun f1 args1, DFun f2 args2) -> if f1 == f2
                                        then liftM and $ sequence $ zipWith tryMatch args1 args2
                                        else return False
      _ -> return False

isConstrVar :: (MonadTyping m) => TVarId -> TableT m Bool
isConstrVar v = do
  vars <- liftM constrVars get
  return (Set.member v vars)


runTableT :: (MonadTyping m) => ExtRecEnv -> Set TVarId -> TableT m a -> m (a,Table)
runTableT env vars m = runStateT m Table
              { superTypeRel = Map.empty,
                subTypeRelTgt = Map.empty,
                ordPred = Set.empty,
                eqPred = Set.empty,
                hasFieldRel = Map.fromList [(AConst SDuration,durFields), 
                                            (AConst SDate, dateFields),
                                            (AConst SDurationDate, ddFields)],
                constrVars = vars,
                recordEnv = env} where
  intType = inject (TInt :&: (Nothing :: SrcPos))
  dateType = inject (TDate :&: (Nothing :: SrcPos))  :: PType
  timeType = inject (TTime :&: (Nothing :: SrcPos))  :: PType
  durFields = Map.fromList [(f,intType) | f <- durationFieldList]
  dateFields = Map.fromList $ ("date",dateType) : ("time",timeType) : 
               [(f,intType) | f <- timeFieldList]
  ddFields = Map.fromList [(f,intType) | f <- durationTimeFieldList]

mkTableTgt :: (MonadTyping m) => [TypeConstrPos PType] -> TableT m ()
mkTableTgt cs = mapM_ addSubs cs
    where addSubs (SubType t1 t2 :&: p) = withPos' p $ do
             t1' <- atomicType "mkTableTgt" t1
             t2' <- atomicType "mkTableTgt" t2
             modify (change t1' t2')
          addSubs _ = return ()
          change t1 t2 tbl@Table{subTypeRelTgt = sub}
              = let newSubRel = 
                        case Map.lookup t2 sub of
                          Nothing -> Map.insert t2 (Set.singleton t1) sub
                          Just s ->  Map.insert t2 (Set.insert t1 s) sub
                in tbl {subTypeRelTgt = newSubRel}

mkTable :: (MonadTyping m) => [TypeConstrPos PType] -> TableT m ()
mkTable cs = mkBaseTable >> mapM_ mkConstrTable cs

mkConstrTable :: (MonadTyping m) => TypeConstrPos PType -> TableT m ()
mkConstrTable (SubType t1 t2 :&: p) = withPos' p $
                                  addSubtype' t1 t2
mkConstrTable (Ord t :&: p) = withPos' p $
                          addOrd' t
mkConstrTable (Eq t :&: p) = withPos' p $
                          addEq' t
mkConstrTable (HasField t1 field t2 :&: p) = withPos' p $
                                         addField' t1 field t2

mkBaseTable  :: (MonadTyping m) => TableT m ()
mkBaseTable = do
  env <- liftM recordEnv get
  mapM_ run (recordEnvList env)
  builtinTypes
    where run Record{recordName = recname, recordFields = fs, recordExtends = ext} = do
            let rec = AConst $ SRecord recname
            mapM_ (\sup -> addSubtype rec $ AConst $ SRecord sup) (Set.toList ext)
            mapM_ (addField' rec) (fieldEnvList fs)
          addField' rec Field{fieldName = name, fieldType = ty} = addField rec name ty
          builtinTypes = addSubtype (AConst SInt) (AConst SReal)
                         >> addSubtype (AConst SDate) (AConst SDurationDate)
                         >> addSubtype (AConst SDuration) (AConst SDurationDate)
