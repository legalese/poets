{-# LANGUAGE
  MultiParamTypeClasses,
  GeneralizedNewtypeDeriving,
  TypeOperators,
  FlexibleInstances,
  FlexibleContexts,
  UndecidableInstances,
  TypeSynonymInstances,
  OverlappingInstances,
  TemplateHaskell
  #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Inference
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements the type inference for the Parrot reporting
-- language.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Inference  where

import Poets.Reporting.Language.Parrot.FreshVar
import Poets.Reporting.Language.Parrot.Typing.BasicType
import Poets.Reporting.Language.Parrot.Typing.Instance
import Poets.Reporting.Language.Parrot.FreeVars
import Poets.Reporting.Language.Parrot.Typing.TypingMonad
import Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
import Poets.Reporting.Language.Parrot.Typing.Simplify.Simplify
import Poets.Reporting.Language.Parrot.Typing.Decomp
import Poets.Reporting.Language.Parrot.Typing.PolyType
import Poets.Reporting.Language.Parrot.Syntax

import Poets.Reporting.Language.Parrot.ReportLibrary

import Poets.Data.Type.Utils
import Poets.Reporting.Language.Parrot.Typing.TypeAlias

import Control.Monad hiding (mapM, sequence, mapM_)
import Control.Monad.Reader hiding (mapM, sequence, mapM_)
import Control.Monad.State hiding (mapM, sequence, mapM_)

import Prelude  hiding (mapM, sequence, foldr, foldl, foldr1, mapM_)

import qualified Data.Comp.Unification as U (unify)
import Data.Comp.Variables
import Data.Comp.Ops
import Data.Foldable

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Traversable


type TypeEnv = Map VVarId PolyType

type TypeEnv' = Map VVarId TypeScheme

type InfType = Context TypeSig PType

freshInst :: PolyType -> InferM PType 
freshInst PolyType { ptForall = univ, ptConstrs = cs, ptType = ty } = do
  let univL = Set.toList univ
  fvars <- mapM liftType =<< freshTVars (length univL) 
  let subst = Map.fromList $ zip univL fvars
      cs' = map (fmap (appSubst subst)) cs
  addConstraints cs'
  return $ appSubst subst ty
  

type InferState = [TypeConstrPos PType]
data InferEnv = InferEnv {
      typeEnv :: TypeEnv,
      recEnv :: ExtRecEnv
    }

type PSubst = Subst TypeSigPos TVarId
type PSubst' = CxtSubst Hole PType TypeSig TVarId


data InferTopState = InferTopState {
     itsRecords :: ExtRecEnv ,
     itsNewRecords :: [ExtRecord],
     itsTypeEnv :: TypeEnv,
     itsPreludeFuns :: Set FunId,
     itsFunTypes :: Map FunId TypeScheme
    }

newtype  InferTopM a = InferTopM {unTypeM :: StateT InferTopState (FreshVarT TypingM) a}
    deriving (Monad, Functor, MonadState InferTopState, MonadTyping, MonadFreshVar TVarId)


runInferTopM :: ExtRecEnv -> Maybe ReportLibrary
             -> InferTopM a -> Either TypingErr (a, InferTopState)
runInferTopM recEnv libm =  runTypingM . runFreshVarT . (`runStateT` state libm) . unTypeM
    where state Nothing = InferTopState recEnv [] Map.empty Set.empty Map.empty
          state (Just ReportLibrary{libRecords = recs, libTypings = typings, libFunctions = funs})
                       = InferTopState (addRecordInfos recEnv recs) [] typings funs Map.empty
                       
addRecord :: ExtRecord -> InferTopM ()
addRecord r = checkRecord r >> modify change
    where change s = s {itsNewRecords = r : itsNewRecords s,
                        itsRecords = addRecordInfo (itsRecords s) r}

checkRecord :: ExtRecord -> InferTopM ()
checkRecord r = mapM_ checkField $ fieldEnvList (recordFields r)
    where checkField f = checkType $ fieldType f
          checkType t = case decomp t of
                          DFun SFun _ -> typeErr $ "record fields must not contain function types"
                          DFun _ ts -> mapM_ checkType ts
                          _ -> return ()

addFunction :: FunId -> TypeScheme -> InferTopM ()
addFunction fun ty = check >> modify change
    where change s = s {itsFunTypes = Map.insert fun ty (itsFunTypes s),
                        itsTypeEnv = Map.insert (vVarId fun) (fromTypeScheme ty) (itsTypeEnv s) }
          check = do 
            InferTopState {itsPreludeFuns = pfuns, itsFunTypes = funs } <- get
            when (Map.member fun funs) $ typeErr $ "function " ++ fun ++ " has already been defined"
            when (Set.member fun pfuns) $ typeErr $ "function " ++ fun ++ " is already defined in the Prelude"
            return ()
            

isTypeSchemeValid :: TypeScheme -> InferTopM ()
isTypeSchemeValid (TypeScheme pos constrs ty) = withPos' pos $ 
  mapM_ isTypeConstrValid constrs >> isTypeValid ty
  

isTypeValid :: PType -> InferTopM ()
isTypeValid ty = do
  recEnv <- liftM itsRecords get
  case undefinedRecord recEnv (stripA ty) of
    Just name -> typeErr $ "record type '" ++ name ++ "' is not defined"
    Nothing -> return ()
  

isTypeConstrValid :: TypeConstrPos PType -> InferTopM ()
isTypeConstrValid (SubType t1 t2 :&: pos) = withPos' pos $ isTypeValid t1 >> isTypeValid t2
isTypeConstrValid (HasField t1 _ t2 :&: pos) = withPos' pos $ isTypeValid t1 >> isTypeValid t2
isTypeConstrValid (Ord ty :&: pos) = withPos' pos $ isTypeValid ty
isTypeConstrValid (Eq ty :&: pos) = withPos' pos $ isTypeValid ty

typeDecls :: POETSRecordEnv -> Maybe ReportLibrary
          -> [DeclSugar] -> Either TypingErr ([ExtRecord],Map FunId TypeScheme)
typeDecls recEnv lib decls = do 
  ((),st) <- runInferTopM recEnv' lib $ inferDecls decls
  return (itsNewRecords st, itsFunTypes st)
    where recEnv' = fromBasicType' recEnv

inferDecls :: [DeclSugar] -> InferTopM ()
inferDecls decls = mapM_ inferDecl decls



inferDecl :: DeclSugar -> InferTopM ()
inferDecl (FunDecl pos funId tyAn args body) = withPos' pos $ do
  ty <- inferFun args body
  recEnv <- liftM itsRecords get
  case tyAn of
    Nothing -> simplifyTypeScheme True recEnv ty >>= done
    Just tyAn -> do 
            isTypeSchemeValid tyAn
            let tyAn' = resolveAlias tyAn
            env <- liftM itsRecords get
            isInstance' funId env tyAn' ty
            done tyAn'
    where done ty = addFunction funId ty
inferDecl (RecDecl pos name ext fdecls) = withPos' pos $ addRecord rec
    where rec = Record { recordName = name,
                         recordFields = newFieldEnv fs,
                         recordExtends = Set.fromList ext,
                         recordAttributes = Set.empty
                       } 
          fs = map getField fdecls
          getField (FieldDecl _ name ty) = Field { fieldName = name,
                                                   fieldType = ty,
                                                   fieldAttributes = Set.empty}

newtype InferM a = InferM {unInferM :: ReaderT InferEnv(StateT InferState (FreshVarT TypingM)) a}
    deriving (Monad, Functor, MonadReader InferEnv, MonadState InferState,
              MonadTyping, MonadFreshVar TVarId)

liftTopM :: (FreshVarT TypingM) a -> InferTopM a
liftTopM = InferTopM . lift


inferFun :: (InferF' f, Functor f) => [VVarId] -> Term f -> InferTopM TypeScheme
inferFun vars body = do
   InferTopState {itsRecords =recEnv, itsTypeEnv = typeEnv} <- get
   (ty,cs) <- liftTopM $ runInferM recEnv typeEnv $ liftType =<< inferLam vars (inferExp body)
   return $ TypeScheme Nothing cs ty

runInferM :: ExtRecEnv -> TypeEnv -> InferM a -> (FreshVarT TypingM) (a, [TypeConstrPos PType])
runInferM recEnv typeEnv m = 
    (`runStateT` []) .
    (`runReaderT` InferEnv typeEnv recEnv) .
    unInferM $ m

addConstraints :: [TypeConstrPos PType] -> InferM ()
addConstraints cs = InferM $ modify (cs ++)

addConstraints' :: [TypeConstr InfType] -> InferM ()
addConstraints' cs = do 
  cs' <- mapM liftConstr cs
  addConstraints cs'

{-| This function extracts the list of constraints that were generated
during the monadic computation.  -}

extractConstraints :: InferM a -> InferM (a,[TypeConstrPos PType])
extractConstraints m = do
  oldCs <- get
  put []
  res <- m
  newCs <- get
  put oldCs
  return (res,newCs)


mkPolyType :: InferM PType -> InferM PolyType
mkPolyType mty = do
  (ty, cs) <- extractConstraints mty
  envVars <- getEnvTypeVars
  let fvs = (freeVars cs `Set.union` freeVars ty) `Set.difference` envVars
  return $ PolyType fvs cs ty
  

liftConstr :: TypeConstr InfType -> InferM (TypeConstrPos PType)
liftConstr c = do pos <- getPos
                  return $ (:&: pos) $ fmap (appCxt . ann pos) c

liftType :: InfType -> InferM PType
liftType ty = liftM (appCxt . (`ann` ty)) getPos

withTyping :: VVarId -> PolyType -> InferM a -> InferM a
withTyping var ty m = local change m
    where change env = env {typeEnv = Map.insert var ty $ typeEnv env}

withTypings :: [(VVarId, PolyType)] -> InferM a -> InferM a
withTypings binds m = local change m
    where change env = env {typeEnv = Map.union (Map.fromList binds) $ typeEnv env}

getEnvTypeVars :: InferM (Set TVarId)
getEnvTypeVars = liftM (freeVars . typeEnv) ask


lookupTyping :: VVarId -> InferM PolyType
lookupTyping var = do
  env <- liftM typeEnv ask 
  case Map.lookup var env of
    Nothing -> typeErr $ "variable '" ++ show var ++ "' is not in scope"
    Just t -> return t

lookupRecord :: RecordName -> InferM ExtRecord
lookupRecord name = do
  r <- liftM recEnv ask
  liftTypeErr $ getFullRecordInfo r name

lookupRecordFields :: RecordName -> InferM (FieldEnv PType)
lookupRecordFields name = do
  r <- liftM recEnv ask
  liftTypeErr $ getFields r name



unify :: [(PType,PType)] -> InferM PSubst
unify eqs = do 
  subst <- liftUnifErr $ U.unify eqs
  appSubstConstrs subst
  return subst

appSubstConstrs :: PSubst -> InferM ()
appSubstConstrs subst = InferM $ modify (map (fmap (appSubst subst)))


equateAll :: [a] -> [(a,a)]
equateAll [] = []
equateAll [_] = []
equateAll (x:xs@(y:_)) = (x,y) : equateAll xs


class InferF f where 
    inferF :: f (InferM PType) -> InferM InfType


class InferF' f where 
    inferF' :: f (InferM PType) -> InferM PType

instance InferF f => InferF' (f :&: SrcPos) where
    inferF' (v :&: pos) = liftM (appCxt . ann pos) $ withPos' pos $ inferF v

instance (InferF' f1, InferF' f2) => InferF' (f1 :+: f2) where
    inferF' (Inl v) = inferF' v
    inferF' (Inr v) = inferF' v


inferExp :: (Functor f, InferF' f) => Term f -> InferM PType
inferExp = cata inferF'


instance (InferF f1, InferF f2) => InferF (f1 :+: f2) where
    inferF (Inl v) = inferF v
    inferF (Inr v) = inferF v

instance InferF Val where
    inferF (VInt _) = inferTypeConst TInt
    inferF (VBool _) = inferTypeConst TBool
    inferF (VString _) = inferTypeConst' $ iTList $ iTChar
    inferF (VDateTime _) = inferTypeConst TDateTime
    inferF (VDate _) = inferTypeConst TDate
    inferF (VTime _) = inferTypeConst TTime
    inferF (VDuration _) = inferTypeConst TDuration
    inferF (VReal _) = inferTypeConst TReal
    inferF (VEnt VEntity{ventType = rname}) =  inferTypeConst' $ iTEnt $ iTRecord rname
    inferF (VList tys) = do
      t <- freshTVar
      inferOp' tys [] (replicate (length tys) t) (iTList t)
    inferF (VRecord (VR recName mfields)) =
        do rec <- lookupRecord recName
           when (isAbstract rec) $
                typeErr $ "record type '" ++ recName
                            ++ "' is abstract and therefore does not "
                            ++ "allow direct construction of values"
           
           fieldDecls <- liftM fieldEnvSorted $ lookupRecordFields recName
           fields <- mapM sequence $ fieldsSorted mfields
           aligned <- zipWithM align fields fieldDecls
           let (args,argTypes) = unzip aligned
           inferOp args [] (map Hole argTypes) (iTRecord recName)
        where align (VF name r) Field {fieldName = name', fieldType = ty} =
                                 case compare name name' of
                                   LT -> typeErr $ "Field '" ++ name
                                         ++ "' is not declared for record '" ++ recName ++ "'"
                                   GT -> typeErr $ "Field '" ++ name'
                                         ++ "' of record '" ++ recName
                                         ++ "' has not been assigned a value"
                                   EQ -> return (r,ty)

instance InferF ValueExt where
    inferF (VVar var) = do
      pType <- lookupTyping var
      typ <- freshInst pType
      a <- freshTVar
      addConstraints' [SubType (Hole typ) a]
      return a
    inferF (VLam vars body) = inferLam vars body
    inferF (VChar _) = inferTypeConst TChar
    inferF (VLeft arg) = do
      [a,b] <- freshTVars 2
      inferOp' [arg] [] [a] (a `iTSum` b)
    inferF (VRight arg) = do
      [a,b] <- freshTVars 2
      inferOp' [arg] [] [b] (a `iTSum` b)
    inferF (VTuple xs) = do 
      as <- freshTVars (length xs)
      inferOp' xs [] as (appCxt $ gProd as)
    inferF (VUnit) = inferTypeConst TUnit

instance InferF ExpExt where
    inferF (App e1 e2) = do
      [a,b] <- freshTVars 2
      inferOp' [e1, e2] [] [a `iTFun` b, a] b
    inferF (BinOpCore e1 op e2) = do
      FunctionType cs args ret <- operatorType (Left op)
      inferOp' [e1,e2] cs args ret
    inferF (Let binds body) = inferBinds binds
        where inferBinds [] = liftM Hole body
              inferBinds ((b :&: pos):bs) = do
                pty <- withPos' pos $
                       mkPolyType $
                       liftType =<< inferLam (letBindArgs b) (letBindBody b)
                withTyping (letBindVar b) pty (inferBinds bs)
    inferF (RecAcc e fname) = do
        [a,b] <- freshTVars 2
        inferOp' [e] [HasField a fname b] [a] b
    inferF (RecMod e mods) = do
        as <- freshTVars (length mods)
        b <- freshTVar
        let mkConstr a (v :&: _) = HasField b (vfieldName v) a
            mkArg (v :&: p) = withPos' p (vfieldValue v)
            constrs = zipWith mkConstr as mods
            args = map mkArg mods
        inferOp' (e:args) constrs (b:as) b
    inferF (DateTime e1 e2) = inferOp' targs [] types iTDateTime
        where targs = [e1, e2]
              types = [iTDate, iTTime]
    inferF (Date (DayExp e1 e2 e3)) = inferOp' targs [] types iTDate
        where targs = [e1, e2, e3]
              types = replicate (length targs) iTInt
    inferF (Time (TimeExp e4 e5 e6)) = inferOp' targs [] types iTTime
        where targs = [e4, e5] ++ maybe [] (:[]) e6
              types = replicate (length targs) iTInt
    inferF (Duration dure) = inferOp' targs [] types iTDuration
        where targs = catMaybes $ toList dure
              types = replicate (length targs) iTInt
    inferF (If cond ifb thenb) = do
        a <- freshTVar
        inferOp' [cond,ifb,thenb] [] [iTBool, a, a] a
    inferF (PrimOp op) = inferPrimOp op
    inferF (Proj e (ProjComp proj)) = do
        when (proj <= 0) $ typeErr "projections have to be non-negative integers"
        a : r : as <- freshTVars (proj + 1)
        inferOp' [e] [] [openProd a r as] a where
                 openProd a b [] = iTProd a b
                 openProd a b (t : ts) = iTProd t (openProd a b ts)
    inferF (Deref e _) = do
      a <- freshTVar
      inferOp'[e] [] [iTEnt a] a
    inferF (TypeOf mvar exp cases mdefCase) = do 
      typ <- exp
      tys <- mapM (inferCase typ) cases
      dtys <- inferDefCase typ mdefCase
      let btys = dtys ++ tys
      s <- unify $ equateAll btys
      case btys of 
        t : _ -> return $ Hole $ appSubst s t
        _ -> typeErr $ "type distinction has no cases"
        where inferCase typ (TypeCase rname body :&: pos) = withPos' pos $ do 
                checkRecDefined $ refRecName rname
                rtype <- liftType $ iTEntRec rname
                btype <- withTyping' mvar (mkMonoType rtype) body
                addConstraints' [Hole rtype `SubType` Hole typ]
                return btype
              inferDefCase _ Nothing = return []
              inferDefCase typ (Just (DefTypeCase body :&: pos)) = liftM (:[]) $
                  withPos' pos $ withTyping' mvar (mkMonoType typ) body
              withTyping' Nothing _ = id
              withTyping' (Just v) t = withTyping v t


instance InferF ExpSugar where
    inferF (BinOpSugar e1 op e2) = do
      FunctionType sub arg ret <- operatorType (Right op)
      inferOp' [e1,e2] sub arg ret
    inferF (TypePred e rec) = do
      checkRecDefined $ refRecName rec
      a <- freshTVar
      inferOp' [e] [iTEntRec rec `SubType` a] [a] iTBool
    inferF (Pick rec) = do
      checkRecDefined $ refRecName rec
      a <- freshTVar
      let rtype = iTEntRec rec
      inferOp [] [rtype `SubType` a] []  (iTList a `iTFun` iTList rtype)
    inferF (ListComprehension res filters) = run filters
      where
          run [] = liftM (iTList . Hole) res
          run (ListGuard e : filters) = do
              ty <- e
              tyBool <- liftType iTBool
              unify [(ty,tyBool)]
              run filters
          run (ListLet var rname e : filters) = 
              run' id var rname e filters
          run (ListGenerator var rname e : filters) = run' iTList var rname e filters
          run' tyCon var rname e filters = do
              eType <- e
              rType <- case rname of
                         Just name -> addConstraints' [tyCon (iTEntRec name) `SubType`  Hole eType]
                                     >> return (iTEntRec name)
                         Nothing -> do
                                    a <- freshTVar
                                    t <- liftType $ tyCon a
                                    subst <- unify [(eType, t)]
                                    return (appSubst subst a)
              rType' <- liftType rType
              withTyping var (mkMonoType rType') $ run filters

    inferF (OperatorSection section) = case section of 
        NormalSection op side -> operatorType (Left op) >>= inferSection side
        TypePredSection rname -> do
            t <- freshTVar
            addConstraints' [iTEntRec rname `SubType` t]
            return (t `iTFun` iTBool)
        where
            inferSection side (FunctionType cs [arg1, arg2] ret) = do
                case side of
                    LeftOperand e -> inferOp' [e] cs [arg1] (arg2 `iTFun` ret)
                    RightOperand e -> inferOp' [e] cs [arg2] (arg1 `iTFun` ret)
                    NoOperand -> inferOp [] cs [] (foldr1 iTFun [arg1, arg2, ret])
            inferSection _ _ = error "expected two arguments for binary operator"

{-| This function checks whether the given record is defined. If not,
a type error is raised.  -}

checkRecDefined :: RecordName -> InferM ()
checkRecDefined name = do
  def <- liftM ((`isDefined` name) . recEnv) ask
  unless def $ typeErr $ "record type '" ++ name ++ "' is not defined"

inferPrimOp  :: PrimOp -> InferM InfType
inferPrimOp Fold = do
  [a,b] <- freshTVars 2
  inferTypeConst' $ foldr1 iTFun [a `iTFun` (b `iTFun` b), b, iTList a, b]
inferPrimOp Not = inferTypeConst' $ iTBool `iTFun` iTBool
inferPrimOp Case = do
  [a,b,c] <- freshTVars 3
  inferTypeConst' $ foldr1 iTFun [a `iTSum` b, a `iTFun` c, b `iTFun` c, c]
inferPrimOp LeftOp = do
  [a,b] <- freshTVars 2
  inferTypeConst' $ a `iTFun` (a `iTSum` b)
inferPrimOp RightOp = do
  [a,b] <- freshTVars 2
  inferTypeConst' $ b `iTFun` (a `iTSum` b)
inferPrimOp Events = inferTypeConst' $ iTList (iTRecord "Event")
inferPrimOp Error = do
  a <- freshTVar
  inferTypeConst' $ (iTList iTChar) `iTFun` a

{-| This data type is used to pass function types around -}

data FunctionType = FunctionType
    [TypeConstr InfType]
    [InfType]
    InfType

operatorType :: (MonadFreshVar TVarId m, MonadTyping m) => 
                Either BinOpCore BinOpSugar -> m FunctionType
operatorType op = do
      a <- freshTVar
      let listty = iTList a
      let plusop = return (FunctionType [SubType a iTReal] [a,a] a)
          divop  = return (FunctionType [] [iTReal, iTReal] iTReal)
          boolop = return (FunctionType [] [iTBool,iTBool] iTBool)
          ordop  = return (FunctionType [Ord a] [a,a] iTBool)
          eqop  = return (FunctionType [Eq a] [a,a] iTBool)
          durPlusop = return (FunctionType [SubType a iTDurationDate] [a,iTDuration] a)
          appendop = return (FunctionType [] [listty, listty] listty)
          consop   = return (FunctionType [] [a, listty] listty)
      case op of
        Left coreOp ->
            case coreOp of
              OpPlus -> plusop
              OpMinus -> plusop
              OpTimes -> plusop
              OpDiv -> divop
              OpAnd -> boolop
              OpOr ->  boolop
              OpNeq -> eqop
              OpEq -> eqop
              OpLt -> ordop
              OpLe -> ordop
              OpGt -> ordop
              OpGe -> ordop
              OpDurPlus -> durPlusop
              OpDurMinus -> durPlusop
              OpCons -> consop
        Right sugarOp ->
            case sugarOp of
              OpAppend -> appendop


inferTypeConst' :: InfType -> InferM InfType
inferTypeConst' g = do
  a <- freshTVar
  addConstraints' [g `SubType` a ]
  return a
                         
inferTypeConst :: (v :<: TypeSig) => v PType -> InferM InfType
inferTypeConst g = do
  a <- freshTVar
  addConstraints' [simpCxt (inj g) `SubType` a ]
  return a

{-| This function infers the type of an lambda abstraction given the
principal types of the body of the lambda abstraction and the list of
the variables that it abstracts from.  -}

inferLam :: [VVarId] -> InferM PType -> InferM InfType
inferLam vars mtype = do
  as <- freshTVars (length vars)
  ptys <- mapM (liftM mkMonoType . liftType) as
  let typings = zip vars ptys
  ty <- withTypings typings mtype
  return $ foldr iTFun (Hole ty) as

inferOp' :: [InferM PType]
         -> [TypeConstr InfType]
         -> [InfType]
         -> InfType
         -> InferM InfType
inferOp' margs cs expArgs ret = do
  args <- sequence margs
  inferOp args cs expArgs ret

inferOp :: [PType]
        -> [TypeConstr InfType]
        -> [InfType]
        -> InfType -> InferM InfType
inferOp args cs expArgs ret = do
  addConstraints' cs
  expArgs' <- mapM liftType expArgs
  s1 <- unify $ zip args expArgs'
  ret' <- liftM (Hole . appSubst s1) $ liftType ret
  a <- freshTVar
  addConstraints' [ret' `SubType` a]
  return a
