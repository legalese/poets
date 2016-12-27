{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  FlexibleContexts,
  TypeSynonymInstances,
  RankNTypes,
  TypeOperators,
  TemplateHaskell,
  DeriveFunctor, 
  DeriveFoldable #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.ReportMonad
-- Copyright   : 3gERP, 2009
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the monad in which reports are executed.
--
--------------------------------------------------------------------------------

module Poets.Reporting.ReportMonad
    ( RM,
      liftRM,
      ReportError(..),
      throwError,
      reportTypeError,
      reportTypeErrorThunk,
      liftTypeError,
      reportRuntimeError,
      reportRuntimeErrorThunk,
      reportUserError,
      iThunk,
      eval,
      eval2,
      dethunk,
      deepDethunk,
      deepEval,
      deepEval2,
      Thunk (..),
      RValue,
      RExpr,
      RExpr',
      runRM,
      --evalRExpr,
      --evalRValue,
      rEvents,
      rTypeOf,
      rTypeOf',
      rRefTypeOf,
      rRefTypeOf',
      rDerefNow,
      rDerefCxt,
      rHasType,
      rString,
      rError,
      rRecord,
      rList,
      rCons,
      rEmpty,
      rFieldLookup,
      rFieldMod,
      rFieldsMod,
      rFoldr,
      rIf,
      rBool,
      rFalse,
      rTrue,
      rEq,
      rNeq,
      rLt,
      rGt,
      rLte,
      rGte,
      rMax,
      rMin,
      rAnd,
      rOr,
      rNot,
      rPlus,
      rMinus,
      rTimes,
      rAbs,
      rSignum,
      rNegate,
      rInteger,
      rRational,
      rDiv,
      rRecip,
      rDuration,
      rDurPlus,
      rDurMinus,
      rCompDateTime,
      rDateTime_,
      rDateTime,
      rTime,
      rDate,
      rDateTime',
      rTime',
      rDate',
    ) where

import Poets.Data hiding (fromSeconds)
import Poets.Data.Render


import Control.Monad.Error hiding (mapM,sequence)
import Control.Monad.Reader hiding (mapM,sequence)
import Control.Monad.Identity hiding (mapM,sequence)
import Prelude  hiding (mapM,sequence)

import Data.Traversable
import Data.Comp.Derive
import Data.Comp.Ops

import Text.Printf

{-|
  This data type represents errors during the computation in
  a reporting function.
-}
data ReportError = ReportTypeError String
                 | ReportUserError String
                 | ReportRuntimeError String
                 | ReportError String
                 deriving (Eq)

instance Error ReportError where
    strMsg = ReportError

instance Show ReportError where
    show (ReportError msg) = "Unexpected error: " ++ msg
    show (ReportRuntimeError msg) = "Runtime error: " ++ msg
    show (ReportUserError msg) = "User error: " ++ msg
    show (ReportTypeError msg) = "Type mismatch: " ++ msg


type SubTypePred = RecordName -> RecordName -> Bool
type EntityTable f = EntId -> Maybe DateTime -> Either String (VRecord (Term f))
type Events f = Term f
data RMEnv f = RMEnv {
      rmEvents :: Events f,
      rmIsSubtype :: SubTypePred,
      rmEntityStore :: EntityTable f
    }

{-|
  The reporting monad.
-}
newtype RM f a = RM {unRM :: ReaderT (RMEnv f) (ErrorT ReportError Identity) a}
    deriving (Functor, Monad, MonadError ReportError, MonadReader (RMEnv f))

liftRM :: (Term g -> Term f) -> RM f a -> RM g a
liftRM trans (RM m) = RM (withReaderT trans' m)
    where trans'  (RMEnv evs sub estore) = (RMEnv (trans evs) sub (mtrans estore))
          mtrans estore x y = fmap (fmap trans) (estore x y)

reportRuntimeError :: (MonadError ReportError m) => String -> m a
reportRuntimeError msg = throwError (ReportRuntimeError msg)

reportUserError :: (MonadError ReportError m) => String -> m a
reportUserError msg = throwError (ReportUserError msg)

reportTypeError :: (MonadError ReportError m) => String -> m a
reportTypeError msg = throwError (ReportTypeError msg)



liftRuntimeError :: (MonadError ReportError m) => Either String a -> m a
liftRuntimeError (Left msg) = reportRuntimeError msg
liftRuntimeError (Right v) = return v

liftTypeError :: (MonadError ReportError m) => Either String a -> m a
liftTypeError (Left msg) = reportTypeError msg
liftTypeError (Right v) = return v

runRM' :: RM f a -> Events f -> EntityTable f -> SubTypePred -> Either ReportError a
runRM' rm events estore subType =  runIdentity . runErrorT. flip runReaderT rmEnv . unRM $ rm
    where rmEnv = RMEnv events subType estore

runRM :: (Val :<: f) => RM f a -> [Term f] -> EntityTable f -> SubTypePred -> Either ReportError a
runRM rm events subType =  runRM' rm events' subType
    where events' = inject . VList $  events

getLatestEntity :: EntId -> Maybe DateTime -> RM f (VRecord (Term f))
getLatestEntity rid date = do
  estore <- liftM rmEntityStore ask
  liftRuntimeError $ estore rid date



data Thunk m e = Thunk (m e) deriving (Functor, Foldable)

$( derive 
   [smartConstructors]
   [''Thunk] )

type RExpr f = Term (Thunk (RM f) :+: f)
type RExpr' f = RM f (RExpr f)

reportTypeErrorThunk :: String -> RExpr f
reportTypeErrorThunk msg = iThunk (reportTypeError msg :: RExpr' f)

reportRuntimeErrorThunk :: String -> RExpr f
reportRuntimeErrorThunk msg = iThunk (reportRuntimeError msg :: RExpr' f)

dethunk :: Monad m => Term (Thunk m :+: f) -> m (f (Term (Thunk m :+: f)))
dethunk (Term (Inl (Thunk m))) = m >>= dethunk
dethunk (Term (Inr t)) = return t

eval :: Monad m => (f (Term (Thunk m :+: f)) -> Term (Thunk m :+: f))
                -> Term (Thunk m :+: f)
                -> Term (Thunk m :+: f)
eval cont (Term (Inl (Thunk m))) = iThunk $ liftM cont $ dethunk =<< m 
eval cont (Term (Inr t)) = cont t

eval2 cont x y = (\ x' -> cont x' `eval` y) `eval` x 

deepDethunk :: (Monad m, Traversable f) => Term (Thunk m :+: f) -> m (Term f)
deepDethunk = liftM Term . mapM deepDethunk <=< dethunk

deepEval :: (Traversable f, Monad m) => 
            (Term f -> Term (Thunk m :+: f))
            -> Term (Thunk m :+: f)
            -> Term (Thunk m :+: f)
deepEval cont v = case deepProject v of 
                    Just v' -> cont v'
                    _ -> iThunk $ liftM cont $ deepDethunk v 

deepEval2 cont x y = (\ x' -> cont x' `deepEval` y ) `deepEval` x


type RValue = RExpr Val

-------------------------------------
-- Operations in the report monad ---
-------------------------------------


rEvents ::  (Val :<: f, Functor f) => RExpr f
rEvents = iThunk rEvents'

rEvents' ::  (Val :<: f, Functor f) => RExpr' f
rEvents' = liftM (deepInject . rmEvents) ask

{-|
  This operator provides a case distinction for type membership
-}

rRefTypeOf :: (Val :<: f) => RExpr f -> [(String ,RExpr f)] -> RExpr f -> RExpr f
rRefTypeOf _ [] def = def
rRefTypeOf rexp tys def = iThunk $ do 
  exp <- dethunk rexp
  case proj exp of 
    Just (VEnt VEntity {ventType = rName}) -> do
                            res <- mapM run tys
                            return $ head (concat res ++ [def])
        where run (ty,rhs) = do 
                bTy <- isSubtype rName ty
                return $ if bTy then [rhs] else []
    _ -> reportTypeError "Types can only be checked for reverence values!"
    

rRefTypeOf' :: (Val :<: f) => RExpr f -> [(String ,RExpr f)] -> RExpr f
rRefTypeOf' rexp cases = rRefTypeOf rexp cases $ reportRuntimeErrorThunk "value does not match any of the given types"

{-|
  This operator provides a case distinction for type membership
-}
rTypeOf :: (Val :<: f) => RExpr f -> [(String ,RExpr f)] -> RExpr f -> RExpr f
rTypeOf _ [] def =  def
rTypeOf rexp tys def = iThunk $ do 
  exp <- dethunk rexp
  case proj exp of 
    Just (VRecord VR{ vrecordName = rName}) -> do
                            res <- mapM run tys
                            return $ head (concat res ++ [def])
        where run (ty,rhs) = do 
                bTy <- isSubtype rName ty
                return $ if bTy then [rhs] else []
    _ -> reportTypeError "Types can only be checked for record values!"

rTypeOf' :: (Val :<: f) => RExpr f -> [(String ,RExpr f)] -> RExpr f
rTypeOf' rexp cases = rTypeOf rexp cases $ 
                      reportRuntimeErrorThunk "value does not match any of the given types"

{-|
  This operator checks for type membership.
-}
rHasType :: (Val :<: f) => RExpr f -> String -> RExpr f
rHasType rexp ty = iThunk $ liftM (inject . VBool) $ hasType' rexp ty


isSubtype :: RecordName -> RecordName -> RM f Bool
isSubtype r1 r2 = do
  isSub <- liftM rmIsSubtype ask
  return $ isSub r1 r2


hasType' :: (Val :<: f) => RExpr f -> RecordName -> RM f Bool
hasType' rexp ty = do 
  exp <- dethunk rexp
  case proj exp of 
    Just (VRecord VR {vrecordName = rName}) -> isSubtype rName ty
    _ -> reportTypeError "Types can only be checked for record values!"


rString :: (Val :<: f) => String -> RExpr f
rString = iVString

rError :: (Val :<: f) => RExpr f -> RExpr f
rError strR = iThunk $ do
  strE <- dethunk strR
  case proj strE of 
    Just (VString str) -> reportUserError str
    _ -> reportTypeError "The argument of error must be a string!"

{-|
  This function constructs record values.
-}

rRecord :: (Val :<: f) => String -> [(String, RExpr f)] -> RExpr f
rRecord name fields = iVRecord VR {vrecordName = name, vrecordFields = newFields fields'}
  where cField (name, val) = VF { vfieldName = name, vfieldValue = val }
        fields' = map cField fields

{-|
  This function constructs list values.
-}
rList :: (Val :<: f) => [RExpr f] -> RExpr f
rList = iVList


rCons :: (Val :<: f) => RExpr f-> RExpr f -> RExpr f
rCons eR = eval $ \ lE -> 
  case proj lE of
    Just (VList l) -> iVList (eR : l)
    _ -> reportTypeErrorThunk "The second argument of |: must be a list!"


{-|
  This is the empty list value.
-}
rEmpty :: (Val :<: f) => RExpr f
rEmpty = iVList []

{-|
  This operator looks up a field in a record value.
-}
rFieldLookup :: (Val :<: f, Render f, Traversable f) => RExpr f -> String -> RExpr f
rFieldLookup rec name = iThunk $ do
  vrec <- dethunk rec
  case proj vrec of
    Just (VRecord rec') -> 
        liftTypeError $ lookupField name rec'
    _ -> do v <- deepDethunk rec
            reportTypeError $ "record fields can only be accessed on record values!\n\
                           \field: "++ name ++ "\n\ 
                           \value: "++ show v

{-|
  This operator sets a field value in a record value.
-}
rFieldMod :: (Val :<: f) => RExpr f -> String -> RExpr f -> RExpr f
rFieldMod rec name newVal = iThunk $ do
  vrec <- dethunk rec
  case proj vrec of
    Just (VRecord rec') -> 
        liftTypeError $ liftM iVRecord $ updateField name newVal rec'
    _ -> reportTypeError "The set-field operator <~ is only allowed on record values!"


{-|
  This operator sets a field value in a record value.
-}
rFieldsMod :: (Val :<: f) => RExpr f -> [(String, RExpr f)] -> RExpr f
rFieldsMod rec mods = foldl appMod rec mods
    where appMod rec (field,new) = rFieldMod rec field new

{-|
  This is the right fold function on list values.
-}
rFoldr :: (Val :<: f) => (RExpr f -> RExpr f -> RExpr f) ->  RExpr f ->  RExpr f -> RExpr f
rFoldr f ve = eval $ \ vList ->
  case proj vList of 
    Just (VList l) -> foldr f ve l
    _ -> reportTypeErrorThunk "foldr expects a list as first argument!: "

rIf :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f -> RExpr f
rIf condE thenE elseE = iThunk $ do
  condV <- dethunk condE
  case proj condV of
    Just (VBool cond) -> return $ if cond then thenE else elseE
    _ -> reportTypeError "the condition argument of ifThenElse has to be Boolean"

rBool :: (Val :<: f) => Bool -> RExpr f
rBool = iVBool


{-|
  Boolean false value.
-}
rFalse :: (Val :<: f) => RExpr f
rFalse =  rBool False

{-|
  Boolean true value.
-}
rTrue :: (Val :<: f) => RExpr f
rTrue = rBool True



rEq, rNeq :: (Traversable f, EqF f, Val :<: f) => RExpr f -> RExpr f -> RExpr f
rEq = deepEval2 $ \ x y -> iVBool (x == y)
rNeq = deepEval2 $ \ x y -> iVBool (x /= y)

rLt, rGt, rLte, rGte :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
rLt = rCompare "<" (iVBool . (== LT))
rLte = rCompare "<=" (iVBool . (/= GT))
rGt = rCompare ">" (iVBool . (== GT))
rGte = rCompare ">=" (iVBool . (/= LT))
rMax, rMin :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
rMax x y = rCompare "max" (\ord -> if  ord == LT then y else x) x y
rMin x y = rCompare "min" (\ord -> if  ord == GT then y else x) x y



{-|
  This is an auxiliary function for defining the comparison operators.
-}
rCompare :: (Val :<: f) => String -> (Ordering -> RExpr f) -> RExpr f -> RExpr f -> RExpr f
rCompare opname cont = eval2 vCompare
    where vCompare x y =
              case (proj x,proj y) of
                (Just (VInt i), Just (VInt j)) -> compare' i j
                (Just (VInt i), Just (VReal j)) -> compare' (fromIntegral i) j
                (Just (VReal i), Just (VInt j)) -> compare' i (fromIntegral j)
                (Just (VReal i), Just (VReal j)) -> compare' i j
                (Just (VDateTime d), Just (VDateTime e)) -> compare' d e
                (Just (VDuration d), Just (VDuration e)) -> compare' d e
                (Just (VList xs), Just (VList ys)) -> lCompare xs ys
                (Just (VString xs), Just (VString ys)) -> compare' xs ys 
                _ -> reportTypeErrorThunk $ "The given values cannot be compared using " ++ opname
          compare' x y  = cont $ compare x y 
          lCompare [] [] = cont EQ
          lCompare [] _  = cont LT
          lCompare _ []  = cont GT
          lCompare (x:xs) (y:ys) = rCompare opname
                                   (\ ord -> case ord of EQ -> lCompare xs ys
                                                         _ -> cont ord) 
                                   x y


boolOp :: (Val :<: f) => String -> (Bool -> Bool -> Bool) -> RExpr f -> RExpr f -> RExpr f
boolOp opName op = eval2 $ \ x y -> 
  case proj x of 
    Just (VBool xB) -> 
        case proj y of
          Just (VBool yB) -> iVBool (op xB yB)
          _ -> reportTypeErrorThunk $ "The second argument of " ++ opName ++ " has to be Boolean."
    _ -> reportTypeErrorThunk $ "The first argument of " ++ opName ++ " has to be Boolean."

rAnd, rOr :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
x `rAnd` y = boolOp "&&" (&&) x y
x `rOr` y = boolOp "||" (||) x y

rNot :: (Val :<: f) => RExpr f -> RExpr f
rNot = eval$ \x ->
  case proj x of
    Just (VBool xB) -> iVBool $ not xB
    _ -> reportTypeErrorThunk "The argument of not has to be Boolean."

{-|
  Auxiliary function for defining the binary operators of 'Num'.
-}
arithOp :: (Val :<: f) =>
           (Int -> Int -> Int)
        -> (Double -> Double -> Double) 
        -> String
        -> RExpr f -> RExpr f
        -> RExpr f
arithOp opInt opDbl opName = eval2 $ \ x y -> 
      case proj x of
        Just (VInt i) ->
            case proj y of
              Just (VInt j) -> iVInt (i `opInt` j)
              Just (VReal j) -> iVReal (fromIntegral i `opDbl` j)
              _ -> err2nd
        Just (VReal i) ->
            case proj y of
              Just (VReal j) -> iVReal (i `opDbl` j)
              Just (VInt j) -> iVReal (i `opDbl` fromIntegral j)
              _ -> err2nd
        _ -> err1st
        where err1st = reportTypeErrorThunk $ "first argument of " ++ opName ++" has to be an integer or a double"
              err2nd = reportTypeErrorThunk $ "second argument of " ++ opName ++ " has to be an integer or a double"

rPlus, rMinus, rTimes :: (Val :<: f)
                      => RExpr f -> RExpr f -> RExpr f
rPlus = arithOp (+) (+) "+"
rMinus = arithOp (-) (-) "-"
rTimes = arithOp (*) (*) "*"

{-|
  Auxiliary function for defining the unary operators of 'Num'.
-}
arithUn :: (Val :<: f) =>
           (Int -> Int)
        -> (Double -> Double) 
        -> String
        -> RExpr f
        -> RExpr f
arithUn opInt opDbl opName = eval $ \ x ->
      case proj x of
        Just (VInt i) ->  iVInt (opInt i)
        Just (VReal i) -> iVReal (opDbl i)
        _ -> reportTypeErrorThunk $ "argument of "++ opName ++" has to be an integer or a double"


rAbs, rSignum, rNegate :: (Val :<: f)
                      => RExpr f -> RExpr f
rAbs = arithUn abs abs "abs"
rSignum = arithUn signum signum "signum"
rNegate = arithUn negate negate "negate"

rInteger :: (Val :<: f) => Integer -> RExpr f
rInteger = iVInt . fromInteger


rRational :: (Val :<: f) => Rational -> RExpr f
rRational = iVReal . fromRational

rRecip  :: (Val :<: f) => RExpr f -> RExpr f
rRecip = eval $ \ x ->
  case proj x of
    Just (VInt i) ->  iVReal (recip $ fromIntegral i)
    Just (VReal i) -> iVReal (recip i)
    _ -> reportTypeErrorThunk "argument of recip has to be an integer or a double"

rDiv  :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
rDiv = eval2 $ \ x y ->
  case proj x of
    Just (VInt i) ->
        case proj y of
          Just (VInt j) -> iVReal (fromIntegral i / fromIntegral j)
          Just (VReal j) -> iVReal (fromIntegral i / j)
          _ -> err2nd
    Just (VReal i) ->
        case proj y of
          Just (VReal j) -> iVReal (i / j)
          Just (VInt j) -> iVReal (i / fromIntegral j)
          _ -> err2nd
    _ -> err1st
    where err1st = reportTypeErrorThunk "first argument of / has to be an integer or a double"
          err2nd = reportTypeErrorThunk "second argument of / has to be an integer or a double"


rDuration :: (Val :<: f) => VDuration (RExpr f) -> RExpr f
rDuration = iThunk . liftM iVDuration . mapM coerce
    where coerce vm = do
            v <- dethunk vm
            case proj v of
              Just (VInt i) -> return i
              _ -> reportTypeError "durations can only be constructed from integers"

rDateTime_ :: (Val :<: f) => DateTime -> RExpr f
rDateTime_ = iVDateTime

rDateTime' :: (Val :<: f) => Integer -> Int -> Int -> Int -> Int -> Int -> RExpr f
rDateTime' y m d h min s = case createDateTime' y m d h min s 0 of
  Just dt -> rDateTime_ dt
  Nothing -> reportRuntimeErrorThunk $
             printf "\"%d-%02d-%02d %02d:%02d:%02d\" is not a well-formed date" y m d h min s

rDateTime :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f -> RExpr f -> RExpr f -> RExpr f -> RExpr f
rDateTime y' m' d' h' min' s' = iThunk $ do
  [y,m,d,h,min,s] <- mapM pr [y', m', d', h', min', s']
  return $ rDateTime' (toInteger y) m d h min s
      where pr rexp = do
            exp <- dethunk rexp
            case proj exp of
              Just (VInt i) -> return i
              _ -> reportTypeError "the components of a date expression have to be integers"

rDate_ :: (Val :<: f) => Date -> RExpr f
rDate_ = iVDate

rDate'  :: (Val :<: f) => Integer -> Int -> Int -> RExpr f
rDate' y m d =  case createDate y m d of
  Just d -> rDate_ d
  Nothing -> reportRuntimeErrorThunk $
             printf "\"%d-%02d-%02d\" is not a well-formed date" y m d
             
rCompDateTime_ :: (Val :<: f) => Date -> Time -> RExpr f
rCompDateTime_ date time = iVDateTime $ createDateTime date time

rCompDateTime :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
rCompDateTime date time = iThunk $ do
  date' <- dethunk date
  time' <- dethunk time
  case proj date' of 
    Just (VDate d) -> case proj time' of
      Just (VTime t) -> return $ rCompDateTime_ d t
      _ -> reportTypeError "the second argument of a date-time composition has to be a time"
    _ -> reportTypeError "the first argument of a date-time composition has to be a date"

rDate :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f -> RExpr f
rDate y' m' d' = iThunk $ do
  [y,m,d] <- mapM pr [y', m', d']
  return $ rDate' (toInteger y) m d
      where pr rexp = do
            exp <- dethunk rexp
            case proj exp of
              Just (VInt i) -> return i
              _ -> reportTypeError "the components of a date expression have to be integers"

rTime_ :: (Val :<: f) => Time -> RExpr f
rTime_ = iVTime

rTime'  :: (Val :<: f) => Int -> Int -> Int -> RExpr f
rTime' h m s = case createTime h m s 0 of
  Just t -> rTime_ t
  Nothing -> reportRuntimeErrorThunk $
             printf "\"%02d:%02d:%02d\" is not a well-formed time" h m s
             
rTime :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f -> RExpr f
rTime h' m' s' = iThunk $ do
  [h,m,s] <- mapM pr [h', m', s']
  return $ rTime' h m s
      where pr rexp = do
            exp <- dethunk rexp
            case proj exp of
              Just (VInt i) -> return i
              _ -> reportTypeError "the components of a time expression have to be integers"


rDurPlus :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
rDurPlus = rDurOp "added to" (flip addDurationToDateTime) addDuration

rDurMinus :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
rDurMinus = rDurOp "subtracted from" subtractDurationFromDateTime subtractDuration


rDurOp :: (Val :<: f) => String -> (DateTime -> VDuration Int -> DateTime)
       -> (VDuration Int -> VDuration Int -> VDuration Int)
       -> RExpr f -> RExpr f -> RExpr f
rDurOp msg opDate opDur = eval2 $ \ l r ->
  case proj r of
    Just (VDuration r') -> 
      case proj l of
        Just (VDateTime l') -> iVDateTime $ opDate l' r'
        Just (VDuration l') -> iVDuration $ opDur l' r'
        _ -> reportTypeErrorThunk $ "a duration value can only be " ++ msg ++ " a date or duration value"
    _ -> reportTypeErrorThunk $ "only a duration value can be " ++ msg ++ " a date or duration value"

rDerefNow :: (Val :<: f, Functor f) => RExpr f -> RExpr f
rDerefNow rm = iThunk $ do
  r <- dethunk rm
  case proj r of
    Just (VEnt VEntity{ventId = rid}) ->
        liftM (iVRecord . fmap deepInject) $ getLatestEntity rid Nothing
    _ -> reportTypeError "Only reference values can be dereferenced!"

rDerefCxt :: (Val :<: f, Functor f) => RExpr f -> RExpr f
rDerefCxt rm = iThunk $ do
  r <- dethunk rm
  case proj r of
    Just (VEnt VEntity{ventId = rid, ventContext = cxt}) ->
        liftM (iVRecord . fmap deepInject) $ getLatestEntity rid cxt
    _ -> reportTypeError "Only reference values can be dereferenced!"