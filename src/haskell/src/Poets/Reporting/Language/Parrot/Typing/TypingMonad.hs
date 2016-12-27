{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.TypingMonad
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the base monad for type inference and related
-- functionality (i.e. unification, matching etc.). It provides a
-- means to generate error messages which provide a indication of the
-- source code position from where the error originated.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.TypingMonad
    ( TypingM
    , TypingErr(..)
    , MonadTyping(..)
    , addErrMsg
    , runTypingM
    , typeErr
    , liftTypeErr
    , liftUnifErr
    , getPos
    , cHasField 
    , cSubType
    , cOrd
    , cEq
    , tRecord
    , tReal
    , tString'
    , tChar
    , tInt
    , tBool
    , tDate
    , tDuration
    , tDurationDate
    , tFun
--    , tProd
    , tSum
    , tList
    , tNil
    , withPos'
    ) where

import Poets.Data.Render
import Poets.Reporting.Language.Parrot.Syntax

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Data.Comp.Unification

import Data.Equivalence.Monad
import Control.Monad.ST.Trans.Internal

{-| This data type represents errors that occur during type
inference/checking.  -}

data TypingErr = TypingErr { teMsg :: String
                           , tePos :: SrcPos }

instance Show TypingErr where
    show (TypingErr msg pos) = "Typing error" ++ pos' ++ ":\n"
                               ++ msg
        where pos' = case pos of
                       Nothing -> ""
                       Just pos -> " at " ++ show pos

instance Error TypingErr where
    strMsg msg = TypingErr {teMsg = msg, tePos = Nothing}


{-| The @TypingM@ monad provides functionality to report errors in
context of a source position.  -}

newtype TypingM a = TypingM { unTypingM :: ReaderT SrcPos (Either TypingErr) a}
    deriving (Functor, Monad)

{-| This function runs the given @TypingM@ monad returning either an
error value or the actual return value.  -}

runTypingM :: TypingM a -> Either TypingErr a
runTypingM (TypingM m) = runReaderT m Nothing

{-| This class captures the capabilities of the typing monad. -}

class (Monad m) => MonadTyping m where
    {-| This function allows to run a typing monad within the context
      of the specified position. That is, error messages within the
      resulting monad are have the specified position as their context. -}

    withPos :: SourcePos -> m a -> m a

    {-| This function, similar to 'catchError' allows to catch and
      handle an erroneous typing computation.  -}

    catchTyErr :: m a -> (TypingErr -> m a) -> m a
      
    {-|
      This function lifts any 'TypingM' monad to this monad.
     -}
    liftTy :: TypingM a -> m a


{-| This function adds the given message to all error messages
generated within the given monadic computation.  -}

addErrMsg :: (MonadTyping m) => String -> m a -> m a
addErrMsg msg m = catchTyErr m handle
        where handle TypingErr{teMsg = msg', tePos = pos} =
                  let pos' = case pos of
                               Nothing -> ""
                               Just pos -> "at " ++ show pos ++ " "
                  in  typeErr $ msg ++ "\n" ++ pos' ++ msg'

{-| This function generates an error with the given message. The
generated error mentions a context position according to the innermost
'withPos' (or possibly 'withPos'') scope this function is used in.  -}

typeErr :: (MonadTyping m) => String -> m a
typeErr msg = liftTy $ TypingM $
                      do pos <- ask
                         throwError TypingErr{tePos = pos, teMsg = msg}

liftTypeErr :: (MonadTyping m) => Either String a -> m a
liftTypeErr (Left msg) = typeErr msg
liftTypeErr (Right v) = return v

liftUnifErr :: (MonadTyping m, Show v) => Either (UnifError TypeSigPos v) a -> m a
liftUnifErr (Right res) = return res
liftUnifErr (Left err) = 
    case err of 
      FailedOccursCheck v t -> typeErr $ "cannot construct infinite type '"
                               ++ show v ++ " = " ++ show (renderTerm t) ++ "'"
      HeadSymbolMismatch t1 t2 -> typeErr $ "type mismatch"
                                  ++ "\ninferred type: " ++ show (renderTerm t1)
                                  ++ "\nexpected type: " ++ show (renderTerm t2)
      UnifError msg -> typeErr $ "internal error while performing unification:\n"
                       ++ msg

{-| This function returns the current source position context.  -}

getPos :: (MonadTyping m) => m SrcPos
getPos = liftTy $ TypingM ask

{-| This function is similar to 'withPos' but takes a position wrapped
in a 'Maybe'. When given a 'Just' value, it behaves like
'withPos'. Otherwise, this function does not alter the given monadic
computation.  -}

withPos' :: (MonadTyping m) => SrcPos -> m a -> m a
withPos' Nothing = id
withPos' (Just pos) = withPos pos

cHasField :: (MonadTyping m) => m (PType -> FieldName -> PType -> TypeConstrPos PType)
cHasField = getPos >>= return . (\ p x y z-> HasField x y z :&: p)

cSubType :: (MonadTyping m) => m (PType -> PType -> TypeConstrPos PType)
cSubType = getPos >>= return . (\p x y -> SubType x y :&: p)

cOrd :: (MonadTyping m) => m (PType -> TypeConstrPos PType)
cOrd = getPos >>= return . \ p x -> Ord x :&: p

cEq :: (MonadTyping m) => m (PType -> TypeConstrPos PType)
cEq = getPos >>= return . \ p x -> Eq x :&: p

tRecord :: (MonadTyping m) => m (RecordName -> PType)
tRecord = getPos >>= (\p -> return (\ n -> inject $ TRecord n :&: p))

tNil :: (MonadTyping m, (f :&: SrcPos) :<: TypeSigPos)
     => f PType -> m PType
tNil nil = liftM (\p -> inject $ nil:&: p) getPos

tReal :: (MonadTyping m) => m PType
tReal = tNil TReal

tString' :: (MonadTyping m) => m PType
tString' = do 
  list <- tList
  liftM list $ tNil TChar

tChar :: (MonadTyping m) => m PType
tChar = tNil TChar

tInt :: (MonadTyping m) => m PType
tInt = tNil TInt

tBool :: (MonadTyping m) => m PType
tBool = tNil TBool

tDate :: (MonadTyping m) => m PType
tDate = tNil TDateTime

tDuration :: (MonadTyping m) => m PType
tDuration = tNil TDuration

tDurationDate :: (MonadTyping m) => m PType
tDurationDate = tNil TDurationDate

tBin :: (MonadTyping m, (f :&: SrcPos) :<: TypeSigPos)
     => (PType -> PType -> f PType) -> m (PType -> PType -> PType)
tBin bin = getPos >>= (\p ->  return (\ s t -> inject $ bin s t :&: p))

tFun :: (MonadTyping m) => m (PType -> PType -> PType)
tFun = tBin TFun

-- tProd :: (MonadTyping m) => m ([PType] -> PType)
-- tProd = tBin TProd

tSum :: (MonadTyping m) => m (PType -> PType -> PType)
tSum = tBin TSum


tUn :: (MonadTyping m, (f :&: SrcPos) :<: TypeSigPos)
     => (PType -> f PType) -> m (PType -> PType)
tUn un = getPos >>= (\p ->  return (\ t -> inject $ un t :&: p))

tList :: (MonadTyping m) => m (PType -> PType)
tList = tUn TList



instance MonadTyping TypingM where
    withPos pos (TypingM m) = TypingM $ local (const $ Just pos) m
    catchTyErr (TypingM m) f = TypingM $ catchError m f'
        where f' err = unTypingM $ f err
    liftTy = id



instance (MonadTyping m, Monoid l) => MonadTyping (WriterT l m) where
    withPos pos (WriterT m)   =  WriterT (withPos pos m)
    catchTyErr (WriterT m) f = WriterT (catchTyErr m (runWriterT . f))
    liftTy = lift . liftTy


instance (MonadTyping m) => MonadTyping (ReaderT r m) where
    withPos pos (ReaderT f)   = ReaderT f'
        where f' e = withPos pos (f e)
    catchTyErr (ReaderT g) f = ReaderT g'
        where g' e = catchTyErr (g e) ((`runReaderT` e) . f)
    liftTy = lift . liftTy

instance (MonadTyping m) => MonadTyping (StateT s m) where
    withPos pos (StateT f)   = StateT f'
        where f' s = withPos pos (f s)
    catchTyErr (StateT g) f = StateT g'
        where g' e = catchTyErr (g e) ((`runStateT` e) . f)
    liftTy = lift . liftTy

instance (MonadTyping m) => MonadTyping (EquivT s c v m) where
    withPos pos (EquivT (ReaderT f))   = EquivT (ReaderT f')
        where f' r = let STT g = f r
                         g' s = withPos pos (g s)
                     in STT g'
    catchTyErr (EquivT (ReaderT f)) h = EquivT (ReaderT f')
        where f' r = let STT g = f r
                         g' s = catchTyErr (g s) (h' r s)
                     in STT g'
              h' r s e = let EquivT (ReaderT f1) = h e
                             STT f2 = f1 r
                         in f2 s
    liftTy = lift . liftTy
