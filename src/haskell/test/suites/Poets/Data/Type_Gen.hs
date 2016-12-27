{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE
  TypeOperators,
  MultiParamTypeClasses,
  FlexibleInstances,
  FlexibleContexts,
  UndecidableInstances,
  TypeSynonymInstances #-}

module Poets.Data.Type_Gen
    (ArbitraryF(..),
     TypedArbitraryF (..),
     TypedArbitrary (..),
     GenType (..),
     forAllTypes,
     force) where

import Poets.Data.Type
import Poets.Data.Type.Utils
import Poets.Data.Type.Render
import Data.Comp
import Data.Comp.Ops
import Data.Comp.Arbitrary
import Poets.Data.Value
import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)


{-|
  This class is a variant of the class 'ArbitraryF' which is supposed
  to generate well-typed expressions according to the typing environment and the
  given type.

  Only one of the two methods 'arbitraryTF'' and 'arbitraryTF' needs to be implemented.
  The method 'arbitraryTF'' allows to give a list of weighted generators. This is intended for
  combining functors using the sum construction. Functors having more than one constructor
  are supposed to return a singleton list with an weight according to the number of constructors.
-}
class TypedArbitraryF f t where
    arbitraryTF' :: TypedArbitrary v t => RecordEnv (Term t) -> Term t -> [(Int,Gen (f v))]
    arbitraryTF' env ty = [(1,arbitraryTF env ty)]
    arbitraryTF :: TypedArbitrary v t => RecordEnv (Term t) -> Term t -> Gen (f v)
    arbitraryTF env ty = frequency (arbitraryTF' env ty)

{-|
  Instances of 'TypedArbitraryF' are closed under sums in the first component.
-}
instance (TypedArbitraryF f t, TypedArbitraryF g t) => TypedArbitraryF (f :+: g) t where
    arbitraryTF' env ty =
        map inl (arbitraryTF' env ty) ++ map inr (arbitraryTF' env ty)
            where inl (i,gen) = (i, Inl <$> gen)
                  inr (i,gen) = (i, Inr <$> gen)

{-|
  This class is a variant of the class 'TypedArbitraryF' which has a type of kind @*@
  rather than @* -> *@ as its first argument.
  
  Only one of the two methods 'arbitraryT'' and 'arbitraryT' needs to be implemented.
-}
class TypedArbitrary v t where
    arbitraryT' :: RecordEnv (Term t) -> (Term t) -> [(Int,Gen v)]
    arbitraryT' env ty = [(1,arbitraryT env ty)]
    arbitraryT :: RecordEnv (Term t) -> (Term t) -> Gen v 
    arbitraryT env ty = frequency (arbitraryT' env ty)

{-|
  Instances of 'TypedArbtitraryFunc' can be lifted to instances of
  'TypedArbitrary' w.r.t. the corresponding expression type.
-}

instance TypedArbitraryF f t => TypedArbitrary (Term f) t where
    arbitraryT' env ty = map expr $ arbitraryTF' env ty
        where expr (i,gen) = (i, Term <$> gen)


{-|
  This class should be instantiated by pairs of type functors t and t' 
  such that a type in Term t can be generated given a type environment of
  t'.
-}
class GenType t t' where
    genType' :: GenType t' t' => RecordEnv (Term t') -> [(Int,Gen (t (Term t')))]
    genInject :: t (Term t') -> Term t'

{-|
  If a type over functor @t@ can be generated from typing environments
  over the same functor @t@ then typing environments over this functor
  @t@ can be generated
-}
instance GenType t t => Arbitrary (RecordEnv (Term t)) where
    arbitrary
       = do size <- sized (\size -> choose (0,size)) 
            gen size (newRecordEnv [])
        where gen 0 env = return env
              gen i env = do record <- genNewRecord env
                             gen (i-1) (addRecordInfo env record)

{-|
  Instances of 'GenType' are closed under sums in the first component.
-}
instance (GenType t1 t, GenType t2 t) => GenType (t1 :+: t2) t where
    genType' env = map inl (genType' env) ++ map inr (genType' env)
        where inl (i,g) = (i,Inl <$> g)
              inr (i,g) = (i,Inr <$> g)
    genInject (Inl v) = genInject v
    genInject (Inr v) = genInject v

{-|
  This function generates types in the given type environment.
-}
genType :: GenType t t => RecordEnv (Term t) -> Gen (Term t)
genType = liftM Term . frequency . genType'

force :: Either String a -> a
force (Left msg) = error msg
force (Right x) = x


{-|
  Auxiliary function for generating record types.
-}
genNewRecord :: GenType t t => RecordEnv (Term t) -> Gen (Record (Term t))
genNewRecord env = 
    do name <- arbitraryNew (getRecordNames env)
       size <- sized (\size -> choose (0,size))
       extends <- if isEmpty env
                    then return Set.empty
                    else oneof [return Set.empty, Set.singleton <$> (elements $ getRecordNames' env)]
       let usedFieldNames = case Set.elems extends of
                              [] -> Set.empty
                              super:_ -> force $ getFieldNames env super
           supers = case Set.elems extends of
                      [] -> []
                      super:_ -> super : (force $ getSuperTypes' env super)
           env' = remRecordInfos env supers
       fields <- genFields env' size usedFieldNames
--       desc <- arbitrary
--       abstract <- arbitrary
       return Record
                  { recordName = name,
                    recordFields = newFieldEnv fields,
                    recordExtends = extends,
                    recordAttributes = Set.empty}
    where genFields _ 0 _ = return []
          genFields env' i old = do field <- genNewField env' old 
                                    fields <- genFields env' (i-1) (Set.insert (fieldName field) old)
                                    return $ field : fields

{-|
  Auxiliary function for generating field types.
-}
genNewField :: GenType t t => RecordEnv (Term t) -> Set FieldName -> Gen (Field (Term t))
genNewField env names = do name <- arbitraryNew names
--                           desc <- arbitrary
                           ty <- genType env
                           return Field {fieldName = name, fieldType = ty, fieldAttributes = Set.empty}
               
{-|
  This is an auxiliary function for generating a new value. It generates a value which is
  not in the given set of values. If there is no such value, then this function does not
  terminate.
-}
arbitraryNew :: (Arbitrary a, Ord a) => Set a -> Gen a
arbitraryNew old = suchThat arbitrary (not . (flip Set.member old))


{-|
  This function provides a means to explicitly quantify over types which are well-formed
  according to the given typing environment.
-}
forAllTypes :: (Testable prop, GenType t t, Functor t, Render t) => RecordEnv (Term t) -> (Term t -> prop) -> Property
forAllTypes env prop = do ty <- genType env
                          whenFail (putStrLn ("type: "++ show ty)) $ property $ prop ty

{-|
  This instance allows to generate arbitrary type constants.
-}
instance ArbitraryF TypeConstant where
    arbitraryF' = [(9,oneof [tint,tbool,tstring,tduration,tdate,ttime,tdatetime,tdouble,trecord])]
        where tint = return TInt
              tbool = return TBool
              tstring = return TString
              tduration = return TDuration
              tdate = return TDate
              ttime = return TTime
              tdatetime = return TDateTime
              tdouble = return TReal
              trecord = TRecord <$> arbitrary

{-|
  This instance allows to generate arbitrary list types.
-}
instance ArbitraryF TypeList where
    arbitraryF' = [(1, oneof [TList <$> arbitrary])]

{-|
  This instance allows to generate arbitrary record types.
-}
instance ArbitraryF TypeEnt where
    arbitraryF' = [(1, oneof [TEnt <$> arbitrary])]
