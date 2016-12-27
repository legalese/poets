{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.EReport
-- Copyright   : 3gERP, 2009
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- Embedded reporting language
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.EReport
    (module Poets.Reporting.Report,
     (|>),
     list,
     (|:),
     null,
     tail,
     head,
     empty,
     (~>),
     (<~),
     map,
     filter,
     exists,
     elem,
     foldr,
     events,
     dateTime,
     true,
     false,
     ifThenElse,
     (+),
     (-),
     (*),
     abs,
     negate,
     (/),
     recip,
     fromRational,
     (++),
     (|->),
     (<),
     (>),
     (<=),
     (>=),
     max,
     min,
     (==),
     (/=),
     not,
     (&&),
     (||),
     const,
     id,
     (.),
     flip,
     ($),
     error,
     (?:),
     (>:),
     tInt,
     tBool,
     tTime,
     tReal,
     tRecord,
     tList
     ) where
import qualified  Prelude as P 
import Prelude hiding (map,foldr,(++),(<),(>),(<=),(>=),compare,max,min,(==),(/=),not,(&&),(||),null,tail,head,error,filter,elem)
import Poets.Data.Value
import Poets.Data.Type
import Data.Comp
import Data.Traversable (Traversable)
import Poets.Data.Render
import Poets.Reporting.Report
import GHC.Exts( IsString(..) )


tInt, tBool, tReal, tTime :: Type
tInt = inject TInt
tBool = inject TBool
tTime = inject TDateTime
tReal = inject TReal
tRecord :: String -> Type
tRecord = inject . TRecord
tList :: Type -> Type
tList = inject . TList 

{-|
  This operator provides a case distinction for type membership
-}
(>:) :: (Val :<: f) => RExpr f -> [(String ,RExpr f)] -> RExpr f
e >: alts = rTypeOf' e alts

{-|
  This operator checks for type membership.
-}
(?:) :: (Val :<: f) => RExpr f -> String -> RExpr f
(?:)  = rHasType

  

{-|
  This allows to use string literals as values.
-}
instance (Val :<: f) => IsString (RExpr f) where
    fromString  =  rString

error :: (Val :<: f) => RExpr f -> RExpr f
error = rError



{-|
  This is an alias for pairing, intended for defining mappings.
-}

(|->) :: a -> b -> (a,b)
(|->) = (,)


{-|
  This function constructs record values.
-}

(|>) :: (Val :<: f) => String -> [(String, RExpr f)] -> RExpr f
(|>) = rRecord



{-|
  This function constructs list values.
-}
list :: (Val :<: f) => [RExpr f] -> RExpr f
list = rList

(|:) :: (Val :<: f) => RExpr f-> RExpr f -> RExpr f
(|:) = rCons

{-|
  This is the empty list value.
-}
empty :: (Val :<: f) => RExpr f
empty = rEmpty

{-|
  This is the null function for list values, it returns true iff the argument
  list is empty.
-}
null :: (Val :<: f) => RExpr f -> RExpr f
null = eval $ \lE ->
  case proj lE of
    Just (VList l) -> iVBool $ P.null l
    _ -> reportTypeErrorThunk "The argument of null must be a list!"

{-|
  This is  the tail function for list values.
-}
tail :: (Val :<: f) => RExpr f -> RExpr f
tail = eval $ \lE->
  case proj lE of
    Just (VList (_:tail)) -> iVList tail
    Just (VList []) -> reportRuntimeErrorThunk "The argument of tail must not be an empty list!"
    _ -> reportTypeErrorThunk "The argument of tail must be a list!"

{-|
  This is the head function for list values.
-}
head :: (Val :<: f) => RExpr f -> RExpr f
head = eval $ \lE->
  case proj lE of
    Just (VList (hd:_)) -> hd
    Just (VList []) -> reportRuntimeErrorThunk "The argument of head must not be an empty list!"
    _ -> reportTypeErrorThunk "The argument of head must be a list!"


{-|
  This is the concatenation operator for list and string values.
-}
(++) :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
(++) = eval2 $ \ s t->
  case proj s of
    Just (VString str1) ->
        case proj t of
          Just (VString str2) -> iVString (str1 P.++ str2)
          _ -> reportTypeErrorThunk "The second argument of ++ must be a string!"
    Just (VList list1) ->
        case proj t of
          Just (VList list2) -> iVList (list1 P.++ list2)
          _ -> reportTypeErrorThunk "The second argument of ++ must be a list!"
    _ -> reportTypeErrorThunk "The first argument of ++ must be a string or a list!"


{-|
  This operator looks up a field in a record value.
-}
(~>) :: (Traversable f, Val :<: f, Render f, Functor f) => RExpr f -> String -> RExpr f
(~>) = rFieldLookup



{-|
  This operator sets a field value in a record value.
-}
(<~) :: (Val :<: f) => RExpr f -> String -> RExpr f -> RExpr f
(<~) = rFieldMod
    

{-|
  This is the map function on list values.
-}
map :: (Val :<: f) => (RExpr f -> RExpr f) -> RExpr f -> RExpr f
map f = eval $ \ vList->
  case proj vList of
    Just (VList l) -> iVList $ P.map f l
    _ -> reportTypeErrorThunk "map expects a list as first argument!"

{-|
  This is the filter function.
-}
filter :: (Val :<: f) => (RExpr f -> RExpr f) -> RExpr f -> RExpr f
filter f vl = foldr isT (list []) vl
    where isT v val = ifThenElse (f v) (v |: val) val

{-|
  The list membership function.
-}
exists f vList =
    (not $ null vList) && f (head vList) || exists f (tail vList)

{-|
  The return element by some conditional 
-}
elem f vList = 
    head $ foldr (\e el->ifThenElse (f e) (e |: el) el) empty vList
{-|
  This is the right fold function on list values.
-}
foldr :: (Val :<: f) => (RExpr f -> RExpr f -> RExpr f) ->  RExpr f ->  RExpr f -> RExpr f
foldr = rFoldr

ifThenElse :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f -> RExpr f
ifThenElse = rIf

{-|
  Boolean false value.
-}
false :: (Val :<: f) => RExpr f
false =  rFalse

{-|
  Boolean true value.
-}
true :: (Val :<: f) => RExpr f
true = rTrue

{-|
  Equality operator on values
-}
(==), (/=) :: (Traversable f, EqF f, Val :<: f) => RExpr f -> RExpr f -> RExpr f
(==) = rEq
(/=) = rNeq


(<), (>), (<=), (>=) :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
(<) = rLt
(>) = rGt
(<=) = rLte
(>=) = rGte
max, min :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
max = rMax
min = rMin


(&&), (||) :: (Val :<: f) => RExpr f -> RExpr f -> RExpr f
(&&) = rAnd
(||) = rOr

not :: (Val :<: f) => RExpr f -> RExpr f
not = rNot
              

-- dummy instances for 'Eq' and 'Show' in order to allow an instance
-- for 'Num'.
instance (EqF f) => Eq (RExpr f) where
    (==) = P.error "dummy instance of Eq (RExpr f)"

instance (Render f) => Show (RExpr f) where
    show = P.error "dummy instance of Show (RExpr f)"

{-|
  This instance declaration allows integer literals and integer operations on values.
-}
instance (Val :<: f, Show (RExpr f), Eq (RExpr f)) => Num (RExpr f) where
    (+) = rPlus
    (-) = rMinus
    (*) = rTimes
    abs = rAbs
    signum = rSignum
    negate = rNegate
    fromInteger = rInteger

events :: (Functor f, Val :<: f) => RExpr f
events = rEvents


dateTime :: (Val :<: f) => DateTime -> RExpr f
dateTime = rDateTime_

{-|
  This instance allows to use floating point literals and floating point operations on values.
-}
instance (Val :<: f, Show (RExpr f), Eq (RExpr f)) => Fractional (RExpr f) where
    fromRational = rRational
    recip = rRecip
    (/) = rDiv


infix 0 |->
infix 1 |>
infixr 5 |:
infixr 5 ++
infix 2 ~>
infix 2 <~

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
