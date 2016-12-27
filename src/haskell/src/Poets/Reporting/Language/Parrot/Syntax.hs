{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleInstances,
  TypeSynonymInstances, FlexibleContexts, MultiParamTypeClasses, 
  ScopedTypeVariables, TupleSections  #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Syntax
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the abstract syntax of the Parrot reporting
-- language.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Syntax
    (
     module Data.Comp
    , module Poets.Data.Value
    , module Poets.Data.Type
    , VFieldPos
    , LetBindPos
    , SrcPos
    , SourcePos
    , VarId
    , tVarId
    , varName
    , TVarId(..)
    , PTypeVar(..)
    , PTypeConst(..)
    , PTypeFun(..)
    , AtomicTypeSigPos
    , AtomicTypeSig
    , TypeSigPos
    , TypeSig
    , PType
    , PType'
    , ConstPType
    , ConstPType'
    , AtomicPType
    , AtomicPType'
    , TypeConstr(..)
    , TypeConstrPos
    , TypeScheme(..)
    , VVarId(..)
    , vVarId
    , ValueExt(..)
    , ValueSigPos
    , PValue
    , TypeCasePos
    , TypeCase(..)
    , DefTypeCasePos
    , DefTypeCase(..)
    , LetBind(..)
    , BinOpCore(..)
    , iTEntRec
    , RefRec(..)
    , BinOpSugar(..)
    , PrimOp(..)
    , Proj(..)
    , Deref (..)
    , ExpExt(..)
    , ExpSugar(..)
    , ListFilters(..)
    , Operand (..)
    , OperatorSection(..)
    , DayExp(..)
    , DurationExp
    , TimeExp(..)
    , ExpCoreSigPos
    , ExpSigPos
    , PExp
    , PExpCore
    , FunId
    , Decl(..)
    , DeclCore
    , DeclSugar
    , FieldDecl(..)
    , Program(..)
    , ProgramCore
    , ProgramSugar
    , Library(..)
    , LibraryCore
    , LibrarySugar
    , AtomicTypeConstr(..)
    , AtomicTypeConstrPos
    , fromAtomicPType
    , toAtomicPType
    , fromAtomicConstraint
    , fromAtomicConstraintPos
    , durationFieldMap
    , durationFieldSet
    , durationFieldList
    , dateFieldMap
    , dateFieldSet
    , dateFieldList
    , dateTimeFieldMap
    , dateTimeFieldSet
    , dateTimeFieldList
    , timeFieldMap
    , timeFieldSet
    , timeFieldList
    , durationTimeFieldSet
    , durationTimeFieldList
    , DurationFields(..)
    , TimeFields(..)
    , DateFields(..)
    , DateTimeFields(..)

     -- * Smart Constructors
    , iTChar
    , iTDurationDate
    , iTFun
    , iTProd
    , gProd
    , iTSum
    , iTVar
    , iTUnit
    , iApp
    , iLet
    , iIf
    , iDeref
    , iDuration
    , iTypeOf
    , iDate
    , iTime
    , iDateTime
    , iProj
    , iVVar
    , iVLam
    , iVChar
    , iVLeft
    , iVRight
    , iVTuple
    , iVUnit
    , iBinOpCore
    , iRecAcc
    , iRecMod
    , iPrimOp
    , iBinOpSugar
    , iTypePred
    , iPick
    , iListComprehension
    , iOperatorSection
    ) where

import Text.PrettyPrint.Leijen as PP
import Poets.Data.Value
import Poets.Data.Value.Utils
import Poets.Data.Type
import Poets.Data.Type.Render
import Data.Comp
import Data.Comp.Derive hiding (showF)

import Data.Time.LocalTime ()
import Data.Time.Calendar

import Data.Maybe
import Data.Foldable hiding (concat, concatMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec.Pos

{-|
  This type represents a position in the original source code.
-}
type SrcPos = Maybe SourcePos


instance EqF f => EqF (f :&: SrcPos) where
     (v1 :&:_) `eqF` (v2 :&: _) = v1 `eqF` v2

instance OrdF f => OrdF (f :&: SrcPos) where
     (v1 :&:_) `compareF` (v2 :&: _) = v1 `compareF` v2

instance Eq (f a) => Eq ((f :&: SrcPos) a) where
    (v1 :&:_) == (v2 :&: _) = v1 == v2

instance Ord (f a) => Ord ((f :&: SrcPos) a) where
    (v1 :&:_) `compare` (v2 :&: _) = v1 `compare` v2


{-| This is the type of a type variable identifier.  -}
data TVarId = TVarId String Int
    deriving (Eq, Ord)



class VarId a where
    varName :: a -> String

instance VarId TVarId where
    varName (TVarId n _) = n


tVarId :: String -> TVarId
tVarId = flip TVarId 0

instance Pretty TVarId where
    pretty (TVarId n 0) = text n
    pretty (TVarId n i) = text n <> PP.int i

instance Show TVarId where
    show = show . pretty


{-| This is the type of a type variable identifier.  -}
data PTypeVar e = TVar TVarId
    deriving (Eq, Ord)

{-| This data type represents the additional types for Parrot.  -}

data PTypeFun e = TFun e e
                | TSum e e
                | TProd e e
                deriving (Eq, Ord)

data PTypeConst e = TDurationDate
                | TChar
                | TUnit
                deriving (Eq, Ord)

$( derive 
   [makeFunctor, makeFoldable, makeTraversable,
    makeEqF, makeOrdF, smartConstructors]
   [''PTypeFun, ''PTypeConst, ''PTypeVar] )

gProd :: (PTypeConst :<: f, PTypeFun :<: f) => [e] -> Context f e
gProd [] = iTUnit
gProd (t:ts) = iTProd (Hole t) (gProd ts)


{-|
  This type represents the signature for type expressions used in Parrot.
-}

type TypeSigPos = 
        PTypeVar :&: SrcPos
    :+: PTypeConst :&: SrcPos
    :+: PTypeFun :&: SrcPos
    :+: TypeConstant :&: SrcPos
    :+: TypeList :&: SrcPos
    :+: TypeEnt :&: SrcPos

{-|
  This type represents the signature for atomic types used in Parrot.
-}

type AtomicTypeSigPos = 
        PTypeVar :&: SrcPos
    :+: PTypeConst :&: SrcPos
    :+: TypeConstant :&: SrcPos

{-|
  This type represents the signature for atomic types used in Parrot.
-}

type ConstTypeSigPos = 
        PTypeConst :&: SrcPos
    :+: TypeConstant :&: SrcPos
              


{-|
  This type represents the signature for type expressions used in Parrot.
-}

type TypeSig = 
        PTypeVar
    :+: PTypeConst
    :+: PTypeFun
    :+: TypeConstant
    :+: TypeList
    :+: TypeEnt

{-|
  This type represents the signature for atomic types used in Parrot.
-}

type ConstTypeSig = 
    PTypeConst
    :+: TypeConstant

{-|
  This type represents the signature for atomic types used in Parrot.
-}

type AtomicTypeSig = 
        PTypeVar
    :+: PTypeConst
    :+: TypeConstant

{-| This Type represents the constant type expressions used in Parrot -}

type ConstPType = Const ConstTypeSigPos

{-| This Type represents the constant type expressions used in Parrot -}

type ConstPType' = Const ConstTypeSig

{-| This Type represents the atomic type expressions used in Parrot -}

type AtomicPType = Term AtomicTypeSigPos

{-| This Type represents the atomic type expressions used in Parrot -}

type AtomicPType' = Term AtomicTypeSig

{-| This Type represents the type expressions used in Parrot -}

type PType = Term TypeSigPos

{-| This Type represents the type expressions used in Parrot -}

type PType' = Term TypeSig
    


{-| This data type represents type constraints used in Parrot.
-}

data TypeConstr e = SubType e e  -- ^ Subtyping constraint
                  | HasField e FieldName e
                  | Ord e
                  | Eq e
                deriving (Eq, Ord)

$( derive 
   [makeFunctor, makeFoldable, makeTraversable,
    makeEqF, makeOrdF]
   [''TypeConstr] )

type TypeConstrPos = TypeConstr :&: SrcPos


instance Render TypeConstr where
    render (SubType  t1 t2) = t1 <+> text "<" <+> t2
    render (HasField t1 field t2) = t1 <> dot
                                    <> text field <+> colon
                                    <+> t2
    render (Ord t) = text "Ord" <+>  t
    render (Eq t) = text "Eq" <+>  t

instance Show a => Show (TypeConstr a) where
    show = show . render . fmap (text . show)
instance Show a => Show (TypeConstrPos a) where
    show = show . render . fmap (text . show)


{-| This data type represents atomic type constraints used in Parrot. Note that 
    field constraints still allows non-atomic field types.
-}

data AtomicTypeConstr e = ASubType AtomicPType AtomicPType -- ^ Subtyping constraint
                        | AHasField AtomicPType FieldName PType
                        | AOrd AtomicPType
                        | AEq AtomicPType
                        deriving (Eq, Ord)

$( derive 
   [makeFunctor, -- makeFoldable, makeTraversable,
    makeEqF, makeOrdF]
   [''AtomicTypeConstr] )

type AtomicTypeConstrPos = AtomicTypeConstr :&: SrcPos

instance Show (AtomicTypeConstr a) where
    show = show . fromAtomicConstraint
instance Show (AtomicTypeConstrPos a) where
    show = show . fromAtomicConstraintPos

fromAtomicPType :: AtomicPType -> PType
fromAtomicPType = deepInject3

toAtomicPType :: PType -> Maybe AtomicPType
toAtomicPType ptype = 
  case project ptype of
    Just (TVar tvar :&: (pos::SrcPos)) ->  Just $ inject (TVar tvar :&: pos)
    Nothing ->  
      case project ptype of
        Just (TDurationDate :&: (pos::SrcPos)) ->  Just $ inject (TDurationDate :&: pos)
        Just (TChar :&: (pos::SrcPos)) ->  Just $ inject (TChar :&: pos)
        Just (TUnit :&: (pos::SrcPos)) ->  Just $ inject (TUnit :&: pos)
        Nothing -> 
          case project ptype of
            Just (TInt :&: (pos::SrcPos)) ->  Just $ inject (TInt :&: pos)
            Just (TBool :&: (pos::SrcPos)) ->  Just $ inject (TBool :&: pos)
            Just (TString :&: (pos::SrcPos)) ->  Just $ inject (TString :&: pos)
            Just (TTime :&: (pos::SrcPos)) ->  Just $ inject (TTime :&: pos)
            Just (TDate :&: (pos::SrcPos)) ->  Just $ inject (TDate :&: pos)
            Just (TDateTime :&: (pos::SrcPos)) ->  Just $ inject (TDateTime :&: pos)
            Just (TDuration :&: (pos::SrcPos)) ->  Just $ inject (TDuration :&: pos)
            Just (TReal :&: (pos::SrcPos)) ->  Just $ inject (TReal :&: pos)
            Just (TRecord n :&: (pos::SrcPos)) ->  Just $ inject (TRecord n :&: pos)
            Nothing -> Nothing

fromAtomicConstraint :: AtomicTypeConstr a -> TypeConstr PType
fromAtomicConstraint (ASubType a b) = SubType (fromAtomicPType a) (fromAtomicPType b)
fromAtomicConstraint (AHasField a n e) = HasField (fromAtomicPType a) n e
fromAtomicConstraint (AOrd a) = Ord (fromAtomicPType a)
fromAtomicConstraint (AEq a) = Eq (fromAtomicPType a)

fromAtomicConstraintPos :: AtomicTypeConstrPos a -> TypeConstrPos PType
fromAtomicConstraintPos (c :&: p) = (fromAtomicConstraint c :&: p)


{-|
  This data type represents type schemes.
-}
data TypeScheme = TypeScheme SrcPos [TypeConstrPos PType] PType


instance Pretty TypeScheme where
    pretty (TypeScheme _ constrs ty)
        = case constrs of
            [] -> renderTerm ty
            [constr] ->  pretty constr <+> text "=>" <+> renderTerm ty
            _ -> tupled (map pretty constrs) <+> text "=>" <+> renderTerm ty

instance Show TypeScheme where
    show = show . pretty
                                        

{-| This type represents (value) variable identifiers.  -}

data VVarId = VVarId String Int
    deriving (Eq, Ord)

instance VarId VVarId where
    varName (VVarId n _) = n

vVarId :: String -> VVarId
vVarId  = flip VVarId 0

instance Pretty VVarId where
    pretty (VVarId n 0) = text n
    pretty (VVarId n i) = text n <> PP.int i

instance Show VVarId where
    show = show . pretty

{-| This data type represents the signature extension for
Parrot values.  -}

data ValueExt e = VVar VVarId     -- ^variable
                | VLam [VVarId] e -- ^lambda abstraction
                | VChar Char      -- ^character value
                | VLeft e         -- ^left injection
                | VRight e        -- ^right injection
                | VTuple [e]       -- ^pairing
                | VUnit           -- ^unit element


{-|
  This type represents the signature for Parrot values.
-}
type ValueSigPos = Val :&: SrcPos
                :+: ValueExt :&: SrcPos

{-|
  This type represents the value expressions of Parrot.
-}
type PValue = Term ValueSigPos

data RefRec = IsRef {refRecName :: RecordName} | IsRec {refRecName :: RecordName}
            deriving (Eq,Ord)


iTEntRec :: (TypeEnt :<: f, TypeConstant :<: f) => RefRec -> Cxt h f a
iTEntRec (IsRef r) = iTEnt $ iTRecord r
iTEntRec (IsRec r) = iTRecord r

{-| This data type represents a single type case of a type case
distinction as used by ht @type of@ language construct. -}

data TypeCase e = TypeCase { typeCaseRec :: RefRec
                           , typeCaseBody :: e }
                  deriving (Eq, Ord)

data DefTypeCase e = DefTypeCase {defTypeCaseBody :: e}
                  deriving (Eq, Ord)

type DefTypeCasePos = DefTypeCase :&: SrcPos

{-|
  This data type enumerates all build-in binary operators of Parrot.
-}

data BinOpCore = OpPlus
           | OpMinus
           | OpTimes
           | OpDiv
           | OpAnd
           | OpOr
           | OpEq
           | OpNeq
           | OpLt
           | OpLe
           | OpGt
           | OpGe
           | OpDurPlus
           | OpDurMinus
           | OpCons
             deriving (Eq, Ord)

{-|
  This data type enumerates all binary operators of Parrot implementes as syntactic sugar.
-}

data BinOpSugar = OpAppend
                  deriving (Eq, Ord)

instance Show BinOpCore where
    show OpPlus = "+"
    show OpMinus = "-"
    show OpTimes = "*"
    show OpDiv = "/"
    show OpAnd = "&&"
    show OpOr = "||"
    show OpNeq = "/="
    show OpEq = "=="
    show OpLt = "<"
    show OpLe = "<="
    show OpGt = ">"
    show OpGe = ">="
    show OpDurPlus = "<+>"
    show OpDurMinus = "<->"
    show OpCons = "#"

instance Show BinOpSugar where
    show OpAppend = "++"

data PrimOp = Fold
            | Not
            | Case
            | Error
            | Events
            | LeftOp
            | RightOp
              deriving (Eq, Ord)

instance Show PrimOp where
    show Fold = "fold"
    show Not = "not"
    show Case = "case"
    show LeftOp = "Inl"
    show RightOp = "Inr"
    show Error = "error"
    show Events = "events"

{-| This data type represents the type of a projection (from a product
type).  -}

data Proj = ProjComp Int
            deriving (Eq, Ord)

instance Show Proj where
    show (ProjComp i) = show i

{-| This data type represents simple let bindings. -}

data LetBind e = LetBind { letBindVar :: VVarId
                         , letBindArgs :: [VVarId]
                         , letBindBody :: e }
                 deriving (Eq, Ord)

type VFieldPos = VField :&: SrcPos
type LetBindPos = LetBind :&: SrcPos
type TypeCasePos = TypeCase :&: SrcPos

type DurationExp e = VDuration (Maybe e)

instance Eq e => Eq (DurationExp e)  where
    x == y = toList x == toList y

instance Ord e => Ord (DurationExp e) where
    x `compare` y = toList x `compare` toList y




{-| This data type represents the (optional) time specification of a
date expression. That is, it contains at least two components, the
hours and the minutes, and possibly an additional one representing the
seconds. -}

data TimeExp e = TimeExp {
      timeExpHour :: e,
      timeExpMinute :: e,
      timeExpSecond :: Maybe e
    }
                 deriving (Eq, Ord)



{-| This data type represents the day specification of a date
expression. That is, it has three components representing year, month,
and day respectively. -}

data DayExp e = DayExp {
      dayExpYear :: e,
      dayExpMonth :: e,
      dayExpDay :: e
    }
                deriving (Eq, Ord)

$(derive [makeFunctor, makeFoldable, makeTraversable, makeEqF, makeOrdF] [''TimeExp, ''DayExp])

data Deref = DerefNow | DerefCxt
           deriving (Eq, Ord)

{-| This data type represents the extensions to the Parrot core value
signature that then makes up the signature Parrot expressions.  -}

data ExpExt e = App e e
              | BinOpCore e BinOpCore e
              | Let [LetBindPos e] e
              | TypeOf {
                  typeOfVar :: (Maybe VVarId),
                  typeOfExp :: e,
                  typeOfCases :: [TypeCasePos e],
                  typeOfDefault ::(Maybe (DefTypeCasePos e)) }
              | RecAcc e FieldName
              | RecMod e [VFieldPos e]
              | DateTime e e
              | Date (DayExp e)
              | Time (TimeExp e)
              | Duration (DurationExp e)
              | If e e e
              | PrimOp PrimOp
              | Proj e Proj
              | Deref e Deref
                
data DurationFields = DurSec | DurMin | DurHour | DurDay | DurWeek | DurMon | DurYear
                    deriving (Eq, Ord)
                             
instance Show DurationFields where
  show DurSec = "seconds"
  show DurMin = "minutes"
  show DurHour = "hours"
  show DurDay = "days"
  show DurWeek = "weeks"
  show DurMon = "months"
  show DurYear = "years"

durationFieldMap :: Map String DurationFields
durationFieldMap = Map.fromList $ concatMap norm durNames
  where norm (n,l) = map (,n) l
        durNames = [(DurSec,["s", "sec", "secs" , "second", "seconds"]),
                      (DurMin,["m", "min", "mins", "minute", "minutes"]),
                      (DurHour, ["h", "hour", "hours"]),
                      (DurDay, ["d", "day", "days"]),
                      (DurWeek, ["w", "week", "weeks"]),
                      (DurMon,["mon", "mons", "month", "months"]),
                      (DurYear,["y", "year", "years"])]

durationFieldSet :: Set String
durationFieldSet = Map.keysSet durationFieldMap

durationFieldList :: [String]
durationFieldList = Map.keys durationFieldMap


data TimeFields = TimeSec | TimeMin | TimeHour
                    deriving (Eq, Ord)
                             
data DateFields = DateDay | DateMon | DateYear
                    deriving (Eq, Ord)
                             
data DateTimeFields = TimeField TimeFields | DateField DateFields 
                    deriving (Eq, Ord)
                             
                             
timeFieldMap :: Map String TimeFields
timeFieldMap = Map.fromList $ concatMap norm durNames
  where norm (n,l) = map (,n) l
        durNames = [(TimeSec,["s", "sec", "secs" , "second", "seconds"]),
                    (TimeMin,["m", "min", "mins", "minute", "minutes"]),
                    (TimeHour, ["h", "hour"])]
                   
dateFieldMap :: Map String DateFields
dateFieldMap = Map.fromList $ concatMap norm durNames
  where norm (n,l) = map (,n) l
        durNames = [(DateDay, ["d", "day"]),
                    (DateMon,["mon", "month"]),
                    (DateYear,["y", "year"])]
                   
dateTimeFieldMap :: Map String DateTimeFields
dateTimeFieldMap = Map.map TimeField timeFieldMap `Map.union` Map.map DateField dateFieldMap
                   
timeFieldSet :: Set String
timeFieldSet = Map.keysSet timeFieldMap

timeFieldList :: [String]
timeFieldList = Map.keys timeFieldMap
                   
dateFieldSet :: Set String
dateFieldSet = Map.keysSet dateFieldMap

dateFieldList :: [String]
dateFieldList = Map.keys dateFieldMap
                   
dateTimeFieldSet :: Set String
dateTimeFieldSet = Map.keysSet dateTimeFieldMap

dateTimeFieldList :: [String]
dateTimeFieldList = Map.keys dateTimeFieldMap

durationTimeFieldSet :: Set String
durationTimeFieldSet = timeFieldSet `Set.intersection` durationFieldSet

durationTimeFieldList :: [String]
durationTimeFieldList = Set.toList durationTimeFieldSet


{-| This data type represents the extensions to the Parrot syntax that
are merely syntactic sugar. -}

data ExpSugar e 
    = BinOpSugar e BinOpSugar e -- ^Binary operators implementes as sugar.
    | TypePred e RefRec -- ^Returns True if the expression is of the given type and False otherwise.
    | Pick RefRec -- ^Returns a coercing function, ie. pick Sub : (Sub < Sup) => [Sup] -> [Sub]
    | ListComprehension e [ListFilters e] -- ^A set-comprehension-like syntactic sugar for map and filter.
    | OperatorSection (OperatorSection e) -- ^Operator sections (to support (+1) as a shorthand for \x -> x + 1, etc.)
      deriving (Eq, Ord)

{-| Operator sections reprsent partially applied operators. For example, the section (*2)
represented as OperatorSection (NormalSection OpTimes (Just (RightOperand (VInt 1)))) is
equivalent to the lambda expression \x -> x * 2. -}

data OperatorSection e
    = NormalSection BinOpCore (Operand e) -- ^Sections that use the BinOp operators.
    | TypePredSection RefRec -- ^Sections for the TypePred (:?) operator.
      deriving (Eq, Ord)

{-| Represents an operand on the left hand or right hand side of an operator. -}

data Operand e
    = NoOperand      -- ^When no operand is given.
    | LeftOperand e  -- ^When only the left operand is given.
    | RightOperand e -- ^When only the right operand is given.
      deriving (Eq, Ord)

{-| This data type represents the elements in a list comprehension 
after the where bar (|). -}

data ListFilters e 
    = ListGuard e -- ^All results where the expression evaluates to False are thrown out.
    | ListGenerator VVarId (Maybe RefRec) e -- ^Binds the variable to every element in the list, one at a time.
                                                --  If the record name is given, the variable name will have this
                                                --  type and all elements that are not of this type will be skipped.
    | ListLet VVarId (Maybe RefRec) e       -- ^Binds a variable to a value.
                                                --  If the record name is given, the variable name will have this type 
                                                --  and all results where the value is not of this type will be skipped.
             deriving (Eq, Ord)




instance Render DayExp where 
    render (DayExp y m d) = hcat $ punctuate (char '-') $ [y,m,d]

instance Render TimeExp where
    render (TimeExp h m s) = hcat $ punctuate colon ([h,m] ++ toList s)





{-| This data type represents the signature of Parrot's expressions.
-}

type ExpCoreSigPos = ExpExt :&: SrcPos
                 :+: ValueSigPos

{-|
  This data type represents core expressions in Parrot.
-}

type PExpCore = Term ExpCoreSigPos

{-| This data type represents the signature of Parrot's expressions.
-}

type ExpSigPos = ExpSugar :&: SrcPos
                 :+: ExpCoreSigPos

{-|
  This data type represents core expressions in Parrot.
-}
type PExp = Term ExpSigPos


{-|
  This type represents function name identifiers
-}

type FunId = String


{-|
  This data type represents top-level declarations in a Parrot program.
-}
data Decl f = FunDecl {
      declPos :: SrcPos,
      declId :: FunId,
      declType :: (Maybe TypeScheme),
      declArgs :: [VVarId],
      declBody :: Term f
    }  | RecDecl {
      declPos :: SrcPos,
      declRec :: RecordName,
      declSuper :: [RecordName],
      declFields :: [FieldDecl]
    }

{-| This data type represents top-level declarations in a Parrot program without sugar -}

type DeclCore = Decl ExpCoreSigPos

{-| This data type represents top-level declarations in a Parrot program with sugar -}

type DeclSugar = Decl ExpSigPos


instance (Functor f, Render f) => Pretty (Decl f) where
    pretty (FunDecl _ fun tySch vars exp) = 
        text fun <+> colon <+> pretty tySch
        PP.<$> text fun <+> hsep (map pretty vars) <+> equals <+> renderTerm exp
    pretty (RecDecl _ rec supm fields) =
        text "rec" <+> supp <> text rec <+> equals <+> braces (cat $ punctuate (comma <> softline) (map pretty fields))
            where supp = case supm of
                           [] -> PP.empty
                           _ -> sep (punctuate comma $ map text supm) <+> char '>' <+> PP.empty

instance (Functor f, Render f) => Show (Decl f) where
    show = show . pretty

{-| This data type represents declarations of field names as used in
record type declarations.  -}

data FieldDecl = FieldDecl SrcPos FieldName PType

instance Pretty FieldDecl where
    pretty (FieldDecl _ field ty) = text field <+> colon <+> renderTerm ty
instance Show FieldDecl where
    show = show . pretty

{-| This data type represents a Parrot program with parameterized
  declarations. 
-}

data Program f = Program { programName :: Maybe String,
                           programDesc :: Maybe String,
                           programTags :: [String],
                           programDecls :: [Decl f] }

{-| This data type represents a Parrot program without sugar -}

type ProgramCore = Program ExpCoreSigPos

{-| This data type represents a Parrot program with sugar -}

type ProgramSugar = Program ExpSigPos

data Library f = Library {libraryDecls :: [Decl f]}


{-| This data type represents a Parrot program without sugar -}

type LibraryCore = Library ExpCoreSigPos

{-| This data type represents a Parrot program with sugar -}

type LibrarySugar = Library ExpSigPos

instance (Functor f, Render f) => Pretty (Program f) where
    pretty (Program name desc tags decls)
        = vsep [text "name" <> colon <+> text (fromMaybe "" name),
                PP.empty,
                text "description" <> colon <+> text (fromMaybe "" desc) ,
                PP.empty,
                text "tags" <> colon <+> (sep $ punctuate comma (map text tags)) ,
                PP.empty,
                vsep $ map pretty decls]
instance (Functor f, Render f) => Show (Program f) where
    show = show . pretty

$( derive 
   [makeFunctor, makeFoldable, makeTraversable,
    makeEqF, makeOrdF, smartConstructors]
   [''ValueExt, ''ExpExt, ''ExpSugar] )

$( derive 
   [makeFunctor, makeFoldable, makeTraversable]
   [''LetBind, ''TypeCase, ''DefTypeCase, ''ListFilters, ''Operand, ''OperatorSection])


instance Render LetBind where
    render (LetBind var args exp) =  pretty var <+> hsep (map pretty args) <+> equals <+> exp
instance (Functor f, Render f) => Show (LetBind (Term f)) where
    show = show . pretty

instance Show RefRec where
    show (IsRef rec) = "<" ++ rec ++ ">"
    show (IsRec rec) = rec

instance Render TypeCase where
    render (TypeCase rec exp) =  text (show rec) <+> text "->" <+> exp
instance (Functor f, Render f) => Show (TypeCase (Term f)) where
    show = show . pretty


instance (Render f) => Render (f :&: SrcPos) where
    render (v :&: _) = render v

instance Render PTypeVar where
    render (TVar v) = pretty v

instance Render PTypeFun where
    render (TFun e1 e2) = parens $ e1 <+> text "->" <+> e2
    render (TSum e1 e2) = parens $ e1 <+> text "+" <+> e2
    render (TProd e1 e2) = parens $ e1 <+> text "*" <+> e2
                         

instance Render PTypeConst where
    render TChar = text "Char"
    render TUnit = text "()"
    render TDurationDate = text "DurationDate"

instance Render Val where
    render (VInt i) = PP.int i
    render (VBool b) = text $ show b
    render (VString s) = dquotes $ text s
    render (VDateTime d) = text $ formatDateTime "<<%F %T>>" d
    render (VDate d) = text "<<" <> text (showGregorian d) <> text ">>"
    render (VTime t) = text "<<" <> text (show t) <> text ">>"
    render (VDuration VD 
            { durationSeconds = s,
              durationMinutes = min,
              durationHours = h,
              durationDays = d,
              durationWeeks = w,
              durationMonths = mon,
              durationYears = y })
        = text "<<" <> durDoc <> text ">>"
        where durDoc = sep $ punctuate comma (map compDoc comps)
              comps = [("second",s),("minute",min),("hour", h),("day", d),
                       ("week", w), ("month", mon), ("year", y)]
              compDoc (lab, count) = int count <+> text lab
                                  <> if count == 1 then PP.empty else char 's'
                                  
    render (VReal d) = PP.double d
    render (VEnt (VEntity name id mcxt)) = text name <+> (PP.angles $ sep $
                                              punctuate comma contents)
        where contents = PP.int id : rest
              rest = case mcxt of
                       Nothing -> []
                       Just cxt -> [text $ formatDateTime "%F %T" cxt]
    render (VRecord (VR name fields)) = text name <+> PP.braces fields'
        where fields' = sep $ punctuate comma (map render $ fieldsList fields)
    render (VList l) = PP.brackets $ sep $ punctuate comma l

instance Render VField where
    render (VF name val) = text name <+> equals <+> val
instance (Functor f, Render f) => Show (VField (Term f)) where
    show = show . pretty

instance Render ValueExt  where
    render (VVar id) = pretty id
    render (VLam ids body) = PP.parens $ char '\\' <+> hsep (map pretty ids) <+> text "->" <+> body
    render (VLeft e)  = parens $ text "Inl" <+> e
    render (VRight e) = parens $ text "Inr" <+> e
    render (VTuple es) = parens $ sep $ punctuate comma es
    render (VChar c) = squotes $ char c
    render VUnit = text "()"

instance Render ExpExt where
    render (App e1 e2) = parens $ e1 <+> e2
    render (BinOpCore e1 op e2) = parens $ e1 <+> text (show op) <+> e2
    render (Let binds body) =text "let" <+> vsep (map render binds)
                       <$> text "in" <+> body
    render (TypeOf mid e cases defCase) = text "type" <+> mid' <> e <+> text "of"
                       <$> vsep (map render cases) <> defCase'
        where defCase' = case defCase of
                          Nothing -> PP.empty
                          Just (DefTypeCase e :&: _) -> PP.empty <$> text "_" <+> text "->" <+> e
              mid' = case mid of 
                      Just id -> text (show id) <+> text "= "
                      Nothing -> PP.empty
    render (RecAcc e field) = e <> char '.' <> text field
    render (RecMod e fields) = parens $ e <+> brackets fields'
        where fields' = sep $ punctuate comma (map render fields)
    render (DateTime day time) = text "<<" <> day <+> time <> text ">>"
    render (Date day)  = text "<<" <> render day <> text ">>"
    render (Time time)  = text "<<" <> render time <> text ">>"
    render (Duration VD
            { durationSeconds = s,
              durationMinutes = min,
              durationHours = h,
              durationDays = d,
              durationWeeks = w,
              durationMonths = mon,
              durationYears = y })
        = text "<<" <> durDoc <> text ">>"
        where durDoc = sep $ punctuate comma (mapMaybe compDoc comps)
              comps = [("second",s),("minute",min),("hour", h),("day", d),
                       ("week", w), ("month", mon), ("year", y)]
              compDoc (lab, Just count) = Just $ count <+> text lab
                                               <> case show count of
                                                    "1" -> PP.empty
                                                    _ -> char 's'
              compDoc (_,Nothing) = Nothing
    render (If cond ifb elseb) = text "if" <+> cond <+> text "then"
                                 <+> ifb <+> text "else" <+> elseb
    render (PrimOp op) = text $ show op
    render (Deref e mode) = e <> mode'
        where mode' = case mode of
                        DerefNow -> char '!'
                        DerefCxt -> char '@'
    render (Proj e proj) = e <> char '.' <> text (show proj)

instance Render ExpSugar where
    render (BinOpSugar e1 op e2) = parens $ e1 <+> text (show op) <+> e2
    render (TypePred e rec) = parens $ e <+> text ":?" <+> text (show rec)
    render (Pick rec) = parens $ text "pick" <+> text (show rec)
    render (ListComprehension e filters) = brackets $ e <+> text "|" <+> filters'
        where filters' = sep $ punctuate comma (map render filters)
    render (OperatorSection (NormalSection op e)) = prettySection (show op) e
    render (OperatorSection (TypePredSection  recordName)) = text $ "(:? " ++ show recordName ++ ")"

prettySection op e = case e of 
    NoOperand      -> parens $ text op
    LeftOperand e  -> parens $ pretty e <+> text op
    RightOperand e -> parens $ text op <+> pretty e

prettyFilter v t o e = pretty v <+> typeFilter t <+> text o <+> e
    where typeFilter (Just t) = text ":" <+> text (show t)
          typeFilter Nothing = text ""

instance Render ListFilters where
    render (ListGuard e) = e
    render (ListLet v t e) = prettyFilter v t "=" e
    render (ListGenerator v t e) = prettyFilter v t "<-" e

