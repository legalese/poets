{-# LANGUAGE TemplateHaskell, TypeOperators, CPP #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Compiler.Haskell
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines a translation of Parrot into Haskell using
-- GHC's AST representation of Haskell.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Compiler.Haskell
    (toHsModule,
     HsName,
     HsModule,
     CompileException (..),
     repFunName,
     repInterfaceFunName) where

import Poets.Reporting.Language.Parrot.Compiler.MetaInterface
import Poets.Reporting.Language.Parrot.ReportLibrary
import Poets.Reporting.Language.Parrot.Syntax hiding (toSeconds)
import Poets.Data.Value.Utils

import HsSyn
import GHC
import Bag
import RdrName
import BasicTypes
import OccName
import FastString

import Data.Foldable (toList)
import Data.Comp.Derive

import Control.Monad
import Control.Monad.Error



$(declRepFuns 
  ["TypeOf", "TypeOf'", "RefTypeOf", "RefTypeOf'", "Error", "Record", "List",
   "Cons", "FieldLookup", "Foldr", "If",
   "False", "True", "Eq", "Neq", "Lt", "Gt", "Lte", "Gte", 
   "And", "Or", "Not", "Plus", "Minus", "Times", "CompDateTime",
   "Integer", "Rational", "Div", "Date", "Time", "Date'", "Time'","DateTime'",
   "Ref","Duration'", "Char", "CharList", "Left", "Right", "Tuple", "Unit", "FieldsMod",
   "Proj", "Case", "Events", "DurPlus", "DurMinus", "DerefNow", "DerefCxt"])

{-| This function constructs an integer representation in the form of
a Haskell expression.  -}

mkInt :: Integral a => a -> HsExp
mkInt = mkHsExp  . HsLit . HsInt . toInteger

mkRepInt :: Integral a => a -> HsExp
mkRepInt i =  mkApp hsInteger (mkInt $ toInteger i)

{-| This function constructs a character representation in the form of
a Haskell expression.  -}

mkChar :: Char -> HsExp
mkChar = mkHsExp . HsLit . HsChar

{-| This function constructs a character representation in the POETS
data type system in the form of a corresponding Haskell expression
using 'mkChar' and 'hsChar'.  -}

mkChar' :: Char -> HsExp
mkChar' ch = hsChar `mkApp` mkChar ch


#if __GLASGOW_HASKELL__ < 708
mkList :: [HsExp] -> HsExp
mkList exps = mkHsExp $ ExplicitList placeHolderType exps
#else
mkList :: [HsExp] -> HsExp
mkList exps = mkHsExp $ ExplicitList placeHolderType Nothing exps
#endif


mkTuple :: [HsExp] -> HsExp
mkTuple comps = mkHsExp $ ExplicitTuple (map Present comps) Boxed

mkPair :: (HsExp,HsExp) -> HsExp
mkPair (e1,e2) = mkTuple [e1,e2]

mkApp e1 e2 = mkHsExp $ HsApp e1 e2

mkApps :: [HsExp] -> HsExp
mkApps = foldl1 mkApp

mkString :: String -> HsExp
mkString = mkHsExp . HsLit . HsString . mkFastString

mkStringRep :: String -> HsExp
mkStringRep str = hsCharList `mkApp` mkString str


-- mkSimpGRHS :: LHsExpr id -> GRHSs id
mkSimpGRHS body = GRHSs {grhssGRHSs = [noLoc $ GRHS [] body],grhssLocalBinds = EmptyLocalBinds}

mkVarId :: VVarId -> RdrName
mkVarId id = mkSimpId $ show id

mkSimpId :: String -> RdrName
mkSimpId id = mkRdrUnqual (mkVarOccFS $ mkFastString id)

mkVar :: VVarId -> HsExp
mkVar id = mkHsExp $ HsVar $ mkVarId id


#if __GLASGOW_HASKELL__ < 708
mkMG = MatchGroup
#else
mkMG alts ty = MG alts [] ty Generated
#endif



-- mkVarsMatch :: [VVarId] -> LHsExpr RdrName -> MatchGroup RdrName
mkVarsMatch vars body = mkMG [noLoc $ Match vars' Nothing body'] placeHolderType
    where vars' = map (noLoc . VarPat . mkVarId) vars
          body' = mkSimpGRHS body

{- TODO: This is used to hack around the missing parenthesis bug around lambda 
   expressions in the pretty printer. Please delete when it is no longer needed. -}
mkExplicitGrouping e = mkHsExp $ HsLet (HsValBinds $ ValBindsIn (listToBag [bind e]) []) (mkVar id)
    where 
        id = vVarId "_fakeExplicitGroupingTemporaryVariable"
        bind e = noLoc $ varBind (mkVarId id) e

mkLam :: [VVarId] -> HsExp -> HsExp
mkLam [] body = body
mkLam args body = mkExplicitGrouping $ mkHsExp $ HsLam $ mkVarsMatch args body

repFunName = "report"

repInterfaceFunName = "__report__"

newtype CompileException = CompileException String

instance Show CompileException where
    show (CompileException msg) = msg

instance Error CompileException where
    strMsg = CompileException

toHsModule :: Maybe ReportLibrary -> Bool -> [DeclCore]
           -> String -> Either CompileException (HsModule HsName)
toHsModule lib reportFunction decls name = do
  funs' <- genFuns
  return HsModule {
               hsmodName = Just $ noLoc $ mkModuleName name,
               hsmodExports = if reportFunction 
                              then Just [noLoc $ IEVar $ mkSimpId repInterfaceFunName]
                              else Nothing,
               hsmodImports = addLib [interfaceModuleImport, preludeModuleImport],
               hsmodDecls = funs',
               hsmodDeprecMessage = Nothing,
               hsmodHaddockModHeader = Nothing }
    where genFuns = if reportFunction
                    then case repFun of
                           Nothing -> fail $ 
                                      "a report specification has to contain a main report function of name '"
                                      ++ repFunName ++ "'"
                           Just fun -> return $ fun : funs
                    else return funs
          repFun = repFunDecl decls
          funs = toHsDecls decls
          addLib imports = case lib of 
                             Nothing -> imports
                             Just lib -> preludeLibraryImport lib : imports
#if __GLASGOW_HASKELL__ < 700
varBind id rhs = VarBind id rhs
#else
varBind id rhs = VarBind id rhs False
#endif

{-| This function generates a function declaration that calls the
reporting function of the name given by 'repFunName' but instead of
several arguments the generated function only has a single argument
which is a list containing the arguments to the reporting
function. The integer argument specifies the number of arguments the
reporting function has. -}

repFunDecl' :: Int -> LHsDecl HsName
repFunDecl' n = noLoc$ ValD (varBind (mkSimpId repInterfaceFunName) rhs )
    where args = map (mkVarId . VVarId "x") [1..n]
          rhs = mkHsExp $ HsPar $ mkHsExp $ HsLam $ mkMG [noLoc $ Match [noLoc pat] Nothing body] placeHolderType
#if __GLASGOW_HASKELL__ < 708
          pat = ListPat (map (noLoc . VarPat) args) placeHolderType
#else
          pat = ListPat (map (noLoc . VarPat) args) placeHolderType Nothing
#endif
          body = GRHSs [noLoc $ GRHS [] body'] EmptyLocalBinds
          repFun = mkHsExp $ HsVar $ mkSimpId repFunName
          args' = map (mkHsExp . HsVar) args
          body' = mkApps (repFun : args')


repFunDecl :: [DeclCore] -> Maybe (LHsDecl HsName)
repFunDecl decls = foldr (mplus . funDecl) Nothing decls
    where funDecl (FunDecl _ name _ args _)
              | name == repFunName = Just $ repFunDecl' (length args)
          funDecl _ = Nothing


toHsDecl :: DeclCore -> [LHsDecl HsName]
toHsDecl RecDecl {} = []
toHsDecl (FunDecl _ name _ args rhs) = [noLoc$ ValD (varBind (mkSimpId name) (mkLam args $ toHsExp rhs))]


toHsDecls :: [DeclCore] -> [LHsDecl HsName]
toHsDecls = concatMap toHsDecl


toHsExp :: (ToHsExpAlg f) => Term f -> HsExp
toHsExp = cata toHsExpAlg

{-| This class defines algebras which allow to convert Parrot
expressions into Haskell expressions.-}

class (Functor f) => ToHsExpAlg f where
    toHsExpAlg :: f HsExp -> HsExp


instance (ToHsExpAlg f) => ToHsExpAlg (f :&: a) where
    toHsExpAlg = liftA toHsExpAlg

$(derive [liftSum] [''ToHsExpAlg])


#if __GLASGOW_HASKELL__ < 702
hsFractional d = HsFractional $ toRational d
#else
hsFractional d = HsFractional $ FL (show d) (toRational d)
#endif 

instance ToHsExpAlg Val where
    toHsExpAlg (VInt i) = mkRepInt i
    toHsExpAlg (VBool True) = hsTrue
    toHsExpAlg (VBool False) = hsFalse
    toHsExpAlg (VString s) = mkStringRep s
    toHsExpAlg (VDateTime dt) = mkApps $ hsDateTime' : mkInt y : map mkInt [m, d, h, min, s]
      where (y,m,d,h,min,s,_) = dateTimeToComponents dt
    toHsExpAlg (VDate date) = mkApps $ hsDate' : mkInt y : map mkInt [m, d]
      where (y,m,d) = dateToYearMonthDay date
    toHsExpAlg (VTime t) = mkApps $ hsTime' :  map mkInt [h,m,s]
      where (h,m,s,_) = timeToHourMinSecMicroSec t
    toHsExpAlg (VEnt VEntity{ventId = id, ventType = rname}) = 
        mkApps [hsRef, mkString rname, mkInt id]
    toHsExpAlg (VDuration d) 
        = hsDuration' `mkApp` mkList (map mkRepInt (toList d))
    toHsExpAlg (VReal d) = mkApp hsRational (mkHsExp $ HsOverLit OverLit
                                               { ol_val = hsFractional d,
                                                 ol_rebindable = False,
                                                 ol_type = placeHolderType,
                                                 ol_witness = undefined} )
    toHsExpAlg (VRecord VR {vrecordName = recName, vrecordFields = fields})
        = mkApps [hsRecord, mkString recName, mkList $ map mkPair' $ fieldsList fields]
        where mkPair' (VF name val) = mkPair (mkString name, val)
    toHsExpAlg (VList l) = mkApp hsList (mkList l)


instance ToHsExpAlg ValueExt where
    toHsExpAlg (VVar id) = mkVar id
    toHsExpAlg (VLam args e) = mkLam args e
    toHsExpAlg (VChar ch) = mkChar' ch
    toHsExpAlg (VLeft e) = mkApp hsLeft e
    toHsExpAlg (VRight e) = mkApp hsRight e
    toHsExpAlg (VTuple es) = hsTuple `mkApp` mkList es
    toHsExpAlg VUnit = hsUnit


binOpToHsExp op = 
    case op of
      OpPlus     -> hsPlus
      OpMinus    -> hsMinus
      OpTimes    -> hsTimes
      OpDiv      -> hsDiv
      OpAnd      -> hsAnd
      OpOr       -> hsOr
      OpEq       -> hsEq
      OpNeq      -> hsNeq
      OpLt       -> hsLt
      OpLe       -> hsLte
      OpGt       -> hsGt
      OpGe       -> hsGte
      OpDurPlus  -> hsDurPlus
      OpDurMinus -> hsDurMinus
      OpCons     -> hsCons


instance ToHsExpAlg ExpExt where
    toHsExpAlg (App e1 e2) = mkApp e1 e2
    toHsExpAlg (BinOpCore e1 op e2) = binOpToHsExp op `mkApp` e1 `mkApp` e2
    toHsExpAlg (Let binds e) = build binds 
        where build [] = e
              build ((LetBind var args body :&: _):rest)
                  = mkApp (mkLam [var] (build rest)) (mkLam args body)
    toHsExpAlg (TypeOf _ _ [] Nothing) = error "type case expression without cases"
    toHsExpAlg (TypeOf _ _ [] (Just d)) = liftA defTypeCaseBody d
    toHsExpAlg (TypeOf mvar exp cases@(c:_) defCase) = mbind typeOf
        where ref = case c of
                      (TypeCase (IsRef _) _ :&: _) -> True
                      _ -> False
              cases' = mkList $ map mkCase cases
              mkCase (TypeCase (IsRec recName) body :&: _) = mkPair (mkString recName, body)
              mkCase (TypeCase (IsRef recName) body :&: _) = mkPair (mkString recName, body)
              arg = case mvar of 
                      Nothing -> exp
                      Just var -> mkVar var
              mbind e = case mvar of 
                          Nothing -> e
                          Just var -> mkApp (mkLam [var] e) exp
              mkTypeOf = if ref then hsRefTypeOf else hsTypeOf
              mkTypeOf' = if ref then hsRefTypeOf' else hsTypeOf'
              typeOf = case defCase of
                         Nothing -> mkApps [mkTypeOf', arg, cases']
                         Just defCase -> mkApps [mkTypeOf, arg, cases',
                                                 liftA defTypeCaseBody defCase]
    toHsExpAlg (RecAcc e field) = mkApps [hsFieldLookup, e, mkString field]
    toHsExpAlg (RecMod e fieldAssigns) = mkApps [hsFieldsMod, e, assigns']
        where assigns' = mkList $ map (liftA assign) fieldAssigns
              assign (VF name val) = mkPair (mkString name,val)
    toHsExpAlg (DateTime d t) = mkApps [hsCompDateTime, d, t]
    toHsExpAlg (Date (DayExp y m d)) = mkApps [hsDate, y,m,d]
    toHsExpAlg (Time (TimeExp h min ms)) =
      mkApps [hsTime, h,min,s]
      where s = maybe (mkRepInt 0) id ms 
    toHsExpAlg (Duration d) =  mkApps $ hsDuration' : map tr (toList d)
        where tr Nothing = mkRepInt 0
              tr (Just e) = e
    toHsExpAlg (If e1 e2 e3) = mkApps [hsIf, e1, e2, e3]
    toHsExpAlg (PrimOp op) = case op of 
                               Fold -> hsFoldr
                               Not -> hsNot
                               Case -> hsCase
                               Error -> hsError
                               Events -> hsEvents
                               LeftOp -> hsLeft
                               RightOp -> hsRight
    toHsExpAlg (Proj e (ProjComp p)) = (hsProj  `mkApp` (mkInt p)) `mkApp` e
    toHsExpAlg (Deref e DerefNow) = hsDerefNow `mkApp` e
    toHsExpAlg (Deref e DerefCxt) = hsDerefCxt `mkApp` e
