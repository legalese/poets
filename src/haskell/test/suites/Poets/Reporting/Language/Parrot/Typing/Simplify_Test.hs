{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Poets.Reporting.Language.Parrot.Typing.Simplify_Test (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (test)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec.Pos
import Debug.Trace

import Data.Comp.Variables
import Poets.Data.Type
import Poets.Reporting.Language.Parrot.FreeVars
import Poets.Reporting.Language.Parrot.Typing.TypingMonad
import Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
import Poets.Reporting.Language.Parrot.Typing.Simplify.Simplify
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicTypes
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp
import Poets.Reporting.Language.Parrot.Typing.Simplify.SubtypeConstraints
import Poets.Reporting.Language.Parrot.Typing.Simplify.FieldConstraints
import Poets.Reporting.Language.Parrot.Typing.Simplify.FuhMishra
import Poets.Reporting.Language.Parrot.FreshVar
import Poets.Reporting.Language.Parrot.Syntax

import Data.Graph
import Control.Monad

---------------------------------------------------
----------------- Test Suits ----------------------
---------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Poets.Reporting.Language.Parrot.Typing.Simplify" [
        entailmentTest,
        lazyTest,
        tautologiesTest,
        domainTest,
        mergeFieldsTest,
        satisfiabilityTest{-,
        removeCyclesTest-}
    ]

entailmentTest = testGroup "Entailment reduction" [
        testCase "No protection" entailment1,
        testCase "Simplify constraints from filter" entailmentFilter,
        testCase "Do not over reduce symbol types" entailmentNotCollapseSymbols,
        testCase "Bug from our 'triple' example" entailmentTriple,
        testCase "Example from Fuh & Mishra paper" entailmentFuhMishraExample,
        testCase "entailmentReduction - Do not protect field constraints on merge down" entailmentNotProtectFieldDown,
        testCase "entailmentReduction - Protect field constraints on merge up" entailmentProtectFieldUp,
        testCase "entailmentReduction - Allow merge up when f-subsumed b a" entailmentNotProtectFieldUp
    ]

lazyTest = testGroup "Lazy instantiation reduction" [
        testCase "Lazy - Do not protect field constraints on down cast" lazyNotProtectFieldDown,
        testCase "Lazy - Protect field constraints on up cast" lazyProtectFieldUp
    ]

tautologiesTest = testGroup "Remove tautologies" [
        testCase "Remove (only) subtype tautologies" subtypeTautologies,
        testCase "Remove (only) field tautologies" fieldTautologies,
        testCase "Remove (only) Eq tautologies" eqTautologies,
        testCase "Remove (only) Ord tautologies" ordTautologies
    ]

domainTest = testGroup "Domain specific eleminations" [        
        testCase "Test all tautologies + some negative tests" domainSpecificElemination
    ]



mergeFieldsTest = testGroup "Merge subtype related field constraints" [
        testCase "Merge Fields 1" mergeFields1,
        testCase "Merge Fields 2" mergeFields2
    ]

satisfiabilityTest = testGroup "Satisfiability" [
        testCase "Satisfiability base cases" satisfiabilityBaseCases,
        testCase "Satisfiability combined cases" satisfiabilityCombined
    ]

{-removeCyclesTest = testGroup "Remove circles" [
        testCase "remove some cycles" combineSCC1
  ]-}
---------------------------------------------------
----------------- Auxiliaries ---------------------
---------------------------------------------------

--type ASubstitution = Subst AtomicTypeSigPos TVarId -- == Map AtomicPType TVarId

noPos = Just (newPos "NoPos" 0 0)

var :: String -> AtomicPType
var n = atomicRecomp (TVarId n 0, noPos)
vars = map var

recomp :: AtomicType -> AtomicPType
recomp a = atomicRecomp (a, noPos)

int  = recomp $ AConst SInt
bool  = recomp $ AConst SBool
string  = recomp $ AConst SString
date  = recomp $ AConst SDate
duration  = recomp $ AConst SDuration
double  = recomp $ AConst SReal
record n  = recomp $ AConst (SRecord n)
char  = recomp $ AConst SChar
unit  = recomp $ AConst SUnit
durationDate  = recomp $ AConst SDurationDate


--protect :: [AtomicPType] -> Set TVarId
--protect as = freeVars as 
protect :: [AtomicPType] -> [AtomicPType]
protect = id

sconstraint :: [(AtomicPType, AtomicPType)] -> [ASubtypeConstraint]
sconstraint = map (\(a,b)-> (a, b, Nothing))

ordconstraint :: [AtomicPType] -> [AConstraint a]
ordconstraint = map (\a -> AOrd a :&: Nothing)

eqconstraint :: [AtomicPType] -> [AConstraint a]
eqconstraint = map (\a -> AEq a :&: Nothing)

sconstraint' = map fromSubtypeConstraint . sconstraint
  
fconstraint :: [(AtomicPType, String, AtomicPType)] -> [AFieldConstraint]
fconstraint = map (\(r,n,t)-> (r, n, fromAtomicPType t, Nothing))

fconstraint' = map fromFieldConstraint . fconstraint
              
mkSubst ::[(AtomicPType, AtomicPType)] -> ASubstitution
mkSubst ps = 
  let toVar t = let AVar tvar = atomicDecomp t in tvar
  in Map.fromList [(toVar s, t) | (s, t) <- ps]

mkSubst' ::[(AtomicPType, AtomicPType)] -> Subst TypeSigPos TVarId
mkSubst' = fromAtomicSubst . mkSubst

fun :: AtomicPType -> AtomicPType -> PType
fun a b = 
  let pFun = TFun (fromAtomicPType a) (fromAtomicPType b) 
             :&: Nothing :: (PTypeFun :&: SrcPos) PType
  in inject $ pFun


mkRecordType :: String -> [(RecordName, AtomicPType)] -> 
                Set (Record PType) -> Bool -> Record PType
mkRecordType name fields super isAbstract = mkRecordType' name fields' super isAbstract
  where 
    fields' :: [(RecordName, PType)]
    fields' = map (mapSnd fromAtomicPType) fields 
    mapSnd f (a, b) = (a, f b)

mkRecordType' :: String -> [(RecordName, PType)] -> 
                Set (Record PType) -> Bool -> Record PType
mkRecordType' name fields super isAbstract = 
    Record {recordName = name,
            recordFields = newFieldEnv (map (\(n, t) -> 
                                             Field{fieldName=n,
                                                   fieldType=t,
                                                   fieldAttributes = Set.empty}) 
                                        fields),
            recordExtends = Set.map recordName super,
            recordAttributes = Set.empty
           }

emptyEnv :: ExtRecEnv
emptyEnv = newRecordEnv []

fruitEnv :: ExtRecEnv
fruitEnv = newRecordEnv [event, food, fruit, apple, banana, orange]
  where
    event = mkRecordType "Event" [("name", string)] Set.empty False
    food  = mkRecordType "Food" [("weight", double)] Set.empty True
    fruit = mkRecordType "Fruit" [("ripe", bool)] (Set.singleton food) False
    apple = mkRecordType "Apple" [("color", string)] (Set.singleton fruit) False
    banana = mkRecordType "Banana" [("length", int)] (Set.singleton fruit) False
    orange = mkRecordType "Orange" [("sections", int)] (Set.singleton fruit) False

assertEqualIgnoreOrder preface expects actuals = 
    assertEqual preface (Set.fromList expects) (Set.fromList actuals)
assertSubsetOf preface expects actuals = 
    assertBool preface (expects `Map.isSubmapOf` actuals)



runM :: FreshVarT TypingM a -> a
runM m = case runTypingM (runFreshVarT m) of
  Left err -> error $ show err
  Right res -> res

[a, b, c, d, e, f] = vars ["a", "b", "c", "d", "e", "f"]
[food, fruit, apple, banana, orange] = map record ["Food", "Fruit", "Apple", "Banana", "Orange"] 

---------------------------------------------------
----------------- Unit Tests ----------------------
---------------------------------------------------


--
-- Entailment reductions
-- 

fun' :: PType -> AtomicPType -> PType
fun' a b = 
  let pFun = TFun a (fromAtomicPType b) 
             :&: Nothing :: (PTypeFun :&: SrcPos) PType
  in inject $ pFun
     
protectedToType :: [AtomicPType] -> PType
protectedToType [] = fromAtomicPType int
protectedToType [a] = fromAtomicPType a
protectedToType (a:b:cs) = foldl fun' (fun a b) cs
  
gReduce protected subs = fuhMishraReduction gSubsumed (protectedToType protected) subs []

entailmentNotCollapseSymbols = do
    let [a, b] = vars ["a", "b"]
    let constraints = sconstraint [(a, bool), (bool, b)]
    let protected = protect []
    let (constraints', substitution) =  gReduce protected constraints
    assertEqualIgnoreOrder "constraints" 
        constraints
        constraints' 
    assertEqual 
        "substitution" 
        (mkSubst [])
        substitution

entailmentTriple = do
    let [a, b, c] = vars ["a", "b", "c"]
    let constraints = sconstraint [(a,b), (a, double), (int, a), (double, c)]
    let protected = protect [b, c]
    let (constraints', substitution) =  gReduce protected constraints
    assertEqual "constraints" 
        (Set.fromList constraints)
        (Set.fromList constraints')
    assertEqual "substitution" 
         (mkSubst [])
         substitution 


entailment1 = do
    let [a, b] = vars ["a", "b"]
    let constraints = sconstraint [(a,b)]
    let protected = protect []
    let (constraints', substitution) =  gReduce protected constraints
    assertEqual "constraints" 
        []
        constraints' 
    assertEqual "substitution size"
        1
        (length $ Map.toList substitution)
                   
    assertSubsetOf "substitution" 
         substitution 
         (mkSubst [(a,b), (b,a)])
       

entailmentFilter = do
    let [a, b, c, d, e, f, g, h, j, k1, k2] = 
            vars ["a", "b", "c", "d", "e", "f", "g", "h", "j", "k1", "k2"]
    let constraints = sconstraint [(j,a), (a,c), (c,e), (e,k1), (c,d), (d,g), 
                                  (d,b), (g,b), (h,b), (k2,f), (f, bool)]
    let protected = protect [k1, k2, b, j]
    let (constraints', substitution) =  gReduce protected constraints
    assertEqualIgnoreOrder "constraints" 
        (sconstraint [(j,b), (j,k1), (k2,bool)])
        constraints' 
    assertEqual
        "substitution" 
--        (mkSubst [(a,c),(c,j),(d,g),(e,j),(f,k2),(g,b),(h,b)]) -- Original one, but wrong
        (mkSubst [(a,j),(c,j),(e,j),(g,b),(h,b),(d,b),(f,k2)])
        substitution

entailmentFuhMishraExample = do
    let [a, b, c, d, e, f] = 
            vars ["a", "b", "c", "d", "e", "f"]
    let constraints = sconstraint [(c,a),(d,a),(c,b),(d,b),(e,c),(f,d)]
    let protected = protect [a, b, c]
    let (constraints', substitution) =  gReduce protected constraints
    assertEqualIgnoreOrder "constraints" 
        (sconstraint [(c,a), (c,b)])
        constraints' 
    assertEqual
        "substitution" 
        (mkSubst [(d,c),(e,c),(f,c)])
        substitution

    

entailmentNotProtectFieldDown = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let scs  = sconstraint' [(b,a)]
    let fcs = fconstraint' [(a, "f1", c)]
    let cs = scs ++ fcs
    let t = fromAtomicPType b
    let (csRes, substRes) =  entailmentReduction t (cs, idSubst)
    assertEqual
        "Substitution" 
        (mkSubst [(a, b)])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (fconstraint' [(b, "f1", c)])
        csRes

entailmentProtectFieldUp = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let scs  = sconstraint' [(a,b)]
    let fcs = fconstraint' [(a, "f1", c)]
    let cs = scs ++ fcs
    let t = fromAtomicPType b
    let (csRes, substRes) =  entailmentReduction t (cs, idSubst)
    assertEqual
        "Substitution" 
        (mkSubst [])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (cs)
        csRes

entailmentNotProtectFieldUp = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let scs  = sconstraint' [(a,b)]
    let fcs = fconstraint' [(a, "f1", c), (b, "f1", c)]
    let cs = scs ++ fcs
    let t = fromAtomicPType b
    let (csRes, substRes) =  entailmentReduction t (cs, idSubst)
    assertEqual
        "Substitution" 
        (mkSubst [(a, b)])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (fconstraint' [(b, "f1", c)])
        csRes

--
-- Lazy instantiation
--

lazyNotProtectFieldDown = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let scs  = sconstraint' [(b,a)]
    let fcs = fconstraint' [(a, "f1", c), (b, "f2", d)]
    let cs = scs ++ fcs
    let t = fromAtomicPType a
    let (csRes, substRes) =  lazy t (cs, idSubst)
    assertEqual
        "Substitution" 
        (mkSubst [(a, b)])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (fconstraint' [(b, "f1", c), (b, "f2", d)])
        csRes

lazyProtectFieldUp = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let scs  = sconstraint' [(b,a)]
    let fcs = fconstraint' [(a, "f1", c), (b, "f2", d)]
    let cs = scs ++ fcs
    let t = fun a int 
    let (csRes, substRes) =  lazy t (cs, idSubst)
    assertEqual
        "Substitution" 
        (mkSubst [])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        cs
        csRes


--
-- Remove tautologies
--

subtypeTautologies = do
    let [fruit, apple, banana] = map record ["Fruit", "Apple", "Banana"]
    let tautologies  = sconstraint' [
          (int, double), (date, durationDate), 
          (duration, durationDate), (apple, fruit), 
          (apple, apple), (int, int)]
    let nonTautologies = sconstraint' [(double, int), (fruit, apple), (apple, banana)]
    let cs = tautologies ++ nonTautologies
    let (csRes, substRes) =  runM $ removeTautologies fruitEnv (cs, idSubst)
    assertEqual
        "Substitution" 
        (idSubst)
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (nonTautologies)
        csRes

fieldTautologies = do
    let tautologies = fconstraint' [
          (record "Fruit", "ripe", bool), (record "Apple", "weight", double)]
    let nonTautologies = fconstraint' [
          (record "Fruit", "ripe", int), (record "Fruit", "color", string), (record "Fruit", "noField", string), (record "NoRecord", "color", string), (record "Apple", "length", int)]
    let cs = tautologies ++ nonTautologies
    let (csRes, substRes) =  runM $ removeTautologies fruitEnv (cs, idSubst)
    assertEqual
        "Substitution" 
        (idSubst)
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (nonTautologies)
        csRes

ordTautologies = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let tautologies = ordconstraint [unit, int, double, char, date, duration, durationDate]
    let nonTautologies = ordconstraint [a, record "Fruit"]
    let cs = tautologies ++ nonTautologies
    let (csRes, substRes) =  runM $ removeTautologies fruitEnv (cs, idSubst)
    assertEqual
        "Substitution" 
        (idSubst)
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (nonTautologies)
        csRes

        
eqTautologies = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let tautologies = eqconstraint [unit, int, double, char, date, duration, durationDate, record "Fruit"]
    let nonTautologies = eqconstraint [a]
    let cs = tautologies ++ nonTautologies
    let (csRes, substRes) =  runM $ removeTautologies fruitEnv (cs, idSubst)
    assertEqual
        "Substitution" 
        (idSubst)
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (nonTautologies)
        csRes


--
-- Domain specific constraint elemination
--

domainSpecificElemination = do
    let [a, b, c, d, e, f, g, h, i, j, k] = 
          vars ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"]
    let tautologies  = sconstraint' [
          (a, bool), (b, bool), (c, int) , (double, d), 
          (e, date), (f, duration), (durationDate, g)]
    let nonTautologies = sconstraint' [(int, h), (j, double), (j, k)]
    let cs = tautologies ++ nonTautologies
    let (csRes, substRes) =  runM $ removeTautologies fruitEnv (cs, idSubst)
    assertEqual
        "Substitution" 
        (mkSubst [(a, bool),(b, bool),(c, int),(d, double),(e, date),(f, duration),(g, durationDate)]) 
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (nonTautologies)
        csRes

--
-- Merge field constraints
--


mergeFields1 = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let scs  = sconstraint' [(b,a)]
    let fcs = fconstraint' [(a, "f1", c), (b, "f1", d)]
    let cs = scs ++ fcs
    let (csRes, substRes) =  runM $ mergeFieldConstraints cs
    assertEqual
        "Substitution" 
        (mkSubst' [(d, c)])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (fconstraint' [(a, "f1", c)] ++ scs)
        csRes
        

mergeFields2 = do
    let [a, b, c, d, e, f] = vars ["a", "b", "c", "d", "e", "f"]
    let scs  = sconstraint' [(b,a),(d,c)]
    let fcs = fconstraint' [(a, "f1", c), (b, "f1", d)]
    let cs = scs ++ fcs
    let (csRes, substRes) =  runM $ mergeFieldConstraints cs
    assertEqual
        "Substitution" 
        (mkSubst' [(d, c)])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        (fconstraint' [(a, "f1", c)] ++ sconstraint' [(b,a),(c,c)])
        csRes

--
-- Satisfiability
--

satisfiabilityBaseCases = do
    assertBool "No constraints" 
      (satisfiable emptyEnv [])
    assertBool "a < a" 
      (satisfiable emptyEnv (sconstraint' [(a,a)]))
    assertBool "a < b" 
      (satisfiable emptyEnv (sconstraint' [(a,b)]))
    assertBool "a < Int" 
      (satisfiable emptyEnv (sconstraint' [(a,int)]))
    assertBool "Int < b" 
      (satisfiable emptyEnv (sconstraint' [(int,b)]))
    assertBool "Unit < b" 
      (satisfiable emptyEnv (sconstraint' [(unit,b)]))
    assertBool "Apple < b" 
      (satisfiable fruitEnv (sconstraint' [(apple,b)]))
    assertBool "Apple < Fruit" 
      (satisfiable fruitEnv (sconstraint' [(apple,fruit)]))
    assertBool "Apple < Food" 
      (satisfiable fruitEnv (sconstraint' [(apple,food)]))
    assertBool "Not: Apple < Banana" 
      (not $ satisfiable fruitEnv (sconstraint' [(apple,banana)]))
    assertBool "Apple.color: String" 
      (satisfiable fruitEnv (fconstraint' [(apple,"color", string)]))
    assertBool "Not: Apple.color: Int" 
      (not $ satisfiable fruitEnv (fconstraint' [(apple,"color", int)]))
    assertBool "Apple.ripe: Bool" 
      (satisfiable fruitEnv (fconstraint' [(apple,"ripe", bool)]))
    assertBool "Not: Apple.length: int" 
      (not $ satisfiable fruitEnv (fconstraint' [(apple,"length", int)]))
    assertBool "a.length: int" 
      (satisfiable fruitEnv (fconstraint' [(a,"length", int)]))
    assertBool "Not: a.length: bool" 
      (not $ satisfiable fruitEnv (fconstraint' [(a,"length", bool)]))
    assertBool "a.color: b" 
      (satisfiable fruitEnv (fconstraint' [(a,"color", b)]))
    assertBool "Not: a.noField: b" 
      (not $ satisfiable fruitEnv (fconstraint' [(a,"noField", b)]))

satisfiabilityCombined = do
    assertBool "a.color: String, b.ripe: Bool" 
      (satisfiable fruitEnv (fconstraint' [(a,"color", string), (b,"ripe", bool)]))
    
    assertBool "a.color: String, a < b, b.ripe: Bool" 
      (satisfiable fruitEnv (fconstraint' [(a,"color", string), (b,"ripe", bool)] ++
                            sconstraint' [(a,b)]))
    
    assertBool "Not: a.color: String, a.length: Int" 
      (not $ satisfiable fruitEnv (fconstraint' [(a,"color", string) 
                                                ,(a,"length", int)
                                                ]))
--
-- Combine SCC (remove cycles)
--


{-combineSCC1 = do
    let [a, b, c, d] = vars ["a", "b", "c", "d"]
    let cs  = sconstraint [(a,b), (b,a), (c, bool), (bool, c)]
    let (csRes, substRes) =  runM $ combineSccs cs
    assertEqual
        "Substitution" 
        (mkSubst [(a, b), (c, bool)])
        substRes
    assertEqualIgnoreOrder
        "Constraints" 
        []
        csRes
-}