{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Parser_Test
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the CSL Parser.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Parser_Test (tests) where

import Poets.Contracts.Language.CSL
import Poets.Contracts.Language.CSL.AST
import Poets.Contracts.Language.CSL.Render (clauseDefToDoc, functionDefToDoc)
import qualified Poets.Contracts.Language.CSL.Parser as CSLParser
import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)
import Poets.Data
import Poets.Contracts.Language.CSL.Aux
import Data.Maybe (fromJust)
import Data.Either.Utils (fromRight)

tests = testGroup "Poets.Contracts.Language.CSL.Parser"
        [testParseExpressions,
         testParseClauses,
         testParseTopLevelDefs,
         testParseFile]

--------------------------------------------------------------------------------
-- Expressions Parser
--------------------------------------------------------------------------------

type Expr = Term (Val :+: VUnit :+: CoreExp :+: SugExp)

testExpr :: String -> String -> Expr -> Test
testExpr name e e' =
    testCase ("Parsing of expression, " ++ name) $
    assertEither (assertEqual "" e') $
    fmap stripA $
    CSLParser.parseExpr "HUnit test" e

testParseExpressions = testGroup "Parsing of expressions"
  [testExpr "Boolean (true)" "true" (iVBool True),
   testExpr "Boolean (false)" "false" (iVBool False),
   testExpr "Unit" "()" iVUnit,
   testExpr "Integer" "42" (iVInt 42),
   testExpr "Real" "42.43" (iVReal 42.43),
   testExpr "String" "\"string\"" (iVString "string"),
   testExpr "Date" "<<2012-01-24>>" (iVDate $ fromJust $ createDate 2012 1 24),
   testExpr "Time" "<<14:50:33>>" (iVTime $ fromJust $ createTime 14 50 33 0),
   testExpr "Time (with microseconds)" "<<14:50:33.123456>>"
            (iVTime $ fromJust $ createTime 14 50 33 123456),
   testExpr "DateTime" "<<2012-01-24 14:50:33>>"
            (iVDateTime $ fromJust $ createDateTime' 2012 1 24 14 50 33 0),
   testExpr "Duration" "1W" (iVDuration $ fromSeconds $ 7 * 24 * 60 * 60),
   testExpr "Record" "Foo" (iVRecord $ VR "Foo" (fromRight $ newFields' [])),
   testExpr "Record (with fields)" "Foo{x = 32}"
            (iVRecord $ VR "Foo" (fromRight $ newFields' [VF "x" (iVInt 32)])),
   testExpr "Entity" "Foo<32>" (iVEnt $ VEntity "Foo" 32 Nothing),
   testExpr "Lambda abstraction" "\\x y -> x"
            (iESLambda ["x", "y"] (iEVar "x")),
   testExpr "If-then-else" "if 32 then 30 else 21"
            (iEIfThenElse (iVInt 32) (iVInt 30) (iVInt 21)),
   testExpr "Case" "case 32 : Baz of Foo x -> 30 | Bar y -> 40"
            (iECase (iVInt 32) (Just "Baz") [CaseExp "Foo" "x" (iVInt 30),
                                             CaseExp "Bar" "y" (iVInt 40)]),
   testExpr "Let" "let x = 32 in x" (iESLet "x" (iVInt 32) (iEVar "x")),
   testExpr "List (empty)" "[]" (iVList []),
   testExpr "List (non-empty)" "[32,x,y]"
            (iVList [iVInt 32, iEVar "x", iEVar "y"]),
   testExpr "Variable" "abc" (iEVar "abc"),
   testExpr "Field projection" "abc.f" (iEProj "f" (iEVar "abc")),
   testExpr "Field updates" "abc{f = 32, y = true}"
            (iESUpdate (iEVar "abc") [("f", iVInt 32), ("y", iVBool True)]),
   testExpr "Application" "true x" (iVBool True `iEApply` iEVar "x"),
   testExpr "Unary operator" "- x" (iESUnOp UMINUS (iEVar "x")),
   testExpr "Binary operator" "x * 32" (iEBinOp TIMES (iEVar "x") (iVInt 32)),
   testExpr "Grouping" "((x + y) * (- 32)) z"
            (iEApply (iEBinOp TIMES (iEBinOp PLUS (iEVar "x") (iEVar "y"))
                                    (iESUnOp UMINUS (iVInt 32))) (iEVar "z"))]


--------------------------------------------------------------------------------
-- Clauses Parser
--------------------------------------------------------------------------------

type Clause = Term (CoreClause :+: SugClause :+: Val :+: VUnit :+: CoreExp :+: SugExp)

testClause :: String -> String -> Clause -> Test
testClause name c c' =
    testCase ("Parsing of clause, " ++ name) $
    assertEither (assertEqual "" c') $
    fmap stripA $
    CSLParser.parseClause "HUnit test" c

testParseClauses = testGroup "Parsing of clauses"
  [testClause "Fulfilment" "fulfilment" iFulfilment,
   testClause "Conjunction" "fulfilment and fulfilment"
              (iFulfilment `iAnd` iFulfilment),
   testClause "Disjunction" "fulfilment or fulfilment"
              (iFulfilment `iOr` iFulfilment),
   testClause "Grouping" "(fulfilment or fulfilment) and (fulfilment and fulfilment)"
              ((iFulfilment `iOr` iFulfilment) `iAnd` (iFulfilment `iAnd` iFulfilment)),
   testClause "Obligation 1" "<x> Foo(f1 x1, f2 x2) where 32 due within true after false remaining r then fulfilment"
              (iObligationSug (iEVar "x") "Foo" [("x1", "f1"), ("x2", "f2")]
                              (Just "r") (Just (iVInt 32))
                              (Within (iVBool True) (Just $ iVBool False))
                              (Just iFulfilment)),
   testClause "Obligation 2" "<x> Foo(f1 x1, f2 x2) due within true after false remaining r then fulfilment"
              (iObligationSug (iEVar "x") "Foo" [("x1", "f1"), ("x2", "f2")]
                              (Just "r") Nothing
                              (Within (iVBool True) (Just $ iVBool False))
                              (Just iFulfilment)),
   testClause "Obligation 3" "<x> Foo(f1 x1, f2 x2) due within true remaining r then fulfilment"
              (iObligationSug (iEVar "x") "Foo" [("x1", "f1"), ("x2", "f2")]
                              (Just "r") Nothing
                              (Within (iVBool True) Nothing)
                              (Just iFulfilment)),
   testClause "Obligation 4" "<x> Foo(f1 x1, f2 x2) due immediately remaining r then fulfilment"
              (iObligationSug (iEVar "x") "Foo" [("x1", "f1"), ("x2", "f2")]
                              (Just "r") Nothing
                              (Immediately Nothing)
                              (Just iFulfilment)),
   testClause "Obligation 5" "<x> Foo(f1 x1, f2 x2) due immediately then fulfilment"
              (iObligationSug (iEVar "x") "Foo" [("x1", "f1"), ("x2", "f2")]
                              Nothing Nothing
                              (Immediately Nothing)
                              (Just iFulfilment)),
   testClause "Obligation 6" "<x> Foo(f1 x1, f2 x2) due immediately"
              (iObligationSug (iEVar "x") "Foo" [("x1", "f1"), ("x2", "f2")]
                              Nothing Nothing
                              (Immediately Nothing)
                              Nothing),
   testClause "External choice" "when Foo(f1 x1, f2 x2) where 32 due within true after false remaining r then fulfilment else fulfilment"
              (iExternalChoiceSug "Foo" [("x1", "f1"), ("x2", "f2")]
                                  (Just "r") (Just (iVInt 32))
                                  (Within (iVBool True) (Just $ iVBool False))
                                  (Just iFulfilment) (Just iFulfilment)),
   testClause "Internal choice" "if 2 + 2 then fulfilment else fulfilment"
              (iInternalChoice (iEBinOp PLUS (iVInt 2) (iVInt 2))
                               iFulfilment iFulfilment),
   testClause "Internal choice" "if 2 + 2 then fulfilment else fulfilment"
              (iInternalChoice (iEBinOp PLUS (iVInt 2) (iVInt 2))
                               iFulfilment iFulfilment),
   testClause "Case" "case 2 of Foo x -> fulfilment | Bar y -> fulfilment"
              (iCase (iVInt 2) Nothing [ClauseCase "Foo" "x" iFulfilment,
                                        ClauseCase "Bar" "y" iFulfilment]),
   testClause "Case w/ type ann." "case 2 : FooBar of Foo x -> fulfilment | Bar y -> fulfilment"
              (iCase (iVInt 2) (Just "FooBar") [ClauseCase "Foo" "x" iFulfilment,
                                                ClauseCase "Bar" "y" iFulfilment]),
   testClause "Template instantiation" "foo(1, 2, x)<a, b>"
              (iInstantiate "foo" [iVInt 1, iVInt 2, iEVar "x"]
                                  [iEVar "a", iEVar "b"])
  ]


--------------------------------------------------------------------------------
-- Top-level definitions
--------------------------------------------------------------------------------

-- Used for testing only
instance Show (ClauseDef (CoreClause :+: SugClause :+: Val :+: VUnit :+: CoreExp :+: SugExp)) where
    show = show . clauseDefToDoc

instance Show (FunctionDef (Val :+: VUnit :+: CoreExp :+: SugExp)) where
    show = show . functionDefToDoc


testClauseDef :: String -> ClauseDef (CoreClause :+: SugClause :+: Val :+: VUnit :+: CoreExp :+: SugExp) -> Test
testClauseDef c c' =
    testCase ("Parsing of clause definition") $
    assertEither (assertEqual "" c') $
    fmap (\cdef -> cdef{clauseDefBody = stripA $ clauseDefBody cdef}) $
    CSLParser.parseClauseDef "HUnit: test" c

testFunDef :: String -> String -> FunctionDef (Val :+: VUnit :+: CoreExp :+: SugExp) -> Test
testFunDef name c c' =
    testCase ("Parsing of function definition, " ++ name) $
    assertEither (assertEqual "" c') $
    fmap (\fdef -> fdef{functionExp = stripA $ functionExp fdef}) $
    CSLParser.parseFunction "HUnit: test" c

testParseTopLevelDefs = testGroup "Parsing of top-level definitions"
  [testClauseDef "clause foo(x : Int)<y : Agent> = fulfilment"
                 (ClauseDefinition "foo" [("x", iTInt)]
                                   [("y", iTRecord "Agent")] iFulfilment),
   testFunDef "simple value" "val x = 32" (FunctionDefinition "x" (iVInt 32)),
   testFunDef "proper function" "fun f a b = 32"
              (FunctionDefinition "f" (iELambda "a" $ iELambda "b" $ iVInt 32))
  ]


--------------------------------------------------------------------------------
-- Parse full file
--------------------------------------------------------------------------------

testParseFile :: Test
testParseFile =
    testCase "Test parsing of sale.csl" $ do
      cont <- readFile "test/contracts/sale.csl"
      let res = CSLParser.parseContract "test/contracts/sale.csl" cont
      case res of
        Left msg ->
            assertFailure $ "Parsing failed with error: " ++ show msg
        Right _ ->
            return ()