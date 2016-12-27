{-# LANGUAGE ScopedTypeVariables, TypeOperators, TypeSynonymInstances,
  FlexibleInstances, MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Evaluator_Test
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Martin Dybdal
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the CSL expression evaluator:
--   Poets.Contracts.Language.CSL.Algebras.Exp.Evaluator
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Evaluator_Test (tests) where

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)
import Data.Comp
import Data.Maybe (fromJust)
import Data.Either.Utils (fromRight)
import Poets.Data.Value hiding (Value)
import Poets.Data.Value.Utils
import Poets.Data.Render
import Poets.Data.Type hiding (Type)
import Poets.Contracts.Language.CSL
import Poets.Contracts.Language.CSL.BuiltIn
import Poets.Contracts.Language.CSL.Desugar
import Poets.Contracts.Language.CSL.Interpreter
import Poets.Contracts.Language.CSL.Typing.Inference
import qualified Poets.Contracts.Language.CSL.Parser as CSLParser
import qualified Data.Map as Map
import Control.Monad

-- | Tests different aspects of the CSL expression evaluator
tests = testGroup "Poets.Contracts.Language.CSL.Evaluator" [testEvalExpressions]

testEval :: SubTypeRelation -> String -> String -> Value -> Test
testEval rel name exp val = testCase name $
  case fmap (stripA . desugarExpr) $ CSLParser.parseExpr "HUnit test" exp of
    Left e ->
        assertFailure $ show e
    Right e' ->
        case evalExpr rel [] pre e' of
                Left e  -> assertFailure $ show e
                Right v -> unless (val == v) (assertFailure (msg val v))
    where msg val v = "expected: " ++ renderVal val ++
                      "\n but got: " ++ renderVal v
          pre = Predefined{preVals = const $ Map.fromList builtInVals,
                           preExps = Map.empty}
          builtInVals = map (\(n, _ :: (Type, [TypeInfConstraints Type]), v) -> (n, v)) builtIn
          renderVal :: Value -> String
          renderVal v = case deepProject2 v of
                          Just (x :: Term (Val :+: VUnit)) ->
                              show (renderTerm x)
                          _ ->
                              "<function>"

testEvalExpressions = testGroup "Evaluation of expressions"
  [testEval' "Record projection" "R{x = 32}.x" (iVInt 32),
   testEval' "Record update" "R{x = 32}{x = 44}"
             (iVRecord $ VR "R" (fromRight $ newFields' [VF "x" (iVInt 44)])),
   testEval' "Function application" "(\\x -> x) 32" (iVInt 32),
   testEval' "Unary minus" "- (- (-32))" (iVInt (-32)),
   testEval' "Negation" "not false" (iVBool True),
   testEval' "Addition" "3 + 3" (iVInt 6),
   testEval' "Subtraction" "3 - 5" (iVInt (-2)),
   testEval' "Multiplication" "3 * 3" (iVInt 9),
   testEval' "Division" "7 / 2" (iVInt 3),
   testEval' "Duration addition" "2H <+> 4D"
             (iVDuration $ fromSeconds $ (4 * 24 + 2) * 60 * 60),
   testEval' "Duration subtraction" "4D <-> 2H"
             (iVDuration $ fromSeconds $ (4 * 24 - 2) * 60 * 60),
   testEval' "Duration multiplication" "3 <*> 4D"
             (iVDuration $ fromSeconds $  3 * 4 * 24 * 60 * 60),
   testEval' "List concatenation" "2 # 3 # []" (iVList [iVInt 2, iVInt 3]),
   testEval' "Equality" "2 == 3" (iVBool False),
   testEval' "Inequality" "2 /= 3" (iVBool True),
   testEval' "<=" "2 <= 3" (iVBool True),
   testEval' ">=" "2 >= 3" (iVBool False),
   testEval' "<" "2 < 3" (iVBool True),
   testEval' ">" "2 > 3" (iVBool False),
   testEval' "Conjunction" "false && true" (iVBool False),
   testEval' "Disjunction" "false || true" (iVBool True),
   testEval' "DateTime projection, year" "<<2012-01-24 14:50:33>>.year"
             (iVInt 2012),
   testEval' "DateTime projection, month" "<<2012-01-24 14:50:33>>.month"
             (iVInt 1),
   testEval' "DateTime projection, day" "<<2012-01-24 14:50:33>>.day"
             (iVInt 24),
   testEval' "DateTime projection, hour" "<<2012-01-24 14:50:33>>.hour"
             (iVInt 14),
   testEval' "DateTime projection, minute" "<<2012-01-24 14:50:33>>.minute"
             (iVInt 50),
   testEval' "DateTime projection, second" "<<2012-01-24 14:50:33>>.second"
             (iVInt 33),
   testEval' "DateTime projection, microsecond"
             "<<2012-01-24 14:50:33.123000>>.microsecond"
             (iVInt 123000),
   testEval' "DateTime update, year" "<<2012-01-24 14:50:33>>{year = 4000}"
             (iVDateTime $ fromJust $ createDateTime' 4000 1 24 14 50 33 0),
   testEval' "DateTime update, month" "<<2012-01-24 14:50:33>>{month = 12}"
             (iVDateTime $ fromJust $ createDateTime' 2012 12 24 14 50 33 0),
   testEval' "DateTime update, day" "<<2012-01-24 14:50:33>>{day = 3}"
             (iVDateTime $ fromJust $ createDateTime' 2012 1 3 14 50 33 0),
   testEval' "DateTime update, hour" "<<2012-01-24 14:50:33>>{hour = 12}"
             (iVDateTime $ fromJust $ createDateTime' 2012 1 24 12 50 33 0),
   testEval' "DateTime update, minute" "<<2012-01-24 14:50:33>>{minute = 40}"
             (iVDateTime $ fromJust $ createDateTime' 2012 1 24 14 40 33 0),
   testEval' "DateTime update, second" "<<2012-01-24 14:50:33>>{second = 59}"
             (iVDateTime $ fromJust $ createDateTime' 2012 1 24 14 50 59 0),
   testEval' "DateTime update, microsecond"
             "<<2012-01-24 14:50:33>>{microsecond = 4000}"
             (iVDateTime $ fromJust $ createDateTime' 2012 1 24 14 50 33 4000),
   testEval' "If-then-else" "if 2 == 3 then 42 else 6" (iVInt 6),
   testEval rel1 "Case expression, 1"
                 "case A{x = 32, y = 42} of B r -> r.x | C r -> r.y" (iVInt 32),
   testEval rel2 "Case expression, 2"
                 "case A{x = 32, y = 42} of B r -> r.x | C r -> r.y" (iVInt 42),
   testEval' "Let expression" "let x = 32 in x + x" (iVInt 64),
   testEval' "Built-in function, foldr"
             ("let app = \\l1 l2 -> foldr (\\e a -> e # a) l2 l1 in " ++
              "let reverse = foldr (\\e a -> app a [e]) [] in " ++
              "reverse [1,2,3,4,5,6,7,8,9,10,11]")
             (iVList $ map iVInt [11,10,9,8,7,6,5,4,3,2,1]),
   testEval' "Built-in function, ceil" "ceil 32.0001" (iVInt 33),
   testEval' "Built-in function, subtractDate"
             "subtractDate <<2012-01-25 14:50:33>> <<2012-01-24 10:30:33>>"
             (iVDuration $ fromSeconds $  ((1 * 24 + 4) * 60 + 20) * 60)]
      where rel = subTypeRel $ newRecordEnv []
            rel1 r1 r2 = r1 == "A" && r2 == "B"
            rel2 r1 r2 = r1 == "A" && r2 == "C"
            testEval' = testEval rel