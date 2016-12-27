{-# LANGUAGE ScopedTypeVariables #-}
module Poets.Data.Value_Test (tests, main) where

import Poets.Data.Value_Gen
import Poets.Data.Value
import Test.HUnit
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Data.Comp
import qualified Data.Map as Map
import Poets.Data.Type
import Poets.Data.Value.Render ()
import Poets.Data.Value.TypeChecker




---------------------------------------------------
----------------- Test Suits ----------------------
---------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Poets.Data.Value" [
         testProperty "typeCheckTerm" prop_typeCheckTerm,
         testCase "Equality for Term lists account for list length" equalityForTermLists
        ]

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

equalityForTermLists = assert $
    let l1 = (Term $ VList [Term $ VBool True]) :: Value in
    let l2 = (Term $ VList [Term $ VBool True, Term $ VBool False]) :: Value in
    l1 /= l2

---------------------------------------------------
----------------- Properties ----------------------
---------------------------------------------------

prop_typeCheckTerm :: POETSRecordEnv -> Property
prop_typeCheckTerm env
    = forAllTypes env (\ty -> forAllTerm env ty (\val -> 
               -- TODO: hard coded to empty entity typing environment for now
               case typeCheckTerm ty env (Map.empty :: EntityTypingEnv) (val :: Value) of 
                 Left _ -> True
                 Right () -> True))