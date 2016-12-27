--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL_Test
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the CSL module.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL_Test (tests) where

import Test.Framework
import qualified Poets.Contracts.Language.CSL.Parser_Test
import qualified Poets.Contracts.Language.CSL.Evaluator_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

tests = testGroup "Poets.Contracts.Language.CSL" [
         Poets.Contracts.Language.CSL.Parser_Test.tests,
         Poets.Contracts.Language.CSL.Evaluator_Test.tests
        ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
