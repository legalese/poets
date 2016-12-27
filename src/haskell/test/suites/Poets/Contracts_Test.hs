--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts_Test
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the contract engine.
--
--------------------------------------------------------------------------------

module Poets.Contracts_Test (tests) where

import qualified Poets.Contracts.Language_Test as Language_Test
import Test.Framework

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

tests = testGroup "Poets.Contracts" [
        Language_Test.tests
        ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
