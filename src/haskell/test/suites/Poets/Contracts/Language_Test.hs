--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language_Test
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the contract module.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language_Test (tests) where

import Test.Framework
import qualified Poets.Contracts.Language.CSL_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

tests = testGroup "Poets.Contracts.Language" [
         Poets.Contracts.Language.CSL_Test.tests
        ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
