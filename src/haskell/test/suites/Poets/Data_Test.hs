--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data_Test
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Morten Ib Nielsen, Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the data module.
--
--------------------------------------------------------------------------------

module Poets.Data_Test (tests) where

import Test.Framework
import qualified Poets.Data.Value_Test
import qualified Poets.Data.Type_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

tests = testGroup "Poets.Data" [
         Poets.Data.Value_Test.tests,
         Poets.Data.Type_Test.tests
        ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------


