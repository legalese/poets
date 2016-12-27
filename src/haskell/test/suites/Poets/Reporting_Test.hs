--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data_Test
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Patrick Bahr, Michael Werk, Joakim Ahnfelt, Jon Elverkilde
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the reporting module.
--
--------------------------------------------------------------------------------

module Poets.Reporting_Test (tests) where

import Test.Framework
import qualified Poets.Reporting.Language_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

tests = testGroup "Poets.Reporting" [
         Poets.Reporting.Language_Test.tests
        ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
