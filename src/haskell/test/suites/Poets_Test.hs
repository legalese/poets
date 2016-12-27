--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Morten Ib Nielsen, Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the POETS server.
--
--------------------------------------------------------------------------------

module Main where

import Test.Framework
import qualified Poets.Data_Test
import qualified Poets.Reporting_Test
import qualified Poets.Contracts_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

main = defaultMain [tests]

tests = testGroup "Poets" [
         Poets.Data_Test.tests,
         Poets.Reporting_Test.tests,
         Poets.Contracts_Test.tests
       ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
