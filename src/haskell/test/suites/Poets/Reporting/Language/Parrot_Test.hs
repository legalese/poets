--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data_Test
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Patrick Bahr, Michael Werk, Joakim Ahnfelt, Jon Elverkilde
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the parrot module.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot_Test (tests) where

import Test.Framework
import qualified Poets.Reporting.Language.Parrot.Evaluate_Test
import qualified Poets.Reporting.Language.Parrot.Typing.Simplify_Test

--------------------------------------------------------------------------------
-- Test Suits
--------------------------------------------------------------------------------

tests = testGroup "Poets.Reporting.Language.Parrot" [
         Poets.Reporting.Language.Parrot.Evaluate_Test.tests,
         Poets.Reporting.Language.Parrot.Typing.Simplify_Test.tests
        ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------
