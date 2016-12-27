{-# LANGUAGE FlexibleContexts, TypeOperators, TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Aux
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Philip Carlsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides auxilary test functions.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Aux
    (
     assertEither
    ) where

import Prelude hiding (EQ)
import Test.HUnit hiding (Test, test)

--------------------
-- Aux functions  --
--------------------

-- |Assert Right for an Either with a showable Left
instance (Show a) => Assertable (Either a b) where
  assert (Left errMsg) = assertFailure $ show errMsg
  assert _             = return ()


-- | Make assertion on Either value, failing on all Left values
assertEither :: (Show a) => (b -> Assertion) -> Either a b -> Assertion
assertEither assertRight either =
  case either of
    Left msg -> assertFailure $ show msg
    Right val -> assertRight val

