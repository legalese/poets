--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.ReportLibrary
-- Copyright   : 3gERP, 2009
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides data strutures to represent libraries of
-- report functions.
--
--------------------------------------------------------------------------------
module Poets.Reporting.Language.Parrot.ReportLibrary
    ( ReportLibrary (..)
     ) where


import Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.PolyType

import Data.Map
import Data.Set

data ReportLibrary = ReportLibrary {
      libRecords :: [ExtRecord],
      libTypings :: Map VVarId PolyType,
      libFunctions :: Set FunId,
      libModuleName :: String,
      libDir :: FilePath}