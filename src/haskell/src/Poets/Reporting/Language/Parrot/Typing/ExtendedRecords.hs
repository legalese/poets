
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.ExtendedRecords
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the extended records used in Parrot.
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.ExtendedRecords where


import Poets.Reporting.Language.Parrot.Syntax

type ExtRecEnv = RecordEnv PType
type ExtFieldEnv = FieldEnv PType
type ExtRecord = Record PType