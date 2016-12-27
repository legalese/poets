{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Report
-- Copyright   : 3gERP, 2009
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the types to represent reports.
--
--------------------------------------------------------------------------------
module Poets.Reporting.Report
    ( module Poets.Reporting.ReportMonad,
      ReportName,
      ReportModule (..),
      ReportType (..),
      ReportId,
      ReportSpec,
      ReportFunction,
      hasTags,
      hasNoTags,
      runReportFunction
    ) where

import Poets.Reporting.ReportMonad
import Poets.Data
import Poets.EventLog
import Poets.EntityStore

import Data.Set (Set)
import qualified Data.Set as Set



type ReportId = Int

{-|
  This type represents the name of a report
-}
type ReportName = String

{-| The type of a report function. The first argument is the list of
arguments for the report.  -}

type ReportFunction = [Value] -> RM Val Value

{-|
  This type represents the type of a report
-}
data ReportType = ReportType {reportArgTypes :: [Type], reportRetType :: Type}

data ReportModule = ReportModule{ reportType :: ReportType,
                                  reportFunction :: ReportFunction,
                                  reportName :: ReportName,
                                  reportDesc :: String,
                                  reportTags :: Set String,
                                  reportSpec :: ReportSpec,
                                  reportSubType :: RecordName -> RecordName -> Bool,
                                  reportId :: ReportId}


-- | This function runs the report function from the given report
-- module in the context of the provided event log and entity store.
runReportFunction  :: ReportModule -> [Event] -> EntityStore
                   -> [Value] -> Either ReportError Value
runReportFunction ReportModule {reportFunction = repFun, reportSubType = isSub} events  estore args 
    = runRM (repFun args) events entityTable isSub
      where entityTable id dt = either (Left . show) Right
                                       (latestEntityBefore estore id dt)

-- | This function checks whether the given report module has all of
-- the given tags.
hasTags :: ReportModule -> Set String -> Bool
hasTags ReportModule{reportTags = rTags} tags = tags `Set.isSubsetOf` rTags

-- | This function checks whether the given report module has none of
-- the given tags.
hasNoTags :: ReportModule -> Set String -> Bool
hasNoTags ReportModule{reportTags = rTags} tags = Set.null $ tags `Set.intersection` rTags


{-|
  This type represents a report specification. For now this means
  a string that contains a well-formed Haskell program.

  TODO define a proper report specification type
-}
type ReportSpec = String
