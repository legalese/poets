{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.ReportInitException
-- Copyright   : 3gERP, 2009
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the error data type which indicates exceptions
-- during the initialisation of report specifications.
--
--------------------------------------------------------------------------------
module Poets.Reporting.ReportInitException
    ( module Control.Monad.Error,
      liftReportException,
      ReportInitException (..)) where


import Poets.EventLog
import Poets.Reporting.Language.Parrot.Compiler.Haskell (CompileException)
import Poets.Reporting.Language.Parrot.Typing.TypingMonad (TypingErr)

import GHC
import Bag
import Outputable
import ErrUtils
import HscTypes
import Text.Parsec.Error


import Control.Monad.Error hiding (liftIO)

data ReportInitException = ReportInitException String
                         | ReportTypeException TypingErr
                         | ReportCompileException CompileException
                         | ReportParseException ParseError
                         | ReportGhcException GhcException
                         | ReportHaskellException SourceError
                         | ReportLogError LogError

liftReportException :: (LiftReportException e) => Either e a -> Either ReportInitException a
liftReportException (Left e) = Left $ toReportException e
liftReportException (Right v) = Right v

class LiftReportException e where
    toReportException :: e -> ReportInitException

instance LiftReportException String where
    toReportException  = ReportInitException

instance LiftReportException TypingErr where
    toReportException = ReportTypeException

instance LiftReportException ParseError where
    toReportException = ReportParseException

instance LiftReportException GhcException where
    toReportException = ReportGhcException

instance LiftReportException CompileException where
    toReportException = ReportCompileException

instance LiftReportException SourceError where
    toReportException = ReportHaskellException

instance Show ReportInitException where
    show (ReportInitException msg) = msg
    show (ReportTypeException err) = show err
    show (ReportParseException err) = show err
    show (ReportCompileException err) = show err
    show (ReportGhcException ge) = show ge
    show (ReportHaskellException se) = show se
    show (ReportLogError le) = show le

instance Error ReportInitException where
    strMsg = ReportInitException