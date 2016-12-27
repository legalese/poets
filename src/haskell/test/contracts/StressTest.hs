--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  
-- Stability   :  unknown
-- Portability :  unknown
--
-- TODO
--
--------------------------------------------------------------------------------

module Main where

import System.Environment
import Network
import Thrift.Transport.Handle
import Thrift.Protocol.Binary
import Value_Types hiding (Value)
import Contracts_Types
import qualified Value_Types as VTypes
import PoetsServer_Client as Client
import PServer.Thrift.Encode
import Poets.Data
import Data.DateTime

-- Encode a POETS value in the Thrift representation
valueEncode :: POETSValue -> VTypes.Value
valueEncode v = let (idx,valMap) = encodeTerm v in
                VTypes.Value{f_Value_values = Just valMap,
                             f_Value_root = Just idx}

timeStamp = fromGregorian 2010 12 01 12 0 0

instantiateClient = do
  handle <- hOpen("127.0.0.1", PortNumber 7911)
  Client.instantiateContract (BinaryProtocol handle,BinaryProtocol handle) m
  where m = valueEncode $ iVRecord $
            VR{vrecordName = "DebugContract",
               vrecordFields =
                   newFields [VF{vfieldName = "startDate",
                                 vfieldValue = iVDateTime timeStamp},
                              VF{vfieldName = "templateName",
                                 vfieldValue = inject $ VString "DebugContract"}]}

registerClient cId = do
  handle <- hOpen("127.0.0.1", PortNumber 7911)
  Client.registerTransactions (BinaryProtocol handle,BinaryProtocol handle) [tr]
  where tr = Transaction{f_Transaction_contractId = Just cId,
                         f_Transaction_timeStamp = Just $ encodeDateTime timeStamp,
                         f_Transaction_transactionData = Just tr'}
        tr' = valueEncode $ iVRecord $
              VR{vrecordName = "DebugTransaction",
                 vrecordFields = newFields []}

loop 0 = return ()
loop n = if n <= 0 then
             return () 
         else do
           cId <- instantiateClient
           registerClient cId
           loop $ n - 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    [count] -> do
        loop $ read count
    _ ->
        putStrLn "Syntax: contractsss <number of iterations>"