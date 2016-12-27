--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  3gERP, 2011
-- License     :  AllRightsReserved
-- Maintainer  :  
-- Stability   :  unknown
-- Portability :  unknown
--
-- TODO
--
--------------------------------------------------------------------------------
module Main where
import System.IO
import System.Environment
import Network
import PoetsServer_Client
import Thrift.Protocol.Binary
import Thrift.Transport.Framed
import Thrift.Transport.Handle ()

import PServer.Thrift.Decode
import Value_Types hiding (Record)

import Poets.Data.Value.Algebras.Render ()

type BinaryConnection = (BinaryProtocol Handle, BinaryProtocol Handle)
type Service = (String, String, BinaryConnection -> String -> IO Bool)

main = do
  args <- getArgs
  case args of
    [repName] -> runReport repName
    _         -> putStrLn "usage: poetsquery REPORT"

runReport rname = do
  h' <- connectTo "localhost" (PortNumber 7911)
  h <- openFramedTransport h' 
  let conn = (BinaryProtocol h, BinaryProtocol h)
  success <- queryReport conn rname [] []
  val <- valueDecode success
  print val

valueDecode Value{f_Value_values = Just valMap,
                  f_Value_root = Just idx} =
    either error return $ decodeVal valMap idx
valueDecode Value{f_Value_values = _,
                  f_Value_root = _} = error "valueDecode"
