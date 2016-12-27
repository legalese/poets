--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  3gERP, 2011
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- A small utility to upload a contract definition to a running POETS server.
--
--------------------------------------------------------------------------------

module Main where

import System.Environment
import Network
import Thrift.Transport.Handle
import Thrift.Transport.Framed
import Thrift.Protocol.Binary
import PoetsServer_Client as Client

getBinPro ip = do
  handle <- hOpen(ip, PortNumber 7911)
  framed <- openFramedTransport handle
  return $ BinaryProtocol framed

add ip path = do
  cont <- readFile path
  binpro <- getBinPro ip
  Client.createContractTemplate (binpro, binpro) cont

update ip path = do
  cont <- readFile path
  binpro <- getBinPro ip
  Client.updateContractTemplate (binpro, binpro) cont

delete ip name = do
  binpro <- getBinPro
  Client.deleteContractTemplate (binpro, binpro) name

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--add", filePath] ->
        add "127.0.0.1" filePath
    ["--add", filePath, ip] ->
        add ip filePath
    ["--update", filePath] ->
        update "127.0.0.1" filePath
    ["--update", filePath, ip] ->
        update ip filePath
    ["--delete", name] ->
        delete "127.0.0.1" name
    ["--delete", name, ip] ->
        delete ip name
    _ ->
        putStrLn "Syntax: upload (--add|--update|--delete) [<file>] <name> [<type>] [<ip>]"