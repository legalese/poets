{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
module Main where
import Control.Monad.Trans
import Poets.Rules.DPL2.Ast
import Poets.Rules.DPL2.Interpreter
import Poets.Rules.DPL2.Parser
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Environment
import System.Console.Haskeline
import Text.Parsec.Prim

data SearchType = DFS | BFS deriving (Data,Typeable,Show,Eq)

data Arguments = Arguments {filename :: String, 
                            query :: String, 
                            sStrat :: SearchType}
                 deriving (Show, Data, Typeable)

getSStrat (Arguments _ _ DFS) = dfs
getSStrat (Arguments _ _ BFS) = bfs

parseFromFile' p fname = do{ input <- readFile fname;
                             return (runParser p emptyPS fname input) }
parse' p input = runParser p emptyPS "<string>" input

cmlArgs = Arguments {filename = def &= argPos 0 &= typFile &= help "Name of file containing a dpl program",
                     query = def &= typ "QUERY" &= help "A query to fire against the program",
                     sStrat = enum [ DFS &= help "Perform depth first search",
                                     BFS &= help "Perform breadth first search"] 
                    } &= summary programName

programName = "dPL - demo v0.1, (C) Morten Ib Nielsen 2010"

main = do sysArgs <- getArgs
          if null sysArgs || (length sysArgs < 1 && head (head sysArgs) /= '-')
            then do let helpMsg = helpText HelpFormatDefault (cmdArgsMode cmlArgs)
                    putStrLn $ showText defaultWrap helpMsg
            else do args <- cmdArgs cmlArgs
                    putStrLn "*** dPL - demo ware not for distribution ***"
                    program <- parseFromFile' programParser $ filename args
                    case program of
                      Left errMsg -> print errMsg
                      Right prog ->
                          case query args /= "" of
                            True -> do processQueryStr (getSStrat args) prog $ query args
                                       runPrompt (getSStrat args) prog
                            False -> runPrompt (getSStrat args) prog

--processQueryStr :: SearchStrategy u a -> Program -> String -> IO ()
processQueryStr sstrat prog queryStr =
  case parse' queryParser queryStr of
    Left errMsg -> print errMsg
    Right sq@(StrictQ _) ->
      print $ runInterpreter sstrat prog sq
    Right dq@(DefeasibleQ _) ->
        print $ runInterpreter sstrat prog dq
    Right (BothQ query) ->
      do putStrLn "Strict solution:"
         print $ runInterpreter sstrat prog (StrictQ query)
         putStrLn "Defeasible solution:"
         print $ runInterpreter sstrat prog (DefeasibleQ query)

--runPrompt :: SearchStrategy -> Program -> IO ()
runPrompt sStrat prog = runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
      maybeLine <- getInputLine "% "
      case maybeLine of
        Nothing -> outputStrLn "exiting..." -- EOF / control-d
        Just ":q" -> outputStrLn "exiting..."
        Just line -> do lift $ processQueryStr sStrat prog line
                        loop