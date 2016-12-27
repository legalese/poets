{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
module Main where
import Control.Monad
import Control.Monad.Error
--import Data.Either

import Data.List
--import Data.Maybe
--import qualified Data.Set as SET

import Poets.Data
import qualified Poets.Rules.DPL.Ast as DPL
import qualified Poets.Rules.LRL.Ast as AST
import Poets.Rules.LRL.Compiler
import Poets.Rules.LRL.Parser
import Poets.Rules.LRL.Typing
--import Poets.Rules.PoetsIntegration.PoetsIntegration
import System.Console.CmdArgs
--import System.Console.Haskeline
import System.Directory
import System.Environment
--import System.IO
import Text.Parsec

data Arguments = Arguments {docsDir :: String, ontology :: String } --, perLine :: Int }
                 deriving (Show, Data, Typeable)

cmlArgs = mode $ Arguments {docsDir = def &= argPos 0 & typ "INPUT_DIR" & text "Name of directory containing documents",
                            ontology = def &= argPos 1 & typ "INPUT_FILE" & text "Name of file containing ontology"}
--                            perLine = 4 &= typ "INTEGER" & text "The number of pictures per line"

programName = "Legal Rule Language -v0, (C) Morten Ib Nielsen 2010"
errExit = "Errors were found. Exiting..."

main = do sysArgs <- getArgs
          if null sysArgs || (length sysArgs < 2 && head (head sysArgs) /= '-')
            then do helpMsg <- cmdArgsHelp programName [cmlArgs] Text
                    putStrLn helpMsg
            else do args <- cmdArgs programName [cmlArgs]
                    let inputdir = docsDir args
                    let ontologyfile = ontology args
                    putStrLn $ "Input dir: " ++ inputdir
                    putStr "Parsing... "
                    docs <- applyToFilesInDir inputdir processFile
                    numDocs <- numFiles inputdir
                    if (length docs) < numDocs
                      then putStrLn errExit

                      else do putStrLn "Done."
                              putStrLn $ "Ontology file: " ++ ontologyfile
                              putStr "Parsing... "
                              ontologyContent <- readFile ontologyfile
                              case pceParser ontologyContent ontologyfile of
                                Left err -> do putStrLn err
                                               putStrLn errExit

                                Right tenv -> do putStrLn "Done."
                                                 putStr "Type Checking... "
                                                 numTypeOk <- foldM' (typeCheckFile tenv) 0 docs
                                                 if numTypeOk < (length docs)
                                                   then putStrLn errExit
                                                   else do let decls = map snd $ AST.docsToList docs
                                                           case runRDR decls of --check that only defined rules are referenced
                                                             Left err -> do putStrLn err
                                                                            putStrLn errExit
                                                             Right () -> do putStrLn $ show $ AST.docsToList docs
                                                                            putStrLn $ "Starting compilation"
                                                                            let dpl = runCompile $ AST.docsToList docs
                                                                            case dpl of
                                                                              Left err -> putStrLn err
                                                                              Right program -> putStrLn $ show (DPL.clauses program)
                                                                            putStrLn "Done."
                                                                         --TODO do stuff with typechecked docs
processFile acc file =
    do content <- readFile file       
       case parse documentParser file content of
         Left err -> do putStrLn $ show err
                        return acc
         Right doc -> return (doc:acc)

typeCheckFile tenv acc doc =
    do case runWF tenv doc of
         Left err -> do putStrLn $ show err
                        return acc
         Right () -> return $ acc + 1

applyToFilesInDir dir f =
    do dirFiles <- getFilesInDir dir
       foldM' (\a b -> f a b ) [] dirFiles

numFiles dir =
    do dirFiles <- getFilesInDir dir
       return $ length dirFiles

getFilesInDir dir =
    do dirContentRelative <- getDirectoryContents dir
       let dirContentAbsolute = map (\e -> (++) (manageDirTrailingSlash dir) e) dirContentRelative
       dirFiles <- filterM (\e -> doesFileExist e) $ dirContentAbsolute
       return dirFiles

manageDirTrailingSlash :: FilePath -> FilePath
manageDirTrailingSlash dir =
    if isSuffixOf "/" dir
        then dir
        else (++) dir "/"

foldM'             :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ a []      =  return $! a
foldM' f a (x:xs)  =  let m = f a x in
                      m `seq` (m >>= \fax -> fax `seq` foldM' f fax xs)



