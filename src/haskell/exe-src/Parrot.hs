module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.Directory
import System.Exit

import Control.Monad

import Poets.Data
import Poets.Data.PCE
import Poets.Reporting.Language.Parrot

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (opts, [file], []) -> do 
        file' <- canonicalizePath file
        case opts of
          [] -> checkReport (newRecordEnv []) Nothing file'
          Ontology path : _ -> do
                            files <- recDirs path
                            on <- fromOntology files
                            checkReport on Nothing file'
    (_,_,msgs) -> putStrLn (concat msgs ++ usageInfo header options)  >> exitFailure
    where fromOntology path = readOntology path 

checkReport ontology lib file
    = do res <- typeReportFile ontology lib file
         case res of
           Left err -> putStrLn (show err) >> exitFailure
           Right _ -> putStrLn "report module is admissible"

data Options = Ontology FilePath
             deriving Show
               
header = "Usage: parrot [OPTION...] parrot-file"

options :: [OptDescr Options]
options = [Option "o" ["ontology"] ontology "path to an ontology file or directory containing ontology files"]
    where ontology = ReqArg Ontology "PATH"

recDirs path = do 
  ex <- doesDirectoryExist path
  if ex then liftM concat (mapM recDirs =<< getDirectoryContents path)
        else if takeExtension path == ".ext"
             then return [(path,path)]
             else return []