module Main where
import Poets.EventLog
import Poets.Reporting.Language.FunSETL.TypeChecker
import Poets.Reporting.Language.FunSETL.Pretty
import Poets.Reporting.Report
import Poets.Reporting.Loader
import Poets.Reporting.Interface
import Poets.Data.Type
import Poets.Data.Value
import Poets.Data.ALaCarte
import Poets.Reporting
import Linker
import System
import System.FilePath
import System.Directory
import Poets.Config
import Poets.Data.Rdfsparser as RdfsParser

main =
    do args <- getArgs
       case args of
         [confFile] -> do
                   confFileAbs <- canonicalizePath confFile
                   setCurrentDirectory (takeDirectory confFileAbs)
                   eConf <- parseConfigFile confFileAbs
                   case eConf of
                     Left msg -> putStrLn ("An error occurred while parsing the configuration file \"" ++ confFile ++"\"!")
                              >> putStrLn(msg)
                     Right conf -> do
                                typeEnv <- RdfsParser.parseFile (dataOntology $ dataConfig conf)
                                case typeEnv of
                                  Left err -> print err
                                  Right te -> do 
                                               eventLog <- createLog (eventsConfig conf) te
                                               reportEngine <- createReportEngine
                                                               (reportsConfig conf) eventLog te
                                               runExample eventLog reportEngine
         _ -> putStrLn("Syntax: poetsserver <configfile>")

runExample eventLog reportEngine = do
  spec <- readFile "reporting/TestReport.hs"
  logEvent eventLog $ inject $ VRecord (VR "DeliverEvent" [])
  logEvent eventLog $ inject $ VRecord (VR "BlaEvent" [])
  reports <- listReports reportEngine
  putStr $ "registered reports:\n" ++ (concatMap (++ "\n") reports)
  added <- addReport reportEngine ReportDecl {reportName = "testReport",
                                     reportDesc = "this is a test",
                                     reportSpec = spec }
  print added
  showLinkerState
  result <- queryReport reportEngine "testReport" [] []
  case result of
    Left err -> putStrLn "An error occurred!" >> print err
    Right value -> putStrLn  $"result: " ++ show value
  rem <- removeReport reportEngine "testReport"
  print rem
  showLinkerState
  stopLog eventLog