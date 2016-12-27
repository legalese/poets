import Poets.Reporting.Loader
import Poets.Data.PCE
import Poets.Reporting.Language.Parrot

loadFile file  = do
  spec <- readFile file
  env <- readOntology "examples/ontologies"       
  res <- loadParrotReport 1 env (ReportDecl "testReport" "this is a test" spec) 
  case res of
    Left err -> print err
    Right res -> print "okay"


