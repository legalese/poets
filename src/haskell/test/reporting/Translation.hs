import Poets.Reporting.Loader
import Poets.Data.PCE
import Poets.Reporting.Language.Parrot

import GHC
import Exception
import Panic
import HscTypes
import GHC.Paths
import DynFlags
import Outputable

transFile file target = do
  spec <- readFile file
  env <- readOntology "examples/ontologies"       
  let res = srcToHaskell env "Test" spec
  case res of
    Left err -> print err
    Right (hsmod,_) -> 
        runGhc (Just libdir) $ 
               liftIO $ writeFile target (show (ppr hsmod defaultUserStyle))

test1 = transFile "examples/reporting/ActiveContracts.rep" "ActiveContracts.hs"
test2 = transFile "examples/reporting/Lists.rep" "Lists.hs"