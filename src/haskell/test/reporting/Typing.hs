{-# LANGUAGE ScopedTypeVariables #-}

import Poets.Reporting.Language.Parrot.Typing
import Poets.Reporting.Language.Parrot.Parser
import Poets.Data.PCE


{-| This function takes a record environment and a path to a Parrot
program file and typechecks it. The results are printed to stdout. -}

checkProg_ :: FilePath -> IO ()
checkProg_ path = do
    res <- parseProgramFile path
    env <- readOntology "examples/ontologies"       
    case res of
        Left err -> print err
        Right prog -> case typeProg env prog of
            Left err -> print err
            Right (extRecord, funTypings :: [(FunId, TypeScheme)]) -> do
                print extRecord
                putStrLn ""                
                printIdTypeSchemes funTypings


printIdTypeScheme :: (FunId, TypeScheme) -> IO ()
printIdTypeScheme (id, scheme) = putStrLn $ id ++ ": " ++ show scheme

printIdTypeSchemes = mapM_ printIdTypeScheme


test = checkProg_  "examples/reporting/ActiveContracts.rep"
test2 = checkProg_  "examples/reports/Test.rep"
test3 = checkProg_  "examples/reports/Test3.rep"
simplificationTest = checkProg_ "examples/reports/SimplificationTest.rep"

