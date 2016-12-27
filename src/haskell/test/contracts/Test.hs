{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleInstances,
             FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  3gERP, 2009
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Test module for the contract enginge.
--
--
--------------------------------------------------------------------------------
module Main where

import Control.Monad.Error
import Network
import Thrift.Transport.Handle
import Thrift.Transport.Framed
import Thrift.Protocol.Binary
import Network
import Value_Types hiding (Value)
import qualified Value_Types as VTypes
import qualified Contracts_Types as CTypes
import PoetsServer_Client as Client
import Data.Comp.Variables
import Data.Either
import PServer.Thrift.Encode
import Poets.Data
import Poets.Data.Render
import Poets.Data.Value.Utils
import Poets.Data.Type.Render ()
import Poets.Contracts.Base as Base
import Poets.Contracts.Language.CSL
import Poets.Contracts.Language.CSL.Render
import Poets.Contracts.Language.CSL.Desugar
import Poets.Contracts.Language.CSL.TypeChecker
import Poets.Contracts.Language.CSL.Interpreter
import Poets.Contracts.Language.CSL.Typing.Inference
import Poets.Contracts.Language.CSL.Typing.Simplify
--import Poets.Contracts.Language.CSL.Algebras.Exp.TypeInferer
--import Poets.Contracts.Language.CSL.Algebras.Exp.Evaluator
--import Poets.Contracts.Language.CSL.Algebras.Exp.Marshal ()
import qualified Poets.Contracts.Language.CSL.Parser as CParser
import qualified Data.Map as Map
import Data.DateTime
import Data.Maybe
import Text.Parsec hiding (Empty,string)
import Text.PrettyPrint.Leijen
import Text.XML.Light

{-instance Show ContractTemplate where
    show = show . contractTemplateToDoc

instance Show ClauseTemplate where
    show = show . clauseTemplateToDoc-}

instance Render f => Render (f :&: SrcPos) where
    render (x :&: _) = render x

instance Show (f a) => Show ((f :&: SrcPos) a) where
    show (x :&: _) = show x

--testCSLParser = CParser.parseFile "test/contracts/test.contract"

--test :: IO (Either CSLError (Contract (Either Violation Clause)))
--test = do
--  tEnv <- readOntology "examples/ontologies/"
--  p <- testCSLParser
--  case p of
--    Left err ->
--        fail $ show err
--    Right (contracts,clauses,_) ->
--        let contractTemplates = Map.fromList $ map (\c -> (contractTemplateName c, c)) contracts in
--        let clauseTemplates = Map.fromList $ map (\c -> (clauseTemplateName c, c)) clauses in
--        case (typeCheckClauseTemplates tEnv clauses >>=
--              \templateEnv -> typeCheckContractTemplates templateEnv tEnv contracts) of
--          Left err ->
--              fail $ show err
--          Right _ ->
--              let (res :: Either CSLError (Contract (Either Violation Clause))) = Base.instantiate (contractTemplates, clauseTemplates) meta in
--              case res of
--                Left _ ->
--                    fail "crash" -- $ show err
--                Right c ->
--                    case contractContent c of
--                      (Left _) ->
--                          fail "Violation"
--                      (Right clause) ->
--                          fail $ show $ clauseToDoc clause

parseExpr :: String -> Either ParseError ExprPos
parseExpr = CParser.parseExpr ""

parseExprCore :: String -> Either ParseError ExprCorePos
parseExprCore = fmap desugarExpr . parseExpr

normExpr :: ExprCore -> ExprCore
normExpr e = normalizeExpr (const $ const False) (Map.singleton "x" (iVInt 5)) Map.empty e

e :: ExprCore
e = iEBinOp PLUS (iEVar "x") (iEBinOp PLUS (iVInt 3) (iVInt 32))

--type TypeEnv = RecordEnv (Term (TypeConstant :*: SrcPos :+: TypeList :*: SrcPos))

{-- Use this function to display the generated typing constraints
typecheckExpr e = do
  tEnv <- readOntology "examples/ontologies/"
  let tEnv' :: TypeEnv = fmap (constP Nothing) tEnv
  let env = Environment{variableEnv = Map.empty,
                            clauseTemplateEnv = Map.empty,
                            recordEnv = tEnv,
                            srcPos = Nothing}
  case parseExpr e of
    Left err ->
        putStrLn $ show err
    Right e' ->
        case typeInferExpr env e' of
          Left err ->
              putStrLn $ show err
          Right tp -> do
              putStrLn $ "Parsed expression: " ++ show e'
              putStrLn $ "Inferred type: " ++ show (typingType tp)
              putStrLn $ "Inferred subtype constraints: " ++
                         show (typingTypeConstraints tp)

test1 = typecheckExpr "2 + 3"
test2 = typecheckExpr "foldr (\\x y -> x+y) true [3,2,1]"
test3 = typecheckExpr "\\x -> x x"-}

{-evalExpr' e = do
  tEnv <- readOntology "4djs/ontologies/"
  mdefs <- readFiles tEnv (Just "4djs/Prelude.csl") "4djs/contracts/"
  case mdefs of
    Left e ->
        putStrLn $ show e
    Right (defs,_) ->
        case parseExpr e of
          Left err ->
              putStrLn $ show err
          Right e' ->
              case evalExpr (functionEnvironment defs) e' of
                Left err ->
                    putStrLn err
                Right v ->
                    putStrLn $ show v

a :: Term (Value :+: Exp)
a = inject $ VInt 42

b :: Maybe (Term Value)
b = deepProject a

normalizeExpr' e = do
  tEnv <- readOntology "4djs/ontologies/"
  mdefs <- readFiles tEnv (Just "4djs/Prelude.csl") "4djs/contracts/"
  case mdefs of
    Left e ->
        putStrLn $ show e
    Right (defs,_) ->
        case parseExpr e of
          Left err ->
              putStrLn $ show err
          Right e' ->
              putStrLn $ show $ normalizeExpr (functionDefinitions defs) e'

calcPriceTest = "x == calcPrice 100 10 2010-10-10 12:00:00 2010-10-10 14:30:00"-}

{-teste :: Expr
teste = inject $ EBinOp{expBinOp = "=",
                        expArg1 = inject $ EProj{expFieldName = "test",
                                        expArg = inject $ VList [inject $ VInt 23],
                                        expSourcePos = Nothing},
                        expArg2 = inject $ VRecord $
                                  VR{vrecordName = "testrecord",
                                     vrecordFields = [VF{vfieldName = "agent",
                                                         vfieldValue = inject $ VString "ME"}]},
                        expSourcePos = Nothing}


-- Read OWL definitions
readOwl  = do Right recordenv <- parseRdfsFile "../../../ontology/poets.owl"
              let env = TypeEnvironment{variableEnv = Map.empty,
                                        templateEnv = Map.empty,
                                        recordEnv = recordenv}
              return env

-- Type check a contract template
typecheck t = do env <- readOwl
                 let r = typeCheckTemplates (recordEnv env) t
                 case r of 
                   Right () -> putStrLn "Templates are well-typed."
                   Left e -> putStrLn $ "Template is ill-typed. Reason: " ++ showTypeError e
    where showTypeError (Poets.Contracts.Language.CSL.TypeChecker.RunTimeError s) = s
          showTypeError (Poets.Contracts.Language.CSL.TypeChecker.TypeError s _) = s

{-- Type check an event
typecheckE e = do env <- readOwl
                  let r = typeCheckEvent (recordEnv env) e
                  case r of 
                    Right _ -> putStrLn "Event is well-typed."
                    Left e -> putStrLn $ "Event is ill-typed. Reason: " -- ++ show e-}

-- Type check a contract expression
typeCheckExpr :: Expr -> IO ()
typeCheckExpr e = do env <- readOwl
                      case typeCheckExpr env e of
                        Right (tp :: CType) -> putStrLn $ "Expression has type "-- ++ (show tp)
                        Left e -> putStrLn $ "Expression is ill-typed. Reason: " ++ (show e)

le :: String -> ValueExpr
le s = inject $ VRecord $
       VR{vrecordName = "LegalEntity",
          vrecordFields = [VF{vfieldName = "name",
                              vfieldValue = inject $ VString s}]}

var :: String -> Expr
var v = inject $ EVar{expVar = v,
                      expSourcePos = Nothing}

{-c1 = ContractInstance{contractInstanceLastUpdate = 0,
                      contractInstanceMetaData = m1,
                      contractInstanceContract = clause}

m1 :: ValueExpr
m1 = inject $ VRecord $
     VR{vrecordName = "Contract",
        vrecordFields = [--VF{vfieldName = contractFieldContractId,
                         --   vfieldValue = inject $ VInt 1},
                         VF{vfieldName = "legalEntityOne",
                            vfieldValue = le "MaxPro"},
                         VF{vfieldName = "legalEntityTwo",
                            vfieldValue = le "Customer"},
                         VF{vfieldName = contractTemplateName,
                            vfieldValue = inject $ VString "standardSale"}]}--,
                         --VF{vfieldName = contractFieldClause,
                         --   vfieldValue = inject $ VString "Paper text"}]}-}

{-
matchE e c tpdefs recordenv = case match tpdefs recordenv c e of
                      Left e ->
                          putStrLn $ "Event matching failed. Reason: " -- ++ show e
                      Right c' ->
                          case extractRecord e of
                            Left e ->
                                putStrLn $ "Event matching failed. Reason: " ++ e
                            Right r ->
                                case lookupField eventFieldTimeStamp r of
                                  Left e ->
                                      putStrLn $ "Event matching failed. Reason: " ++ e
                                  Right (Expr (VTime t)) ->
                                      case update tpdefs t c' of
                                        Left e -> putStrLn $ "Event matching failed. Reason: " -- ++ show e
                                        Right c'' -> putStrLn $ show $ contractClause c''
  -}                                       


{-
clause = Commitment
         {
           clauseResponsible = One,
           clauseEventType = "Transform",
           clauseVarFields = Map.fromList [("a","agent"),("r1","sourceResource"),("r2","targetResource")],
           clausePredicate = [inject $
                              EBinOp{expBinOp =  "=",
                                     expArg1 = inject $
                                     EProj{expFieldName = "resourceName",
                                           expArg = var "r1",
                                           expAnnot = NoAnnot},
                                     expArg2 = inject $
                                     EProj{expFieldName = "resourceName",
                                           expArg = var "r2",
                                           expAnnot = NoAnnot},
                                     expAnnot = NoAnnot},
                              inject $
                              EBinOp{expBinOp =  "=",
                                     expArg1 = inject $
                                     EProj{expFieldName = "resourceQuantity",
                                           expArg = var "r1",
                                           expAnnot = NoAnnot},
                                     expArg2 = inject $
                                     VDouble 10,
                                     expAnnot = NoAnnot},
                              inject $
                              EBinOp{expBinOp =  "=",
                                     expArg1 = inject $
                                     EProj{expFieldName = "resourceQuantity",
                                           expArg = var "r2",
                                           expAnnot = NoAnnot},
                                     expArg2 = inject $
                                     VDouble 20,
                                     expAnnot = NoAnnot}],
           clauseDeadline = (Just (inject $
                                   EBinOp{expBinOp = "+",
                                          expArg1 = inject $ VTime 10,
                                          expArg2 = inject $ VInt 5,
                                          expAnnot = NoAnnot},
                                   Just clause4)),
           clauseContinuation = ("t",clause2),
           clauseAnnot = NoAnnot}


clause2 = Commitment{clauseResponsible = One,
                     clauseEventType = "Deliver",
                     clauseVarFields = Map.fromList [("a","deliveryAddress"),("r3","resource")],
                     clausePredicate = [inject $
                                        EBinOp{expBinOp =  "=",
                                               expArg1 = var "r2",
                                               expArg2 = var "r3",
                                               expAnnot = NoAnnot}],
                     clauseDeadline = Just (inject $
                                            EBinOp{expBinOp = "+",
                                                   expArg1 = var "t",
                                                   expArg2 = inject $
                                                             EBinOp{expBinOp = "+",
                                                                    expArg1 = inject $ VInt 10,
                                                                    expArg2 = inject $ VInt 7,
                                                                    expAnnot = NoAnnot},
                                                   expAnnot = NoAnnot},
                                            Nothing),
                     clauseContinuation = ("t", Empty{clauseAnnot = NoAnnot}),
                     clauseAnnot = NoAnnot}


clause3 = If{clauseCondition = inject $
                               EBinOp{expBinOp = "=",
                                      expArg1 = var "x",
                                      expArg2 = var "y",
                                      expAnnot = NoAnnot},
             clauseThen = clause,
             clauseElse = clause4,
             clauseAnnot = NoAnnot}

clause4 = Do{clauseSubs = Map.fromList [("l1",Empty{clauseAnnot = NoAnnot}),
                                        ("l2",Empty{clauseAnnot = NoAnnot})],
             clauseAnnot = NoAnnot}-}


res :: Double -> ValueExpr
res n = inject $ VRecord $
      VR{vrecordName = "Resource",
         vrecordFields = [VF{vfieldName = "resourceName",
                             vfieldValue = inject $ VString "Printer"},
                          VF{vfieldName = "resourceQuantity",
                             vfieldValue = inject $ VDouble n}]}

cid = 16

{-e :: ValueExpr
e = inject $ VRecord $
    VR{vrecordName = "Transform",
       vrecordFields = [VF{vfieldName = "contractId",
                           vfieldValue = inject $ VInt cid},
                        VF{vfieldName = "eventRouting",
                           vfieldValue = inject $ VString "l1"},
                        VF{vfieldName = "isLegalEntityOne",
                           vfieldValue = inject $ VBool True},
                        VF{vfieldName = "timeStamp",
                           vfieldValue = inject $ VTime 10},
                        VF{vfieldName = "agent",
                           vfieldValue = inject $ VRecord $
                                         VR{vrecordName = "Agent",
                                            vrecordFields = [VF{vfieldName = "name",
                                                                vfieldValue = inject $ VString "Mii"}]}},
                        VF{vfieldName = "sourceResource",
                           vfieldValue = res 10},
                        VF{vfieldName = "targetResource",
                           vfieldValue = res 20}]}-}
m = do env <- readOwl
       matchE e c1 Map.empty $ recordEnv env-}


--inject $ VDateTime $ fromGregorian 2010 12 01 12 0 0
--time = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><datetime value=\"2010-12-07T12:00:00\" xmlns=\"http://poets.diku.dk/values\"/>"

{-time = VTypes.DateTime{f_DateTime_year = Just 2010,
                       f_DateTime_month = Just 10,
                       f_DateTime_day = Just 23,
                       f_DateTime_hour = Just 09,
                       f_DateTime_minutes = Just 58,
                       f_DateTime_seconds = Just 51,
                       f_DateTime_miliseconds = Just 0}-}

event = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><record name=\"TransferAndDeliver\" xmlns=\"http://poets.diku.dk/values\"><field name=\"contractId\"><int value=\"30\"/></field><field name=\"eventRouting\"><string value=\"\"/></field><field name=\"isLegalEntityOne\"><bool value=\"true\"/></field><field name=\"timeStamp\"><datetime value=\"2010-04-07T13:47:20.001+02:00\"/></field><field name=\"location\"><record name=\"Address\"><field name=\"addressString\"><string value=\"Somewhere\"/></field></record></field><field name=\"resource\"><record name=\"Resource\"><field name=\"resourceTypes\"><list><record name=\"ResourceType\"><field name=\"resourceQuantity\"><double value=\"1.0\"/></field><field name=\"resourceName\"><string value=\"Nintendo Wii\"/></field></record><record name=\"ResourceType\"><field name=\"resourceQuantity\"><double value=\"2.0\"/></field><field name=\"resourceName\"><string value=\"iPhone\"/></field></record></list></field></record></field></record>"

runClient cid et = do handle <- hOpen("127.0.0.1", PortNumber 7911)
--                         let et = [] --["TransferOwnership{contractId=" ++ show cid ++ ",eventRouting=\"l1\",isLegalEntityOne=true,resource=Resource{resourceName=\"Nintendo Wii\",resourceQuantity=10.0},timeStamp=time(50)}"]
                      result <- Client.getExpectedTransactions
                                          (BinaryProtocol handle,
                                           BinaryProtocol handle) cid et
                      let (Just x) = CTypes.f_TransactionPattern_predicate $ head result
--                         let Just x = head result
--                         let Just y = head x
                      putStrLn $ show x -- $ length result

meta :: ContractMetaData
meta = inject $ VRecord $
     VR{vrecordName = "Sale",
        vrecordFields =
            newFields [VF{vfieldName = "startDate",
                          vfieldValue = iVDateTime $ createDateTime (fromJust $ createDate 2010 12 01) (fromJust $ createTime 12 0 0 0)},
                       VF{vfieldName = "templateName",
                          vfieldValue = inject $ VString "sale"},
                       VF{vfieldName = "buyer",
                          vfieldValue =
                              inject $ VRecord VR{vrecordName = "Buyer",
                                                  vrecordFields = newFields
                                                      [VF{vfieldName = "name",
                                                          vfieldValue = inject $ VString "Me"}]}},
                       VF{vfieldName = "seller",
                          vfieldValue =
                              inject $ VRecord VR{vrecordName = "Seller",
                                                  vrecordFields = newFields
                                                  [VF{vfieldName = "name",
                                                      vfieldValue = inject $ VString "You"}]}}]}

meta2 :: ContractMetaData
meta2 = inject $ VRecord $
     VR{vrecordName = "DebugContract",
        vrecordFields = newFields [VF{vfieldName = "startDate",
                          vfieldValue = iVDateTime $ createDateTime (fromJust $ createDate 2010 12 01) (fromJust $ createTime 12 0 0 0)},
                       VF{vfieldName = "templateName",
                          vfieldValue = inject $ VString "debugContract"},
                       VF{vfieldName = "debugString",
                          vfieldValue = inject $ VString "oh no\nlinebreaks!"}]}

-- Encode a POETS value in the Thrift representation
valueEncode :: Poets.Data.Value -> VTypes.Value
valueEncode v = let (idx,valMap) = encodeTerm v in
                VTypes.Value{f_Value_values = Just valMap,
                             f_Value_root = Just idx}

getBinPro = do
  handle <- hOpen("127.0.0.1", PortNumber 7911)
  framed <- openFramedTransport handle
  return $ BinaryProtocol framed


instantiateClient = do
  binpro <- getBinPro
  result <- Client.createContract (binpro, binpro) (valueEncode meta2)
  putStrLn $ show result

updateClient cId = do
  binpro <- getBinPro
  result <- Client.updateContract (binpro, binpro)
                                  cId
                                  (valueEncode meta2)
  putStrLn $ show result

getContr cId = do
  binpro <- getBinPro
  result <- Client.getContract (binpro, binpro) cId []
  putStrLn $ show result




{-runClient3 cid et = do
  handle <- hOpen("127.0.0.1", PortNumber 7911)
  result <- getContract (BinaryProtocol handle,
                         BinaryProtocol handle) cid et
  putStrLn result-}

isConc id = do
  binpro <- getBinPro
  result <- Client.isConcludable (binpro, binpro) id
  putStrLn $ show result


createEntityClient = do
  binpro <- getBinPro
  result <- Client.createEntity (binpro, binpro)
                                (valueEncode entity) "Currency"
  putStrLn $ show result
  where entity :: ContractMetaData
        entity = iVRecord $
                 VR{vrecordName = "DKK",
                    vrecordFields = newFields []}

updateEntityClient id = do
  binpro <- getBinPro
  result <- Client.updateEntity (binpro, binpro)
                                id (valueEncode entity)
  putStrLn $ show result
  where entity :: ContractMetaData
        entity = iVRecord $
                 VR{vrecordName = "USD",
                    vrecordFields = newFields []}

deleteEntityClient id = do
  binpro <- getBinPro
  result <- Client.deleteEntity (binpro, binpro)
                                id
  putStrLn $ show result

queryReportClient name args = do
{-(h, _, _) <- accept s
            framed <- openFramedTransport h
            let binpro = BinaryProtocol framed
            return (binpro, binpro)
-}
  handle <- hOpen("127.0.0.1", PortNumber 7911)
  framed <- openFramedTransport handle
  let binpro = BinaryProtocol framed
  result <- Client.queryReport (binpro,binpro) -- (BinaryProtocol handle, BinaryProtocol handle)
                               name
                               []
                               args
  putStrLn $ show result

{-
{-parse = do mc <- CParser.parseTemplatesFile "test.contract"
           case mc of
             Left err -> putStrLn $ "Error: " ++ show err
             Right ts -> do putStrLn $ show $ templateToDoc $ head ts
                            typecheck ts

parseV s = let mv = parseValue s in
           case mv of
             Left err -> (putStrLn $ "Error: " ++ show err) >> (return $ inject $ VInt 42)
             Right v -> return v
-}

testval = "Transform{contractId = 1\n         ,eventRouting = \"\"\n         ,isLegalEntityOne = true\n         ,timeStamp = time(10)\n         ,agent = Agent{name = \"Mii\"}\n         ,sourceResource = Resource{resourceName = \"Printer\"\n                                   ,resourceQuantity = 10.0}\n         ,targetResource = Resource{resourceName = \"Printer\"\n                                   ,resourceQuantity = 20.0}}"

testval2 = "Transform{contractId = 1\n         ,eventRouting = \"\"\n         ,isLegalEntityOne = true\n         ,timeStamp = time(10)\n         ,agent = Agent{name = \"Mii\"}\n         ,sourceResource = Resource{resourceName = \"Printer\"\n                                   ,resourceQuantity = 10.0}\n         ,targetResource = Resource{resourceName = \"Printer\"\n                                   ,resourceQuantity = 20.0}}"

{-testrepo :: Repository.Repository Clause
testrepo = Map.fromList $ [(1,c1)]-}

main = putStrLn "Test"


xmlval = "<?xml ?><record name=\"Contract\" xmlns=\"http://poets.diku.dk/values\"> <field name=\"legalEntityTwo\">   <record name=\"Customer\">     <field name=\"name\">       <string value=\"Jesper\"/>     </field>   </record> </field> <field name=\"startDate\">   <datetime value=\"2010-04-01T11:31:07.001+02:00\"/> </field> <field name=\"legalEntityOne\">   <record name=\"Customer\">     <field name=\"name\">       <string value=\"Erik\"/>     </field>   </record> </field> <field name=\"templateName\">   <string value=\"testmult\"/> </field></record>"

xmlval2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><record name=\"Contract\" xmlns=\"http://poets.diku.dk/values\"><field name=\"legalEntityTwo\"><record name=\"LegalEntity\"><field name=\"name\"><string value=\"wq\"/></field></record></field><field name=\"startDate\"><datetime value=\"2010-04-01T00:58:00\"/></field><field name=\"legalEntityOne\"><record name=\"PrivateIndividual\"><field name=\"name\"><string value=\"te\"/></field></record></field><field name=\"templateName\"><string value=\"testinstallment\"/></field></record>"

xmlval3 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><record name=\"Contract\" xmlns=\"http://poets.diku.dk/values\"><field name=\"legalEntityTwo\"><record name=\"Customer\"><field name=\"name\"><string value=\"23\"/></field></record></field><field name=\"startDate\"><datetime value=\"2010-04-01T14:27:27.001+02:00\"/></field><field name=\"legalEntityOne\"><record name=\"Vendor\"><field name=\"name\"><string value=\"e\"/></field></record></field><field name=\"templateName\"><string value=\"testmult\"/></field></record>"

xmlval4 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><record name=\"Contract\" xmlns=\"http://poets.diku.dk/values\"><field name=\"legalEntityTwo\"><record name=\"Customer\"><field name=\"name\"><string value=\"Tom\"/></field></record></field><field name=\"startDate\"><datetime value=\"2010-04-05T11:23:24.001+02:00\"/></field><field name=\"legalEntityOne\"><record name=\"Vendor\"><field name=\"name\"><string value=\"Tom2\"/></field></record></field><field name=\"templateName\"><string value=\"test\"/></field></record>"

xmlval5 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><record xmlns=\"http://poets.diku.dk/values\" name=\"CreateContract\"><field name=\"contract\"><record name=\"Contract\"><field name=\"legalEntityTwo\"><record name=\"Customer\"><field name=\"name\"><string value=\"Tom\" /></field></record></field><field name=\"startDate\"><datetime value=\"2010-04-06T12:43:24.001Z\" /></field><field name=\"legalEntityOne\"><record name=\"Vendor\"><field name=\"name\"><string value=\"Mette\" /></field></record></field><field name=\"templateName\"><string value=\"test\" /></field></record></field><field name=\"contractId\"><int value=\"0\" /></field></record>"-}

xmlval = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><record xmlns=\"http://poets.diku.dk/values\" name=\"TransferAndDeliver\"><field name=\"location\"><record  name=\"Address\"> <field name=\"addressString\"><string value=\"Som\"/></field>    </record></field><field name=\"resource\"><list><record  name=\"Resource\"> <field name=\"resourceName\"><string value=\"gr\"/></field><field name=\"resourceQuantity\"><double value=\"9.0\"/></field>  </record></list></field> <field name=\"contractId\"><int value=\"9\"/></field><field name=\"isLegalEntityOne\"><bool value=\"true\"/></field><field name=\"timeStamp\"><datetime value=\"2010-07-23T09:32:42+02:00\"/></field><field name=\"transactionRouting\"><string value=\"\"/></field>  </record>"

{-testParser filePath = do
  cont <- readFile filePath
  case fmap desugarContractDefinition (CParser.parseContract filePath cont "" "" "") of
    Left err ->
        putStrLn $ show err
    Right c ->
        putStrLn "TODO" -- $ show $ clauseDefinitionToDoc $ head $ map snd $ Map.toList $ contractDefClauseDefs c-}