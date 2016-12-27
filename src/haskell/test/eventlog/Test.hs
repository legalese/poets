module Main where

import Poets.Data
import System.IO.Error
import System.FilePath
import System.Directory
import Poets.EventLog
import Control.Monad
import Control.Monad.Error
import Poets.Logging
import Poets.Config
import qualified System as System
import Data.DateTime

import Poets.Contracts as Contracts
import Poets.Contracts.Base as Base
--import Poets.Contracts.Repository as Repository
import Poets.Contracts.Language.CL1 as CL1
import Poets.Reporting

main = do
  let confFile = "examples/example.config"
  reportSpec <- readFile "examples/reporting/ActiveContracts.hs"
  confFileAbs <- canonicalizePath confFile
  setCurrentDirectory (takeDirectory confFileAbs)
  eConf <- parseConfigFile confFileAbs
  case eConf of
    Left err ->
        fail $ "An error occurred while parsing the " ++
                 "configuration file \"" ++ confFile ++"\": " ++ err
    Right conf -> do
               setupLogging (serverConfig conf)
               -- Translate POETS Controlled English to OWL
               pceContents <- readFile (dataPceOntology $ dataConfig conf)
               case pceParser pceContents (dataPceOntology $ dataConfig conf) of
                 Left err -> do
                    emergencyData err 
                    fail "Data module failed to start"
                 Right te -> do
                    eventLog <- createLog (eventsConfig conf) te
                    let createContractEvents = 
                            map (\cid -> createContractEvent cid mData) [1..10]
                    let paymentEvents = 
                            map (\cid -> createPaymentEvent cid True (valueData 1)) [1..10]
                    evL <- getEvents' eventLog
                    case length evL of
                      0 -> mapM (logEvent eventLog) (createContractEvents ++ paymentEvents)
                      _ -> do return []
                    contractEngine <- Contracts.createContractEngine
                                      (contractsConfig conf) eventLog te
                    reportEngine <- createReportEngine (reportsConfig conf) eventLog te
                    let name = "activeContracts"
                    let desc = "desc"
                    a <- addReport reportEngine ReportDecl
                             {reportName = name,
                              reportSpec = reportSpec,
                              reportDesc = desc}
                    print a
                    evL <- getEvents' eventLog
                    print $ length createContractEvents
                    reports <- listReports reportEngine
                    print reports
                    print "done"



typeCheckEvents te evs = 
    case mapM (typeCheckEvent te) evs of
      Left err -> fail err
      Right _ -> ""

mData :: ContractMetaData
mData = 
    let le1 = VF{field_name = "legalEntityOne",
                 field_value = inject $ VRecord VR{record_name = "LegalEntity",
                                                   record_fields = [VF{field_name = "name",
                                                                       field_value = inject $ VString "me"}]}} in
    let le2 = VF{field_name = "legalEntityTwo",
                 field_value = inject $ VRecord VR{record_name = "LegalEntity",
                                                   record_fields = [VF{field_name = "name",
                                                                       field_value = inject $ VString "you"}]}} in
    let sd = VF{field_name = "startDate",
                field_value = inject $ VDateTime startOfTime} in
    let tn = VF{field_name = "templateName",
                field_value = inject $ VString "simpleSale"} in
    inject $ VRecord VR{record_name = "Contract", 
                        record_fields = [le1, le2, sd, tn]}

valueData :: Double -> ValueExpr
valueData value =
    -- returns a record of type Currency < ValueDesignator < Resource
    let name = VF{field_name = "resourceName",
                  field_value = inject $ VString "currency"}
        quan = VF{field_name = "resourceQuantity",
                  field_value = inject $ VDouble value} in
    inject $ VRecord VR{record_name = "Currency",
                        record_fields = [name, quan]}
   
createPaymentEvent ::  ContractId -> Bool -> ValueExpr -> Event
createPaymentEvent cid isOne value =
    inject $
    VRecord VR{record_name = "Payment",
               record_fields =
                   [VF{field_name = "value",
                       field_value = value},
                    VF{field_name = createContractEventFieldContractId,
                       field_value = inject $ VInt cid},
                    VF{field_name = "isLegalEntityOne",
                       field_value = inject $ VBool isOne},
                    VF{field_name = "timeStamp",
                       field_value = inject $ VDateTime startOfTime},
                    VF{field_name = "transactionRouting",
                       field_value = inject $ VString ""}]}

-- Generate a "CreateContract" event
createContractEvent :: ContractId -> ContractMetaData -> Event
createContractEvent cid m =
    inject $
    VRecord VR{record_name = createContractEventClass,
               record_fields =
                   [VF{field_name = createContractEventFieldMetaData,
                       field_value = m},
                    VF{field_name = createContractEventFieldContractId,
                       field_value = inject $ VInt cid}]}