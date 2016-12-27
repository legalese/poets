/*
 * Interface to the POETS server.
 *
 */

namespace java dk.diku.poets.gen.thrift
include "value.thrift"
include "type.thrift"
include "data.thrift"
include "entities.thrift"
include "contracts.thrift"
include "reporting.thrift"
include "rules.thrift"


// "unexpected" error
exception RunTimeException {
  1 : string errorMsg
}

service PoetsServer {
  // DATA DEFINITIONS
  /* Add a new set of data definitions to the system */
  void addDataDefinitions(
    1: list<data.PCE> pce
  ) throws (
    2: data.ValidationError validationError
    3: data.ParseError parseError
  )

  /* Retrieve the definition of a record */
  data.RecordDefinition getRecordDefinition(
    1: value.RecordName recordName
  ) throws (
    2: data.UndefinedRecord undefRecord
  )

  /* Retrieve the sub types of a given record */
  set<value.RecordName> getSubTypes(
    1: value.RecordName recordName
  ) throws (
    2: data.UndefinedRecord undefRecord
  )

  // CONTRACT ENGINE
  /* Get the residual clause of a running contract */
  contracts.Residual getContract(
    1: contracts.ContractId cid
    2: list<contracts.Transaction> transactions // "What if" transactions.
  ) throws (
    2: value.DecodeException msg
    3: contracts.ContractNotFoundException notFound
    4: contracts.ContractBreach contractBreach
    5: contracts.UnexpectedTransaction unexpectedTransaction
    6: value.TypeException typeErr
    7: contracts.TransactionTooOldException oldErr
    8: RunTimeException runtimeErr
  )

  /* Get a list of expected transactions for a running contract */
  list <contracts.TransactionPattern> getExpectedTransactions(
    1: contracts.ContractId cid
    2: list<contracts.Transaction> transactions // "What if" transactions.
  ) throws (
    2: value.DecodeException msg
    3: contracts.ContractNotFoundException notFound
    4: contracts.ContractBreach contractBreach
    5: contracts.UnexpectedTransaction unexpectedTransaction
    6: value.TypeException typeErr
    7: contracts.TransactionTooOldException oldErr
    8: RunTimeException runtimeErr
  )

  /* Register a (list of) transaction(s) for a running contract */
  void registerTransactions(
    1: list<contracts.Transaction> transactions
  ) throws (
    2: value.DecodeException msg
    3: contracts.ContractNotFoundException notFound
    4: contracts.ContractBreach contractBreach
    5: contracts.UnexpectedTransaction unexpectedTransaction
    6: value.TypeException typeErr
    7: contracts.TransactionTooOldException oldErr
    8: RunTimeException runtimeErr
    9: entities.EntityNotFound entityIdNotFound
    10: entities.EntityDeleted entityIdDeleted
  )

  /* Instantiate a new contract */
  contracts.ContractId createContract(
    1: value.Value contractMetaData
  ) throws (
    2: value.DecodeException msg
    3: value.TypeException typeErr
    4: contracts.TemplateNotFoundException templateNotFoundErr
    5: contracts.TemplateTypeMismatchException templateTypeErr
    6: contracts.ContractBreach contractBreach
    7: RunTimeException runtimeErr
    8: entities.EntityNotFound entityIdNotFound
    9: entities.EntityDeleted entityIdDeleted
  )

  /* Update a running contract */
  void updateContract(
    1: contracts.ContractId contractId
    2: value.Value contractMetaData
  ) throws (
    2: value.DecodeException msg
    3: value.TypeException typeErr
    4: contracts.TemplateNotFoundException templateNotFoundErr
    5: contracts.TemplateTypeMismatchException templateTypeErr
    6: contracts.UpdateTypeMismatch updateTypeErr
    7: RunTimeException runtimeErr
    8: contracts.ContractNotFoundException notFound
    9: contracts.ContractBreach contractBreach
    10: contracts.UnexpectedTransaction unexpectedTransaction
    11: entities.EntityNotFound entityIdNotFound
    12: entities.EntityDeleted entityIdDeleted
  )

  /* Cancel a running contract */
  void deleteContract(
    1: contracts.ContractId contractId
  ) throws (
    2: contracts.ContractNotFoundException notFound
    3: RunTimeException runtimeErr
  )

  /* Check if a running contract is concludable */
  bool isConcludable(
    1: contracts.ContractId cid
  ) throws (
    2: contracts.ContractNotFoundException notFound
    3: contracts.ContractBreach contractBreach
    4: RunTimeException runtimeErr
  )

  /* Conclude a running contract */
  void concludeContract(
    1: contracts.ContractId cid
  ) throws (
    2: contracts.ContractNotFoundException notFound
    3: contracts.ContractNotConcludable notConcludable
    4: contracts.ContractBreach contractBreach
    5: RunTimeException runtimeErr
  )

  /* Create a new contract definition */
  void createContractTemplate(
    1: contracts.CSL contractDef
  ) throws (
    2: value.TypeException typeErr
    3: contracts.ParseException parseException
    4: contracts.GuardednessException guardednessException
    5: contracts.ContractDefExistsException contractDefExists
  )

  /* Update an existing contract definition */
  void updateContractTemplate(
    1: contracts.CSL contractDef
  ) throws (
    2: value.TypeException typeErr
    3: contracts.ParseException parseException
    4: contracts.GuardednessException guardednessException
    5: contracts.ContractDefNotFoundException contractDefNotFound
  )

  /* Delete an existing contract definition */
  void deleteContractTemplate(
    1: string name
  ) throws (
    2: contracts.ContractDefNotFoundException contractDefNotFound
  )

  // ENTITY STORE
  /* Create a new entity */
  entities.EntityId createEntity(
    1: value.Value data
    2: value.RecordName type
  ) throws (
    2: value.DecodeException msg
    3: value.TypeException typeErr
  )

  /* Update an existing entity */
  void updateEntity(
    1: entities.EntityId entityId
    2: value.Value data
  ) throws (
    2: value.DecodeException msg
    3: value.TypeException typeErr
    4: entities.EntityNotFound entityIdNotFound
    5: entities.EntityDeleted entityIdDeleted
  )

  /* Delete an existing entity */
  void deleteEntity(
    1: entities.EntityId entityId
  ) throws (
    2: entities.EntityNotFound entityIdNotFound
    3: entities.EntityDeleted entityIdDeleted
  )

  //REPORTING ENGINE
  /*
   * This function provides meta information about the given report.
   */
  reporting.Report getReport(
    1 : reporting.ReportName name
  ) throws (
    2 : reporting.ReportNotFoundException notFound
  )

  /*
   * This function adds the given report to the reporting engine and returns
   * true, provided a report with the same name is not registered, yet.
   * Otherwise, no action is performed and false is returned.
   * In case the report specification is not well-formed a ReportInitException
   * is thrown
   */
  bool createReport(
    1 : reporting.ReportSpecification spec // specification of the report
  ) throws (
    2 : reporting.ReportInitException init
  )

  /*
   * This function modifies an already existent report by replacing it
   * with the one given as an argument here. The report that is replaced, is
   * the one with the same name as the one given as an argument. If no such
   * report is present no action is performed and this function returns false.
   * Otherwise, if the given report is valid it replaces the report of the same
   * name and returns true. In case the report specification is not well-formed
   * a ReportInitException is thrown and the original report is kept.
   */
  bool updateReport(
    1 : reporting.ReportSpecification spec // specification of the report
  ) throws (
    2 : reporting.ReportInitException init
  )
  
  /*
   * If a report of the given name is registered this function removes it
   * from the system and returns true. Otherwise, no action is performed and
   * false is returned.
   */
  bool deleteReport(
    1 : reporting.ReportName report
  )
  
  /*
   * This function executes the given report with the provided arguments.
   * The arguments have to be of the types as indicated by the report's 
   * meta information (see getReport()). The value returned by the report
   * is of the according return type as given in the report's meta information.
   */
  value.Value queryReport(
    1 : reporting.ReportName name      // name of the report to execute
    2 : list<value.Value> events    // events that the report is provided with
	                            // *additionally* to the events in the
                                    // event log
    3 : list<value.Value> arguments // arguments to supply to the execution of
                                    // the report
  ) throws (
    2 : reporting.ReportNotFoundException notFound,
    3 : RunTimeException runtime,
    4 : value.TypeException type
  )

  // RULE ENGINE
  /*
   * This function executes the given query on the rule engine and
   * returns a non-empty list of solutions (substitutions). If none
   * can be found an exception (4-7) is thrown. The rule set to use
   * is derived from the ontology.
   */
  list<map<string,rules.QValue>> queryRules(
    1: rules.QValue query // the query to execute
  ) throws (
    2: value.DecodeException decodeException,
    3: RunTimeException rulesRuntimeException,
    4: rules.Inconsistent inconsistentErr,
    5: rules.NoRuleSets noRuleSetsErr,
    6: rules.CannotComplete cannotCompleteErr,
    7: rules.PleaseSpecify pleaseSpecifyErr
    )
}