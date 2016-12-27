/*
 * Contract engine definitions.
 *
 * Thrift does not support recursive data types. We therefore encode the AST of
 * CSL expressions using pointers.
 */

namespace java dk.diku.poets.gen.thrift.contracts
include "value.thrift"

typedef i32 ContractId
typedef string TemplateName
typedef string Var
typedef string CSL
enum TransactionKind {Obligation = 1, Permission = 2}

// A transaction consists of the ID of the contract to which the transaction
// belongs, the time stamp of the transaction, and the transaction data itself
struct Transaction {
  1 : ContractId contractId,
  2 : value.DateTime timeStamp,
  3 : value.Value transactionData
}

// A deadline is a DateTime interval.
struct Deadline {
  1 : value.DateTime lowerLimit
  2 : value.DateTime upperLimit
}

// Record projection expression
struct RecordProj {
  1 : value.Index recordExp
  2 : value.FieldName fieldName // Field to project
}

// Record update expression
struct RecordUpdate {
  1 : value.Index recordExp
  2 : value.FieldName fieldName // Field to update
  3 : value.Index updateExp
}

// Binary operators
enum BinOp {
  EQ = 1,
  LEQ = 2,
  PLUS = 3,
  TIMES = 4,
  DIV = 5,
  AND = 6,
  CONS = 7,
  DPLUS = 8,
  DTIMES = 9
}

// Binary operator expression
struct BinaryOp {
  1 : BinOp operator
  2 : value.Index leftExp
  3 : value.Index rightExp
}

// Conditional expression
struct IfThenElse {
  1 : value.Index conditionalExp
  2 : value.Index thenExp
  3 : value.Index elseExp
}

struct CaseExp {
  1 : value.RecordName rName
  2 : Var variable
  3 : value.Index body
}

// Case expression
struct Case {
  1 : value.Index caseExp
  2 : list<CaseExp> caseExps
}

// Lambda expression
struct Lambda {
  1 : Var variable
  2 : value.Index bodyExp
}

// Application expression
struct Application {
  1 : value.Index functionExp
  2 : value.Index argumentExp
}

// Buil-in functions
enum Function {
  Foldr = 1,
  Ceil = 2,
  SubtractDate = 3,
  Reports = 4
}

struct Unit {}

// Expression
struct Exp {
  1 : optional i32 intExp
  2 : optional bool boolExp
  3 : optional string stringExp
  4 : optional value.Date dateExp
  5 : optional value.Time timeExp
  6 : optional value.DateTime dateTimeExp
  7 : optional value.Duration durationExp
  8 : optional double realExp
  9 : optional value.Record recordExp
  10 : optional value.Entity entityExp
  11 : optional list<value.Index> listExps
  12 : optional value.FieldName fieldNameExp
  13 : optional RecordProj recordProjExp
  14 : optional RecordUpdate recordUpdateExp
  15 : optional BinaryOp binaryOpExp
  16 : optional Var variableExp
  17 : optional Lambda lambdaExp
  18 : optional Application applicationExp
  19 : optional Function functionExp
  20 : optional IfThenElse ifThenElseExp
  21 : optional Case caseExp
  22 : optional Unit unitExp
}

struct Expression {
  1 : map<value.Index,Exp> expressions
  2 : value.Index root
}

// Transaction pattern
struct TransactionPattern {
  1 : ContractId contractId
  2 : optional string description
  3 : TransactionKind transactionKind
  4 : optional value.Value responsible // Set iff transactionKind == Obligation
  5 : value.RecordName transactionType
  6 : Expression predicate
  7 : Deadline deadline
}

struct Residual {
  1 : value.DateTime currentTime
  2 : CSL residualClause
}

// Contract not found
exception ContractNotFoundException {
  1 : ContractId contractId
}

// Contract not concludable
exception ContractNotConcludable {
  1 : ContractId contractId
}

// An annotated breach
struct Breach {
  1 : value.Value responsible
  2 : string description
}

// Contract breah
exception ContractBreach {
  1 : ContractId contractId
  2 : value.DateTime timestamp
  3 : list<Breach> responsibles
}

// Transaction was unexpected
exception UnexpectedTransaction {
  1 : value.Value transaction
}

// Tried to register an "old" transaction (with timestamp 'transactionTime')
// against a newer contract (last updated at 'contractLastUpdated'), i.e.
// 'transactionTime' <= 'contractLastUpdated'
exception TransactionTooOldException {
  1 : ContractId contractId
  2 : value.DateTime transactionTime
  3 : value.DateTime contractLastUpdated
}

// Error when updating a contract: The type of the contract meta data
// sent to the server ('newContractType') does not match the type of the
// original contract ('oldContractType').
exception UpdateTypeMismatch {
  1 : ContractId contractId
  2 : value.RecordName oldContractType
  3 : value.RecordName newContractType
}

// Contract template not found
exception TemplateNotFoundException {
  1 : TemplateName templateName
}

// Error when instantiating a contract: The type of the contract meta data
// sent to the server ('contractMetaDataType') does not match the type 
// ('templateType') of the template ('templateName') sent to the server.
exception TemplateTypeMismatchException {
  1 : TemplateName templateName
  2 : value.RecordName templateType
  3 : value.RecordName contractMetaDataType
}

// Contract definition not found
exception ContractDefNotFoundException {
  1 : string name
}

// Contract definition already exists
exception ContractDefExistsException {
  1 : string name
}

// Parse error
exception ParseException {
  1 : string err
}

// Template definitions are not guarded
exception GuardednessException {
  1 : string err
}