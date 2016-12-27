/*
 * Reporting engine definitions.
 *
 */

namespace java dk.diku.poets.gen.thrift.reporting
include "type.thrift"

// name of a report
typedef string ReportName
// specification of a report (i.e. Haskell code for now)
typedef string ReportSpecification

// The external interface to a report.
struct ReportType {
  1 : type.Type returnType
  2 : list<type.Type> argTypes
}

// Meta information for a report
struct Report {
  1 : ReportName name          // name of the report
  2 : ReportSpecification spec // specification of the report
  3 : string description       // textual description of the report
  4 : set<string> tags         // tags of the report
  5 : ReportType type          // interface to the report
}

// A requested report was not found.
exception ReportNotFoundException {
}

// A report specification is not well-formed.
exception ReportInitException {
  1 : string errorMsg
}