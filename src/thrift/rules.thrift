/*
 * Rules definitions.
 *
 */

namespace java dk.diku.poets.gen.thrift.rules
include "value.thrift"

struct QVar {
  1 : string id
}

struct Unknown {
  1 : string id
}

struct QVal {
  1 : optional i32 intVal
  2 : optional bool boolVal
  3 : optional string stringVal
  4 : optional value.Date dateVal
  5 : optional value.Time timeVal
  6 : optional value.DateTime dateTimeVal
  7 : optional value.Duration durationVal
  8 : optional double realVal
  9 : optional value.Record recordVal
 10 : optional value.Entity entityVal
 11 : optional list<value.Index> listVals
 12 : optional QVar qVarVal
 13 : optional Unknown unknownVal
}

struct QValue {
  1 : map<value.Index,QVal> values
  2 : value.Index root
}

exception Inconsistent { /* The (partial) value provided is inconsistent with the rule set. */
  1 : string errMsg
}

exception NoRuleSets { /* The engine has no rule sets, to which the query in question can be applied. */
  1 : string errMsg
}

exception CannotComplete { /* The rule engine cannot answer the query (no rules apply). */
  1 : string errMsg
}

exception PleaseSpecify { /* The rule engine needs additional information. */
  1 : list<QValue> toSpecify
}