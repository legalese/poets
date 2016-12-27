/*
 * Data engine definitions.
 *
 */

namespace java dk.diku.poets.gen.thrift.data
include "value.thrift"
include "type.thrift"

typedef string PCE

struct FieldAttribute {
  1 : optional i32 fieldOrder,
  2 : optional string fieldRestriction
}

struct FieldDefinition {
  1 : value.FieldName fieldName,
  2 : type.Type fieldType,
  3 : set<FieldAttribute> fieldAttributes
}

enum BasicRecordAttribute {
  RecordIsAbstract = 1,
  RecordIsLocked = 2,
  RecordIsHidden = 3
}

struct RecordAttribute {
  1 : optional BasicRecordAttribute basicAttr
}

struct RecordDefinition {
  1 : value.RecordName recordName,
  2 : map<value.FieldName,FieldDefinition> fieldsDefinitions,
  3 : set<value.RecordName> superClasses,
  4 : set<RecordAttribute> recordAttributes
}

// Record name does not exist
exception UndefinedRecord {
  1 : value.RecordName recordName
}

// Uploaded PCE does not validate
exception ValidationError {
  1 : string error
}

// Uploaded PCE does not parse
exception ParseError {
  1 : string error
}