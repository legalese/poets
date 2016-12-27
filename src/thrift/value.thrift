
/*
 * POETS values.
 * 
 * Thrift does not support recursive data types. We therefore encode the AST of
 * POETS values using pointers.
 *
 */

namespace java dk.diku.poets.gen.thrift.value

typedef i32 Index
typedef string RecordName
typedef string FieldName

// UTC date value
struct Date {
  1 : i32 year
  2 : i32 month
  3 : i32 day
}

// UTC time value
struct Time {
  1 : i32 hour
  2 : i32 minute
  3 : i32 second
  4 : i32 microsecond
}

// UTC date+time value
struct DateTime {
  1 : Date date,
  2 : Time time
}

// Relative time
struct Duration {
  1 : i32 durationSeconds
  2 : i32 durationMinutes
  3 : i32 durationHours
  4 : i32 durationDays
  5 : i32 durationWeeks
  6 : i32 durationMonths
  7 : i32 durationYears
}

struct Record {
  1 : RecordName recordName
  2 : map<FieldName,Index> fields
}

struct Entity {
  1 : RecordName recordName
  2 : i32 entPointer
}

struct Val {
  1 : optional i32 intVal
  2 : optional bool boolVal
  3 : optional string stringVal
  4 : optional Date dateVal
  5 : optional Time timeVal
  6 : optional DateTime dateTimeVal
  7 : optional Duration durationVal
  8 : optional double realVal
  9 : optional Record recordVal
  10 : optional Entity entVal
  11 : optional list<Index> listVals
}

struct Value {
  1 : map<Index,Val> values
  2 : Index root
}

// Failed to decode a value
exception DecodeException {
  1 : string msg
}

// Input to the server (e.g., entity data) did not type check
exception TypeException {
  1 : string errorMsg
}