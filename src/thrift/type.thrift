/*
 * POETS types.
 * 
 * Thrift does not support recursive data types. We therefore encode the AST of
 * POETS types using pointers.
 *
 */

namespace java dk.diku.poets.gen.thrift.type

typedef i32 Index
typedef string RecordName

enum TypeConstant {
  Int = 1,
  Bool = 2,
  String = 3,
  Date = 4,
  Time = 5,
  DateTime = 6,
  Duration = 7,
  Real = 8
}

struct Typ {
  1 : optional TypeConstant typeConstant
  2 : optional RecordName typeRecord
  3 : optional RecordName typeEntity
  4 : optional Index typeList
}

struct Type {
  1 : map<Index,Typ> types
  2 : Index root
}