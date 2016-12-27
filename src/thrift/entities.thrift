/*
 * Entity store definitions.
 */

namespace java dk.diku.poets.gen.thrift.entities
include "value.thrift"

typedef i32 EntityId

// Entity with specified ID not found
exception EntityNotFound {
  1 : EntityId id
}

// Entity with specified ID deleted
exception EntityDeleted {
  1 : EntityId id
}