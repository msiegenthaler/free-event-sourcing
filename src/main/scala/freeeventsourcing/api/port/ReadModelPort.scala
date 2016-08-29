package freeeventsourcing.api.port

import freeeventsourcing.api.ReadModel

/** Implements a read model. */
trait ReadModelPort[RM <: ReadModel] {
  //TODO this is a special type of port.. it actually needs to implement the read model trait
  //TODO we might need to treat this differently.. because we only allow one impl per read model
  //TODO probably easier to just contribute them when the bc is constructed
}
