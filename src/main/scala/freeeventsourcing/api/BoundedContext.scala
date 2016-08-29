package freeeventsourcing.api

import shapeless.HList

//TODO should this actually be named domain model? the BC might be bigger and include the adapters?
//TODO 'domainmodel' might also be a better package name than 'api'
trait BoundedContext { self ⇒
  val name: String

  type Aggregates <: HList
  val aggregates: Aggregates

  /** Convenience definition for object (xx.BoundedContext instead of xx.type). */
  type BoundedContext = self.type
}