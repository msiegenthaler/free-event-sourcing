package freeeventsourcing.api.domainmodel

import shapeless.HList

trait DomainModel { self ⇒
  val name: String

  type Aggregates <: HList
  val aggregates: Aggregates

  /** Convenience definition for object (xx.BoundedContext instead of xx.type). */
  type BoundedContext = self.type
}