package freeeventsourcing.api

import shapeless.HList

trait BoundedContext { self ⇒
  val name: String

  type Aggregates <: HList
  val aggregates: Aggregates

  /** Convenience definition for object (xx.BoundedContext instead of xx.type). */
  type BoundedContext = self.type
}