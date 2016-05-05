package slfes2

import shapeless.HList

trait BoundedContext { self ⇒
  val name: String

  type Aggregates <: HList
  val aggregates: Aggregates

  type Selectors <: HList

  /** Convenience definition for object (xx.BoundedContext instead of xx.type). */
  type BoundedContext = self.type
}