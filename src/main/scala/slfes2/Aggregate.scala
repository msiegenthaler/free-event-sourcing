package slfes2

import shapeless.Coproduct

trait Aggregate { self ⇒
  type Id
  type Event
  type Command <: { type Error <: Coproduct }

  /** Convenience definition for object (xx.Aggregate instead of xx.type). */
  type Aggregate = self.type

  val name: String
}