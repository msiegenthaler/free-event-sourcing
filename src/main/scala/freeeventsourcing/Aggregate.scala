package freeeventsourcing

import shapeless.Coproduct

trait Aggregate { self ⇒
  type Id
  type Event
  type Command <: AggregateCommand

  /** Convenience definition for object (xx.Aggregate instead of xx.type). */
  type Aggregate = self.type

  val name: String
}

trait AggregateCommand { type Error <: Coproduct }