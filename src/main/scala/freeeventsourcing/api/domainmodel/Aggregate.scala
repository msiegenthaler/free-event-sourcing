package freeeventsourcing.api.domainmodel

import freeeventsourcing.utils.ADT
import shapeless.Coproduct

trait Aggregate { self ⇒
  type Id
  type Event <: DomainEvent
  type Command <: AggregateCommand

  /** Convenience definition for object (xx.Aggregate instead of xx.type). */
  type Aggregate = self.type

  val name: String
}

trait AggregateCommand extends ADT { type Error <: Coproduct }