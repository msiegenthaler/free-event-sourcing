package slfes2

import scala.language.implicitConversions
import cats.data.Xor
import shapeless.Coproduct
import simulacrum.typeclass

trait Aggregate { self ⇒
  type Id
  type Event
  type Command <: { type Error <: Coproduct }

  /** Convenience definition for object (xx.Aggregate instead of xx.type). */
  type Aggregate = self.type

  val name: String
}

@typeclass trait AggregateImplementation[A <: Aggregate] {
  type Id = A#Id
  type Event = A#Event
  type Command = A#Command
  type State
  def handleCommand[C <: Command](command: C, state: State): C#Error Xor Seq[Event]
  def applyEvent(event: Event, state: State): State
  def seed(id: Id): State
}
