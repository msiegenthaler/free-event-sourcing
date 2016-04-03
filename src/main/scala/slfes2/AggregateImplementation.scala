package slfes2

import scala.language.implicitConversions
import cats.data.Xor
import shapeless.{ Coproduct, Generic, Typeable }
import simulacrum.typeclass

@typeclass trait AggregateImplementation[A <: Aggregate] {
  type Id = A#Id
  type Event = A#Event
  type Command = A#Command
  type State
  def handleCommand[C <: Command](command: C, state: State): C#Error Xor Seq[Event]
  def applyEvent(event: Event, state: State): State
  def seed(id: Id): State

  implicit def typeableId: Typeable[Id]
  implicit def typeableEvent: Typeable[Event]
  implicit def typeableCommand: Typeable[Command]
}

object AggregateImplementation {
  def apply[State](
    aggregate: Aggregate
  )(
    seed: aggregate.Id ⇒ State,
    applyEvent: (aggregate.Event, State) ⇒ State,
    handleCommand: CommandHandler[State, aggregate.Command, aggregate.Event]
  )(implicit
    tI: Typeable[aggregate.Id],
    tE: Typeable[aggregate.Event],
    tC: Typeable[aggregate.Command]): AggregateImplementation[aggregate.Aggregate] = {
    type S = State
    def seed2 = seed
    def apply2 = applyEvent
    def handle2 = handleCommand
    new AggregateImplementation[aggregate.Aggregate] {
      type State = S
      def seed(id: Id) = seed2(id)
      def applyEvent(event: aggregate.Event, state: State) = apply2(event, state)
      def handleCommand[C <: aggregate.Command](command: C, state: State) = handle2(command, state)
      implicit def typeableId = tI
      implicit def typeableEvent = tE
      implicit def typeableCommand = tC
    }
  }

  trait CommandHandler[State, Command <: { type Error <: Coproduct }, Event] {
    def apply[C <: Command](command: C, state: State): C#Error Xor Seq[Event]
  }
}
