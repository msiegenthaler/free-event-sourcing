package slfes2

import scala.language.implicitConversions
import scala.collection.immutable.Seq
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

  def aggregate: A
}

object AggregateImplementation {
  def apply[State](
    forAggregate: Aggregate
  )(
    seed: forAggregate.Id ⇒ State,
    applyEvent: (forAggregate.Event, State) ⇒ State,
    handleCommand: CommandHandler[State, forAggregate.Command, forAggregate.Event]
  ): AggregateImplementation[forAggregate.Aggregate] = {
    type S = State
    def seed2 = seed
    def apply2 = applyEvent
    def handle2 = handleCommand
    new AggregateImplementation[forAggregate.Aggregate] {
      type State = S
      def seed(id: Id) = seed2(id)
      def applyEvent(event: forAggregate.Event, state: State) = apply2(event, state)
      def handleCommand[C <: forAggregate.Command](command: C, state: State) = handle2(command, state)
      def aggregate = forAggregate
    }
  }

  trait CommandHandler[State, Command <: AggregateCommand, Event] {
    def apply[C <: Command](command: C, state: State): C#Error Xor Seq[Event]
  }
}