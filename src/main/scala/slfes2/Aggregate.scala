package slfes2

import scala.language.implicitConversions
import cats.data.Xor
import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Typeable }
import simulacrum.typeclass
import slfes2.syntax.CoproductCommandHandler.HandleCommand
import slfes2.syntax.CoproductEventApplicator.ApplyEvent
import slfes2.syntax.{ CoproductCommandHandler, CoproductEventApplicator }

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

  implicit def typeableId: Typeable[Id]
  implicit def typeableEvent: Typeable[Event]
  implicit def typeableCommand: Typeable[Command]
}
object AggregateImplementation {
  def apply[State, CE <: Coproduct, CC <: Coproduct](
    aggregate: Aggregate
  )(
    seed: aggregate.Id ⇒ State,
    eventApplicator: CoproductEventApplicator[aggregate.Event, State],
    commandHandler: CoproductCommandHandler[aggregate.Command, aggregate.Event]
  )(implicit
    tI: Typeable[aggregate.Id],
    tE: Typeable[aggregate.Event],
    tC: Typeable[aggregate.Command],
    gE: Generic.Aux[aggregate.Event, CE],
    fE: ApplyEvent[eventApplicator.type, CE, aggregate.Event, State],
    gC: Generic.Aux[aggregate.Command, CC],
    fC: HandleCommand[commandHandler.type, CC, aggregate.Command, aggregate.Command, aggregate.Event]): AggregateImplementation[aggregate.Aggregate] = {
    val seed2 = seed
    type S = State
    new AggregateImplementation[aggregate.Aggregate] {
      type State = S
      def seed(id: Id) = seed2(id)
      def applyEvent(event: aggregate.Event, state: State) = eventApplicator.apply(event, state)
      def handleCommand[C <: aggregate.Command](command: C, state: State) = commandHandler.handle(command)(gC, fC)
      implicit def typeableId = tI
      implicit def typeableEvent = tE
      implicit def typeableCommand = tC
    }
  }
}
