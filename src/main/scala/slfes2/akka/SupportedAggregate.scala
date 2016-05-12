package slfes2.akka

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import shapeless.Typeable
import simulacrum.typeclass
import slfes2.utils.StringSerializable
import slfes2.{ Aggregate, AggregateImplementation }

/** Typeclass for aggregates that are supported by this akka backend. */
@implicitNotFound("Aggregate ${A} does not seem supported. Needed in the implicit scope are:\n a) AggregateImplementation[${A}]\n b) StringSerializer[${A}#Id]")
@typeclass trait SupportedAggregate[A <: Aggregate] extends AggregateImplementation[A] {
  implicit def idSerializer: StringSerializable[A#Id]
  implicit def typeableCommand: Typeable[Command]
  implicit def typeableId: Typeable[Id]
  implicit def typeableEvent: Typeable[Event]
}
object SupportedAggregate {
  implicit def derive[A <: Aggregate](implicit
    impl: AggregateImplementation[A],
    s: StringSerializable[A#Id],
    ti: Typeable[A#Id],
    te: Typeable[A#Event],
    tc: Typeable[A#Command]): SupportedAggregate[A] = {
    new SupportedAggregate[A] {
      type State = impl.State
      def aggregate = impl.aggregate
      def seed(id: Id) = seed(id)
      def applyEvent(event: A#Event, state: State) = impl.applyEvent(event, state)
      def handleCommand[C <: A#Command](command: C, state: State) = impl.handleCommand(command, state)
      implicit def typeableId = ti
      implicit def typeableEvent = te
      implicit def typeableCommand = tc
      val idSerializer = s
    }
  }
}