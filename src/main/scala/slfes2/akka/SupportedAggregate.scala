package slfes2.akka

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import simulacrum.typeclass
import slfes.utils.StringSerializable
import slfes2.accountprocessing.{ Account, Transaction }
import slfes2.{ Aggregate, AggregateImplementation }

/** Typeclass for aggregates that are supported by this akka backend. */
@implicitNotFound("Aggregate ${A} does not seem supported. Needed in the implicit scope are:\n a) AggregateImplementation[${A}]\n b) StringSerializer[${A}#Id]")
@typeclass trait SupportedAggregate[A <: Aggregate] extends AggregateImplementation[A] {
  implicit def idSerializer: StringSerializable[A#Id]
}
object SupportedAggregate {
  implicit def derive[A <: Aggregate](implicit impl: AggregateImplementation[A], s: StringSerializable[A#Id]): SupportedAggregate[A] = {
    new SupportedAggregate[A] {
      type State = impl.State
      def aggregate = impl.aggregate
      def seed(id: Id) = seed(id)
      def applyEvent(event: A#Event, state: State) = impl.applyEvent(event, state)
      def handleCommand[C <: A#Command](command: C, state: State) = impl.handleCommand(command, state)
      implicit def typeableId = impl.typeableId
      implicit def typeableEvent = impl.typeableEvent
      implicit def typeableCommand = impl.typeableCommand
      val idSerializer = s
    }
  }
}