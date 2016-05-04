package slfes2

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import scala.reflect._
import simulacrum.typeclass
import slfes.utils.=!=

case class AggregateEvent[A <: Aggregate](aggregateType: A, aggregate: A#Id, event: A#Event)

@typeclass trait AggregateEventType[E] {
  def eventType: String
}
object AggregateEventType {
  //TODO temporary, this is not the most robust solution (refactorings)
  implicit def fromClass[E: ClassTag] = new AggregateEventType[E] {
    def eventType = classTag[E].runtimeClass.getName
  }
}

case class AggregateEventSelector[A <: Aggregate](aggregateType: A, aggregate: A#Id, eventType: String)
object AggregateEventSelector {
  def apply[A <: Aggregate](tpe: A)(id: A#Id) = new EventCatcher[A](tpe, id)

  class EventCatcher[A <: Aggregate](aggregateType: A, aggregate: A#Id) {
    def apply[E <: A#Event](implicit et: AggregateEventType[E], notBaseType: ConcreteEvent[A, E]): AggregateEventSelector[A] = {
      AggregateEventSelector[A](aggregateType, aggregate, et.eventType)
    }
  }

  // Just so we get nicer error messages
  @implicitNotFound("Aggregates ${A} event base type (${E}) cannot be selected for. Please use a concrete event type (one of the subclasses of ${E}).")
  sealed trait ConcreteEvent[A <: Aggregate, E]
  object ConcreteEvent {
    implicit def instance[A <: Aggregate, E](implicit ev: E =!= A#Event) = new ConcreteEvent[A, E] {}
  }
}
