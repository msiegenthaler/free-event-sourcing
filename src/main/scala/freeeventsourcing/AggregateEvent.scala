package freeeventsourcing

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import scala.reflect._
import shapeless.ops.hlist.Selector
import shapeless.{ ::, HList, Typeable }
import simulacrum.typeclass
import freeeventsourcing.utils.{ =!=, CompositeName, StringSerializable }
import freeeventsourcing.utils.StringSerializable.ops._

case class AggregateEvent[A <: Aggregate](aggregateType: A, aggregate: A#Id, event: A#Event, eventTime: EventTime)
object AggregateEvent {
  implicit def eventInstance[A <: Aggregate] = new Event[AggregateEvent[A]] {
    def eventTime(e: AggregateEvent[A]) = e.eventTime
  }
}

@typeclass trait AggregateEventType[E] {
  def eventType: String
}
object AggregateEventType {
  //TODO temporary, this is not the most robust solution (refactorings)
  implicit def fromClass[E: ClassTag] = new AggregateEventType[E] {
    def eventType = classTag[E].runtimeClass.getName
  }
}

case class AggregateEventSelector[A <: Aggregate, E <: A#Event](aggregateType: A, aggregate: A#Id, eventType: String)(
    implicit
    idser: StringSerializable[A#Id]
) {
  type Event = E
  def topic = {
    val key = CompositeName("aggregate") / aggregateType.name / aggregate.serializeToString / eventType
    EventTopic(key)
  }
}
object AggregateEventSelector {
  def apply[A <: Aggregate](tpe: A)(id: A#Id) = new EventCatcher[A](tpe, id)

  implicit def eventSelectorInstance[A <: Aggregate, E <: A#Event: Typeable](implicit s: StringSerializable[A#Id]) = new EventSelector[AggregateEventSelector[A, E]] {
    def select(selector: AggregateEventSelector[A, E], e: Any) = Typeable[E].cast(e)
    def topic(selector: AggregateEventSelector[A, E]) = selector.topic
  }

  class EventCatcher[A <: Aggregate](aggregateType: A, aggregate: A#Id) {
    def apply[E <: A#Event](
      implicit
      et: AggregateEventType[E],
      idser: StringSerializable[A#Id],
      notBaseType: ConcreteEvent[A, E]
    ): AggregateEventSelector[A, E] = {
      AggregateEventSelector[A, E](aggregateType, aggregate, et.eventType)
    }
  }

  // Just so we get nicer error messages
  @implicitNotFound("Aggregates ${A} event base type (${E}) cannot be selected for. Please use a concrete event type (one of the subclasses of ${E}).")
  sealed trait ConcreteEvent[A <: Aggregate, E]
  object ConcreteEvent {
    implicit def instance[A <: Aggregate, E](implicit ev: E =!= A#Event) = new ConcreteEvent[A, E] {}
  }

  @implicitNotFound("${S} is not a valid aggregate event selector for the aggregates ${Aggregates}")
  sealed trait ValidFor[S, Aggregates <: HList]
  object ValidFor {
    implicit def selector[A <: Aggregate, E <: A#Event, AS <: HList](implicit ev: Selector[AS, A]) =
      new ValidFor[AggregateEventSelector[A, E], AS] {}
  }
}

object AggregateEventRouter {
  def apply[A <: Aggregate](aggregate: A)(implicit t: Typeable[AggregateEvent[A]], idser: StringSerializable[A#Id]) = EventRouter { event ⇒
    t.cast(event).filter(_.aggregateType == aggregate).map { event ⇒
      val eventType = event.event.getClass.getName //TODO change to more robust implementation
      val selector = AggregateEventSelector(aggregate, event.aggregate, eventType)
      selector.topic
    }
  }
}