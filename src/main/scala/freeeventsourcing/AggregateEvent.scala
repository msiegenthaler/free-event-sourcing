package freeeventsourcing

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import scala.reflect._
import shapeless.ops.hlist.Selector
import shapeless.{ ::, HList, TypeCase, Typeable }
import simulacrum.typeclass
import freeeventsourcing.utils.{ =!=, CompositeName, StringSerializable }
import freeeventsourcing.utils.StringSerializable.ops._

/** Wraps an event from an aggregate to preserve the information about the source. Is used as the event type of the bus. */
case class AggregateEvent[A <: Aggregate, +E <: A#Event](aggregateType: A, aggregate: A#Id, event: E)(implicit et: AggregateEventType[A, E]) {
  lazy val eventType = et.eventType
}
object AggregateEvent {
  def create[A <: Aggregate, E <: A#Event: AggregateEventType[A, ?]](aggregateType: A)(aggregate: A#Id, event: E) =
    AggregateEvent(aggregateType, aggregate, event)
}

/** String representation of an event type for the aggregate. */
trait AggregateEventType[A <: Aggregate, E <: A#Event] {
  private[freeeventsourcing] def eventType: String
}
object AggregateEventType {
  def apply[A <: Aggregate, E <: A#Event](implicit e: AggregateEventType[A, E]) = e

  implicit def fromClass[A <: Aggregate: ClassTag, E <: A#Event: ClassTag] = new AggregateEventType[A, E] {
    def eventType = {
      //try to shorten it as much as possible to be more resistant to refactorings.
      val aggregateClass = classTag[A].runtimeClass.getName
      def dropAggregatePrefix(name: String) = {
        if (name.startsWith(aggregateClass)) name.drop(aggregateClass.length)
        else name
      }
      def dropCommonEventPrefixes(name: String) = {
        val prefixes = List("Event$", "Events$")
        prefixes.
          filter(name.startsWith).
          take(1).map(p ⇒ name.drop(p.length)).
          headOption.getOrElse(name)
      }
      dropCommonEventPrefixes(dropAggregatePrefix(classTag[E].runtimeClass.getName))
    }
  }
}

case class AggregateEventSelector[A <: Aggregate, E <: A#Event] private (aggregateType: A, aggregate: A#Id, private val topic: EventTopic) {
  type Event = AggregateEvent[A, E]
}

object AggregateEventSelector {
  def apply[A <: Aggregate](tpe: A)(id: A#Id) = new EventCatcher[A](tpe, id)

  class EventCatcher[A <: Aggregate](aggregateType: A, aggregate: A#Id) {
    def apply[E <: A#Event: AggregateEventType[A, ?]](
      implicit
      i: StringSerializable[A#Id],
      ev: ConcreteEvent[A, E]
    ): AggregateEventSelector[A, E] = {
      val topic = topicFor(aggregateType, aggregate, AggregateEventType[A, E].eventType)
      AggregateEventSelector(aggregateType, aggregate, topic)
    }
  }

  implicit def eventSelectorInstance[A <: Aggregate, E <: A#Event: AggregateEventType[A, ?]: Typeable] = {
    new EventSelector[AggregateEventSelector[A, E]] {
      def select(selector: AggregateEventSelector[A, E], event: Any) = event match {
        case AggregateEvent(selector.aggregateType, selector.aggregate, event) ⇒
          Typeable[E].cast(event).map(e ⇒
            AggregateEvent(selector.aggregateType, selector.aggregate, e))
        case _ ⇒ None
      }
      def topic(selector: AggregateEventSelector[A, E]) = selector.topic
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

  object Router {
    def forAggregate[A <: Aggregate: ClassTag](aggregateType: A)(implicit t: Typeable[A#Id], i: StringSerializable[A#Id]) = {
      val Id = TypeCase[A#Id]
      EventRouter {
        case e @ AggregateEvent(`aggregateType`, Id(aggregate), _) ⇒
          topicFor(aggregateType, aggregate, e.eventType)
      }
    }
  }

  private[AggregateEventSelector] def topicFor[A <: Aggregate](aggregateType: A, aggregate: A#Id, eventType: String)(
    implicit
    s: StringSerializable[A#Id]
  ): EventTopic = {
    val name = CompositeName("aggregate") / aggregateType.name / aggregate.serializeToString / eventType
    EventTopic(name)
  }
}