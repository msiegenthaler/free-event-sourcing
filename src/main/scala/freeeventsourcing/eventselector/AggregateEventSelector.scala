package freeeventsourcing.eventselector

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import freeeventsourcing._
import freeeventsourcing.support.ValidSelector
import freeeventsourcing.utils.StringSerializable.ops._
import freeeventsourcing.utils.{ =!=, CompositeName, StringSerializable }
import shapeless.ops.hlist.Selector
import shapeless.{ HList, TypeCase, Typeable }

/** Selector for a specific type of events on an aggregate instance */
case class AggregateEventSelector[A <: Aggregate, E <: A#Event] private (aggregateType: A, aggregate: A#Id, topic: EventTopic) {
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
      def select(selector: AggregateEventSelector[A, E], event: Any, metadata: EventMetadata) = event match {
        case AggregateEvent(selector.aggregateType, selector.aggregate, evt) ⇒
          Typeable[E].cast(evt).map(e ⇒
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

  implicit def validSelector[BC <: BoundedContext, S](implicit ev: ValidFor[S, BC#Aggregates]) =
    new ValidSelector[BC, S] {}

  object Router {
    def forAggregateType[A <: Aggregate: ClassTag](aggregateType: A)(implicit t: Typeable[A#Id], i: StringSerializable[A#Id]) = {
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
