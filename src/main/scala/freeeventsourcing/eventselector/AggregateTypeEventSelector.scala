package freeeventsourcing.eventselector

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import freeeventsourcing.eventselector.AggregateEventSelector.ConcreteEvent
import freeeventsourcing._
import freeeventsourcing.support.ValidSelector
import freeeventsourcing.utils.CompositeName
import shapeless.ops.hlist.Selector
import shapeless.{ HList, Typeable }

/** Selector for a type of events across all instances of an aggregate type. */
case class AggregateTypeEventSelector[A <: Aggregate, E <: A#Event] private (aggregateType: A, topic: EventTopic) {
  type Event = AggregateEvent[A, E]
}

object AggregateTypeEventSelector {
  def apply[A <: Aggregate](tpe: A) = new EventCatcher[A](tpe)

  class EventCatcher[A <: Aggregate](aggregateType: A) {
    def apply[E <: A#Event: AggregateEventType[A, ?]](implicit ev: ConcreteEvent[A, E]): AggregateTypeEventSelector[A, E] = {
      val topic = topicFor(aggregateType, AggregateEventType[A, E].eventType)
      AggregateTypeEventSelector(aggregateType, topic)
    }
  }

  implicit def eventSelectorInstance[A <: Aggregate, E <: A#Event: AggregateEventType[A, ?]: Typeable](implicit i: Typeable[A#Id]) = {
    new EventSelector[AggregateTypeEventSelector[A, E]] {
      def select(selector: AggregateTypeEventSelector[A, E], event: Any, metadata: EventMetadata) = event match {
        case AggregateEvent(selector.aggregateType, aggregate, evt) ⇒
          for {
            e ← Typeable[E].cast(evt)
            a ← Typeable[A#Id].cast(aggregate)
          } yield AggregateEvent(selector.aggregateType, a, e)
        case _ ⇒ None
      }
      def topic(selector: AggregateTypeEventSelector[A, E]) = selector.topic
      def startTime(selector: AggregateTypeEventSelector[A, E]) = EventTime.Zero
    }
  }

  @implicitNotFound("${S} is not a valid aggregate type event selector for the aggregates ${Aggregates}")
  sealed trait ValidFor[S, Aggregates <: HList]
  object ValidFor {
    implicit def selector[A <: Aggregate, E <: A#Event, AS <: HList](implicit ev: Selector[AS, A]) =
      new ValidFor[AggregateTypeEventSelector[A, E], AS] {}
  }

  implicit def validSelector[BC <: BoundedContext, S](implicit ev: ValidFor[S, BC#Aggregates]) =
    new ValidSelector[BC, S] {}

  object Router {
    def forAggregateType[A <: Aggregate: ClassTag](aggregateType: A) = EventRouter {
      case e @ AggregateEvent(`aggregateType`, _, _) ⇒ topicFor(aggregateType, e.eventType)
    }
  }

  private[AggregateTypeEventSelector] def topicFor[A <: Aggregate](aggregateType: A, eventType: String): EventTopic = {
    EventTopic(CompositeName("aggregateType") / aggregateType.name / eventType)
  }
}
