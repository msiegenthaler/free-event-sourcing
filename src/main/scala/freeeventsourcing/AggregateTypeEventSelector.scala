package freeeventsourcing

import scala.reflect.ClassTag
import freeeventsourcing.AggregateEventSelector.ConcreteEvent
import freeeventsourcing.utils.CompositeName
import shapeless.{ TypeCase, Typeable }

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
      def select(selector: AggregateTypeEventSelector[A, E], event: Any) = event match {
        case AggregateEvent(selector.aggregateType, aggregate, event) ⇒
          for {
            e ← Typeable[E].cast(event)
            a ← Typeable[A#Id].cast(aggregate)
          } yield AggregateEvent(selector.aggregateType, a, e)
        case _ ⇒ None
      }
      def topic(selector: AggregateTypeEventSelector[A, E]) = selector.topic
    }
  }

  object Router {
    def forAggregateType[A <: Aggregate: ClassTag](aggregateType: A) = EventRouter {
      case e @ AggregateEvent(`aggregateType`, _, _) ⇒ topicFor(aggregateType, e.eventType)
    }
  }

  private[AggregateTypeEventSelector] def topicFor[A <: Aggregate](aggregateType: A, eventType: String): EventTopic = {
    EventTopic(CompositeName("aggregateType") / aggregateType.name / eventType)
  }
}
