package freeeventsourcing

import scala.language.implicitConversions
import scala.collection.immutable.Set
import freeeventsourcing.EventSelector.ops._
import freeeventsourcing.utils.CompositeName
import simulacrum.typeclass

/** Selects a specific subset of events. */
@typeclass trait EventSelector[S <: EventSelector.WithEventType] {
  /** Checks if the event received on the topic matches this selector. */
  def select(selector: S, event: Any, metadata: EventMetadata): Option[S#Event]

  /** Topics are used for more efficient filtering of events.
   *  Only events on this topic will be delivered to the selector's select method.
   */
  def topic(selector: S): EventTopic

  /** Only events after this time are relevant to the selector. */
  def startTime(selector: S): EventTime

  /** Only match events that occured after the (casual) time specified.
   *  Note that in a concurrent, distributed system the notation of 'after' is not the same as the regular wall clock.
   *  See EventTime for further details.
   */
  def after(selector: S, time: EventTime)(implicit s: EventSelector[S]): EventSelector.AfterSelector[S] =
    EventSelector.AfterSelector(selector, selector.startTime.later(time))

  /** Apply an additional filter/transformation to the event selector. */
  def where[E](selector: S, predicate: (S#Event, EventMetadata) ⇒ Option[E]): EventSelector.FilteredSelector[S, E] =
    EventSelector.FilteredSelector(selector, predicate)
}
object EventSelector {
  type WithEventType = { type Event }

  case class FilteredSelector[S <: WithEventType, E](base: S, predicate: (S#Event, EventMetadata) ⇒ Option[E]) {
    type Event = E
  }
  object FilteredSelector {
    implicit def eventSelector[S <: WithEventType: EventSelector, E]: EventSelector[FilteredSelector[S, E]] = {
      new EventSelector[FilteredSelector[S, E]] {
        def select(selector: FilteredSelector[S, E], event: Any, metadata: EventMetadata) =
          selector.base.select(event, metadata).flatMap(e ⇒ selector.predicate(e, metadata))
        def topic(selector: FilteredSelector[S, E]) = selector.base.topic
        def startTime(selector: FilteredSelector[S, E]) = selector.base.startTime
      }
    }
  }

  case class AfterSelector[S <: WithEventType](base: S, startTime: EventTime) {
    type Event = S#Event
  }
  object AfterSelector {
    implicit def eventSelector[S <: WithEventType: EventSelector]: EventSelector[AfterSelector[S]] = {
      new EventSelector[AfterSelector[S]] {
        def select(selector: AfterSelector[S], event: Any, metadata: EventMetadata) = {
          if (metadata.time after startTime(selector)) selector.base.select(event, metadata)
          else None
        }
        def startTime(selector: AfterSelector[S]) = selector.startTime
        def topic(selector: AfterSelector[S]) = selector.base.topic
      }
    }
  }
}

/** Topic for specific class of events. Ie all events from a certain aggregate type or for a specific aggregate. */
case class EventTopic(composedTopic: CompositeName) extends AnyVal {
  def topic = composedTopic.serialize
}

/** Responsible for routing events to the correct topics. */
object EventRouter {
  type EventRouter = EventWithMetadata[Any] ⇒ Set[EventTopic]

  def apply(f: PartialFunction[Any, EventTopic]): EventRouter =
    option(f.lift)
  def option(f: Any ⇒ Option[EventTopic]): EventRouter =
    e ⇒ f(e.payload).toSet
  def multi(f: PartialFunction[Any, Set[EventTopic]]): EventRouter =
    e ⇒ f.lift(e.payload).getOrElse(Set.empty)
}