package freeeventsourcing

import scala.language.implicitConversions
import scala.collection.immutable.Set
import freeeventsourcing.utils.CompositeName
import simulacrum.typeclass

//TODO add a way to compare (before, concurrent, after)
trait EventTime

@typeclass trait Event[E] {
  def eventTime(e: E): EventTime
  //TODO maybe a "trace" (cause of the event)
}

/** Selects a specific subset of events. */
@typeclass trait EventSelector[S <: EventSelector.WithEventType] {
  /** Checks if the event received on the topic matches this selector. */
  def select(event: Any): Option[S#Event]

  /** Topics are used for more efficient filtering of events. Only events on this topic will be delivered to the selector. */
  def topic(selector: S): EventTopic
}
object EventSelector {
  type WithEventType = { type Event }
}

/** Topic for specific class of events. Ie all events from a certain aggregate type or for a specific aggregate. */
case class EventTopic(composedTopic: CompositeName) extends AnyVal {
  def topic = composedTopic.serialize
}

/** Responsible for routing events to the correct topics. */
object EventRouter {
  type EventRouter = Any ⇒ Set[EventTopic]

  def apply(f: PartialFunction[Any, EventTopic]): EventRouter =
    apply(f.lift)
  def apply(f: Any ⇒ Option[EventTopic]): EventRouter =
    e ⇒ f(e).toSet
  def multi(f: PartialFunction[Any, Set[EventTopic]]): EventRouter =
    e ⇒ f.lift(e).getOrElse(Set.empty)
}