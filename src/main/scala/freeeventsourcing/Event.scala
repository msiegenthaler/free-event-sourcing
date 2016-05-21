package freeeventsourcing

import scala.language.implicitConversions
import scala.collection.immutable.Traversable
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
  def castEvent(e: Any): S#Event

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

object EventTagger {
  type EventTagger = Any ⇒ Traversable[EventTopic] // TODO find different return type..

  def apply(f: PartialFunction[Any, EventTopic]): EventTagger =
    apply(f.lift)
  def apply(f: Any ⇒ Option[EventTopic]): EventTagger =
    e ⇒ f(e).toList
  def multi(f: PartialFunction[Any, Traversable[EventTopic]]): EventTagger =
    e ⇒ f.lift(e).getOrElse(Nil)
}