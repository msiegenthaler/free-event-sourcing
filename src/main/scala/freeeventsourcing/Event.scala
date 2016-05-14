package freeeventsourcing

import scala.language.implicitConversions
import scala.collection.immutable.Traversable
import simulacrum.typeclass

//TODO add a way to compare (before, concurrent, after)
trait EventTime

@typeclass trait Event[E] {
  def eventTime(e: E): EventTime
  //TODO maybe a "trace" (cause of the event)
}

@typeclass trait EventSelector[S <: EventSelector.WithEventType] {
  def castEvent(e: Any): S#Event
  def asTag(selector: S): EventTag //TODO Move into akka?
}
object EventSelector {
  type WithEventType = { type Event }

}

//TODO Move into akka
case class EventTag(key: String, value: String)

object EventTagger {
  type EventTagger = Any ⇒ Traversable[EventTag] // TODO find different return type..

  def apply(f: PartialFunction[Any, EventTag]): EventTagger =
    apply(f.lift)
  def apply(f: Any ⇒ Option[EventTag]): EventTagger =
    e ⇒ f(e).toList
  def multi(f: PartialFunction[Any, Traversable[EventTag]]): EventTagger =
    e ⇒ f.lift(e).getOrElse(Nil)
}