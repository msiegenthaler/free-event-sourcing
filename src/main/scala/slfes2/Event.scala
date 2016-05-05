package slfes2

import scala.language.implicitConversions
import scala.collection.immutable.Traversable
import simulacrum.typeclass

trait EventTime {
  //TODO compareable (before, concurrent, after)
}

@typeclass trait Event[E] {
  def eventTime(e: E): EventTime
  //TODO maybe a "trace" (cause of the event)
}

case class EventTag(key: String, value: String)
@typeclass trait EventSelector[S] {
  def asTag(selector: S): EventTag
}

object EventIndexer {
  type EventIndexer = Any ⇒ Traversable[EventTag]

  def apply(f: PartialFunction[Any, EventTag]): EventIndexer =
    apply(f.lift)
  def apply(f: Any ⇒ Option[EventTag]): EventIndexer =
    e ⇒ f(e).toList
  def multi(f: PartialFunction[Any, Traversable[EventTag]]): EventIndexer =
    e ⇒ f.lift(e).getOrElse(Nil)
}