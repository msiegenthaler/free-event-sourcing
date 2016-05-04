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

case class SerializedEventSelector(selectorType: String, value: String)
@typeclass trait EventSelector[S] {
  def serialize(selector: S): SerializedEventSelector
}

object EventIndexer {
  type EventIndexer = Any ⇒ Traversable[SerializedEventSelector]

  def apply(f: PartialFunction[Any, SerializedEventSelector]): EventIndexer =
    apply(f.lift)
  def apply(f: Any ⇒ Option[SerializedEventSelector]): EventIndexer =
    e ⇒ f(e).toList
  def multi(f: PartialFunction[Any, Traversable[SerializedEventSelector]]): EventIndexer =
    e ⇒ f.lift(e).getOrElse(Nil)
}