package slfes

import shapeless.{ :+:, CNil, Coproduct, Inl, Inr }

import scala.language.implicitConversions
import shapeless.ops.coproduct.{ Inject, Selector }
import simulacrum.typeclass

case class EventType(name: String) extends AnyVal

@typeclass trait EventWithType[E] {
  def eventType(event: E): EventType
}
object EventWithType extends LowPriorityTypedEvent {
  implicit def coproduct[H: EventWithType, T <: Coproduct: EventWithType] = new EventWithType[H :+: T] {
    def eventType(event: H :+: T) = event match {
      case Inl(h) ⇒ EventWithType[H].eventType(h)
      case Inr(t) ⇒ EventWithType[T].eventType(t)
    }
  }
  implicit def coproductCNil = new EventWithType[CNil] {
    def eventType(event: CNil) = throw new IllegalArgumentException("Empty coproduct")
  }
}
trait LowPriorityTypedEvent {
  implicit def product[E <: Product] = new EventWithType[E] {
    def eventType(event: E) = EventType(event.productPrefix)
  }
}

case class EventMetadata[A <: Aggregate](from: A#Id, sequence: Long, eventType: EventType, aggregate: A)

case class Evt[E, A <: Aggregate](event: E, metadata: EventMetadata[A])(implicit ev: Evt.EvtCreate) {
  type Event = E
  type Events = A#Event
  type Aggregate = A
  def from = metadata.from
  def sequence = metadata.sequence
}
object Evt {
  /** Restricts creation of the case class to here. */
  sealed trait EvtCreate
  implicit private object EvtCreateInstance extends EvtCreate

  def specific[E, A <: Aggregate](evt: E, meta: EventMetadata[A])(implicit i: Selector[A#Event, E]): Evt[E, A] = Evt[E, A](evt, meta)
  def generic[E, A <: Aggregate](evt: E, meta: EventMetadata[A])(implicit ev: A#Event =:= E): Evt[E, A] = Evt[E, A](evt, meta)

  def select[E, A <: Aggregate](e: AggregateEvt[A])(implicit s: Selector[A#Event, E]): Option[Evt[E, A]] =
    s(e.event).map(event ⇒ Evt(event, e.metadata))
}