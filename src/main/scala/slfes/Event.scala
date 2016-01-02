package slfes

import shapeless.ops.coproduct.{Selector, Inject}

case class EventMetadata[A <: Aggregate](from: A#Id, sequence: Long, aggregate: A)

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