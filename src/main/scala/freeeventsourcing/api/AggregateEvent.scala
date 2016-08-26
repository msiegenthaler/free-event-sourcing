package freeeventsourcing.api

import scala.reflect._

/** Wraps an event from an aggregate to preserve the information about the source. Is used as the event type of the bus. */
case class AggregateEvent[A <: Aggregate, +E <: A#Event](aggregateType: A, aggregate: A#Id, event: E)(implicit et: AggregateEventType[A, E]) {
  lazy val eventType = et.eventType
}
object AggregateEvent {
  def create[A <: Aggregate, E <: A#Event: AggregateEventType[A, ?]](aggregateType: A)(aggregate: A#Id, event: E) =
    AggregateEvent(aggregateType, aggregate, event)
}

/** String representation of an event type for the aggregate. */
trait AggregateEventType[A <: Aggregate, E <: A#Event] {
  private[freeeventsourcing] def eventType: String
}
object AggregateEventType {
  def apply[A <: Aggregate, E <: A#Event](implicit e: AggregateEventType[A, E]) = e

  implicit def fromClass[A <: Aggregate: ClassTag, E <: A#Event: ClassTag] = new AggregateEventType[A, E] {
    def eventType = {
      //try to shorten it as much as possible to be more resistant to refactorings.
      val aggregateClass = classTag[A].runtimeClass.getName
      def dropAggregatePrefix(name: String) = {
        if (name.startsWith(aggregateClass)) name.drop(aggregateClass.length)
        else name
      }
      def dropCommonEventPrefixes(name: String) = {
        val prefixes = List("Event$", "Events$")
        prefixes.
          filter(name.startsWith).
          take(1).map(p ⇒ name.drop(p.length)).
          headOption.getOrElse(name)
      }
      dropCommonEventPrefixes(dropAggregatePrefix(classTag[E].runtimeClass.getName))
    }
  }
}