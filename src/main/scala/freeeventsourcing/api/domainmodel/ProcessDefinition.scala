package freeeventsourcing.api.domainmodel

import cats.free.Free
import freeeventsourcing.api.domainmodel.EventSelector.WithEventType
import freeeventsourcing.api.domainmodel.ProcessDefinition.ProcessMonad
import freeeventsourcing.api.domainmodel.eventselector.ValidSelector

/** Defines a process. A process listens for events within a bounded context
 *  and triggers commands as the reaction to events or an event sequence.
 */
sealed trait ProcessDefinition[BC <: BoundedContext] {
  val name: String
  val boundedContext: BC

  def initiator: Initiator
  type Initiator <: WithEventType
  implicit def initiatorSelector: ValidSelector[BC, Initiator]

  def body(initialEvent: EventWithMetadata[Initiator#Event]): ProcessMonad[BC, Unit]
}
object ProcessDefinition {
  /** Create a new process definition. */
  def apply[BC <: BoundedContext, S <: WithEventType: ValidSelector[BC, ?]](
    in: BC, name_ : String
  )(initiator_ : S)(body_ : EventWithMetadata[S#Event] ⇒ ProcessMonad[BC, _]) = {
    new ProcessDefinition[BC] {
      val name = name_
      val boundedContext = in
      val initiator = initiator_
      type Initiator = S
      val initiatorSelector = implicitly[ValidSelector[BC, S]]
      def body(initialEvent: EventWithMetadata[Initiator#Event]) = body(initialEvent)
    }
  }

  type ProcessMonad[BC <: BoundedContext, A] = Free[ProcessAction[BC, ?], A]
}