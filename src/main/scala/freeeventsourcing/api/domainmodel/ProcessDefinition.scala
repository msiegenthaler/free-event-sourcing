package freeeventsourcing.api.domainmodel

import cats.free.Free
import freeeventsourcing.api.domainmodel.EventSelector.WithEventType
import freeeventsourcing.api.domainmodel.ProcessDefinition.ProcessMonad
import freeeventsourcing.api.domainmodel.eventselector.ValidSelector

/** Defines a process. A process listens for events within a bounded context
 *  and triggers commands as the reaction to events or an event sequence.
 */
sealed trait ProcessDefinition[DM <: DomainModel] {
  val name: String
  val domainModel: DM

  def initiator: Initiator
  type Initiator <: WithEventType
  implicit def initiatorSelector: ValidSelector[DM, Initiator]

  def body(initialEvent: EventWithMetadata[Initiator#Event]): ProcessMonad[DM, Unit]
}
object ProcessDefinition {
  /** Create a new process definition. */
  def apply[DM <: DomainModel, S <: WithEventType: ValidSelector[DM, ?]](
    in: DM, name_ : String
  )(initiator_ : S)(body_ : EventWithMetadata[S#Event] ⇒ ProcessMonad[DM, _]) = {
    new ProcessDefinition[DM] {
      val name = name_
      val domainModel = in
      val initiator = initiator_
      type Initiator = S
      val initiatorSelector = implicitly[ValidSelector[DM, S]]
      def body(initialEvent: EventWithMetadata[Initiator#Event]) = body(initialEvent)
    }
  }

  type ProcessMonad[DM <: DomainModel, A] = Free[ProcessAction[DM, ?], A]
}