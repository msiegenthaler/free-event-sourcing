package freeeventsourcing

import cats.free.Free
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.ProcessDefinition.ProcessMonad
import freeeventsourcing.support.ValidSelector

/** Defines a process. A process listens for events within a bounded context
 *  and triggers commands as the reaction to events or an event sequence.
 */
sealed trait ProcessDefinition[BC <: BoundedContext] {
  val name: String
  type Id
  val boundedContext: BC

  def initiator: Initiator
  type Initiator <: WithEventType
  implicit def initiatorSelector: ValidSelector[BC, Initiator]

  def spawn(event: Initiator#Event): Option[Id]

  def body(id: Id): ProcessMonad[BC, _]
}
object ProcessDefinition {
  /** Create a new process definition. */
  def apply[BC <: BoundedContext, S <: WithEventType: ValidSelector[BC, ?], I](
    in: BC, name_ : String
  )(initiator_ : S, spawn_ : S#Event ⇒ Option[I])(
    body_ : I ⇒ ProcessMonad[BC, _]
  ) = {
    new ProcessDefinition[BC] {
      val name = name_
      type Id = I
      val boundedContext = in
      val initiator = initiator_
      type Initiator = S
      val initiatorSelector = implicitly[ValidSelector[BC, S]]
      def spawn(event: S#Event) = spawn_(event)
      def body(id: Id) = body_(id)
    }
  }

  type ProcessMonad[BC <: BoundedContext, A] = Free[ProcessAction[BC, ?], A]
}