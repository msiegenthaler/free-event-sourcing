package freeeventsourcing

import cats.free.Free
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.support.ValidSelector

object ProcessDefinition {
  def apply[BC <: BoundedContext, S <: WithEventType: ValidSelector[BC, ?], Id](
    in: BC, name: String
  )(
    initiator: S, spawn: S#Event ⇒ Option[Id]
  )(
    body: Id ⇒ ProcessMonad[BC, _]
  ) =
    ???

  type ProcessMonad[BC <: BoundedContext, A] = Free[ProcessAction[BC, ?], A]
}