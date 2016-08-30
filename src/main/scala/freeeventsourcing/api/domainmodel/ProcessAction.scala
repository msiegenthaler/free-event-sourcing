package freeeventsourcing.api.domainmodel

import java.time.Instant
import freeeventsourcing.api.domainmodel.EventSelector.WithEventType
import freeeventsourcing.api.domainmodel.ProcessAction.FirstOf.Alternatives
import freeeventsourcing.api.domainmodel.ProcessDefinition.ProcessMonad
import freeeventsourcing.api.domainmodel.eventselector.ValidSelector
import freeeventsourcing.utils.ADT
import shapeless.{ :+:, CNil, Coproduct }

/** A 'step' of a process. Is used inside the free monad (see ProcessMonad). */
sealed trait ProcessAction[DM <: DomainModel, +A] extends ADT
object ProcessAction {

  /** Wait for something to happen. */
  sealed trait Await[DM <: DomainModel, +A] extends ProcessAction[DM, A]

  /** Wait for a selector to match an event. */
  final case class AwaitEvent[DM <: DomainModel, S <: WithEventType](selector: S)(
    implicit
    val eventSelector: EventSelector[S], ev: ValidSelector[DM, S]
  ) extends Await[DM, EventWithMetadata[S#Event]]

  /** Wait until the specified instant (date/time). */
  final case class WaitUntil[DM <: DomainModel](when: Instant) extends Await[DM, Unit]

  /** Wait until the first of the alternatives (all of type Await) happens. */
  final case class FirstOf[DM <: DomainModel, Alt <: Alternatives[DM]](alternatives: Alt) extends Await[DM, Alt#Events]
  object FirstOf {
    /** Specialized Coproduct that contains Awaits and keeps track of the resulting event coproduct. */
    sealed trait Alternatives[DM <: DomainModel] extends ADT {
      type Events <: Coproduct
    }
    final case class Empty[DM <: DomainModel]() extends Alternatives[DM] {
      type Events = CNil
    }
    final case class Alternative[DM <: DomainModel, A, T <: Alternatives[DM]](await: Await[DM, A], more: T) extends Alternatives[DM] {
      type Events = A :+: T#Events
    }
  }

  /** Execute a command on an aggregate */
  final case class Execute[DM <: DomainModel, A <: Aggregate, Cmd <: A#Command](
    aggregateType: A, aggregate: A#Id, command: A#Command, errorHandler: Cmd#Error ⇒ ProcessMonad[DM, Unit]
  )(implicit ev: ValidAggregate[DM, A])
      extends ProcessAction[DM, Unit] {
    type Command = Cmd
    type Error = Cmd#Error
  }

  /** Terminate the process */
  final case class End[DM <: DomainModel]() extends ProcessAction[DM, Unit]
}