package freeeventsourcing

import java.time.Instant
import cats.free.Free
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.Process.ProcessMonad
import freeeventsourcing.ProcessAction.FirstOf.Alternatives
import freeeventsourcing.support.{ ValidAggregate, ValidSelector }
import shapeless.{ :+:, CNil, Coproduct }

object Process {
  type ProcessMonad[BC <: BoundedContext, A] = Free[ProcessAction[BC, ?], A]
}

/** A 'step' of a process. Is used inside the free monad (see ProcessMonad). */
sealed trait ProcessAction[BC <: BoundedContext, +A]
object ProcessAction {

  /** Wait for something to happen. */
  sealed trait Await[BC <: BoundedContext, +A] extends ProcessAction[BC, A]

  /** Wait for a selector to match an event. */
  case class AwaitEvent[BC <: BoundedContext, S <: WithEventType: EventSelector](selector: S)(
    implicit
    ev: ValidSelector[BC, S]
  ) extends Await[BC, S#Event]

  /** Wait until the specified instant (date/time). */
  case class WaitUntil[BC <: BoundedContext](when: Instant) extends Await[BC, Unit]

  /** Wait until the first of the alternatives (all of type Await) happens. */
  case class FirstOf[BC <: BoundedContext, Alt <: Alternatives[BC]](alternatives: Alt) extends Await[BC, Alt#Events]
  object FirstOf {
    /** Specialized Coproduct that contains Awaits and keeps track of the resulting event coproduct. */
    sealed trait Alternatives[BC <: BoundedContext] {
      type Events <: Coproduct
    }
    case class Empty[BC <: BoundedContext]() extends Alternatives[BC] {
      type Events = CNil
    }
    case class Alternative[BC <: BoundedContext, A, T <: Alternatives[BC]](await: Await[BC, A], more: T) extends Alternatives[BC] {
      type Events = A :+: T#Events
    }
  }

  /** Execute a command on an aggregate */
  case class Execute[BC <: BoundedContext, A <: Aggregate, Cmd <: A#Command](
    aggregateType: A, aggregate: A#Id, command: A#Command, errorHandler: Cmd#Error ⇒ ProcessMonad[BC, Unit]
  )(implicit ev: ValidAggregate[BC, A])
      extends ProcessAction[BC, Unit]

  /** Terminate the process */
  case class End[BC <: BoundedContext]() extends ProcessAction[BC, Unit]
}