package freeeventsourcing

import java.time.Instant
import cats.free.Free
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.support.{ ValidAggregate, ValidSelector }
import shapeless.{ :+:, CNil, Coproduct }

//TODO do we need the encapsulating object?
object Process {
  type ProcessMonad[BC <: BoundedContext, A] = Free[ProcessAction[BC, ?], A]

  //TODO rename to action?
  sealed trait ProcessAction[BC <: BoundedContext, +A]
  object ProcessAction {
    sealed trait Await[BC <: BoundedContext, +A] extends ProcessAction[BC, A]
    case class AwaitEvent[BC <: BoundedContext, S <: WithEventType: EventSelector](selector: S)(
      implicit
      ev: ValidSelector[BC, S]
    ) extends Await[BC, S#Event]
    case class WaitUntil[BC <: BoundedContext](when: Instant) extends Await[BC, Unit]
    case class FirstOf[BC <: BoundedContext, Alt <: Alternatives[BC]](alternatives: Alt) extends Await[BC, Alt#Events]

    case class Execute[BC <: BoundedContext, A <: Aggregate, Cmd <: A#Command](
      aggregateType: A, aggregate: A#Id, command: A#Command, errorHandler: Cmd#Error ⇒ ProcessMonad[BC, Unit]
    )(
      implicit
      ev: ValidAggregate[BC, A]
    ) extends ProcessAction[BC, Unit]

    case class End[BC <: BoundedContext]() extends ProcessAction[BC, Unit]

    /** Specialized Coproduct that contains Awaits and keeps track of the resulting event coproduct. */
    sealed trait Alternatives[BC <: BoundedContext] {
      type Events <: Coproduct
    }
    object Alternatives {
      case class No[BC <: BoundedContext]() extends Alternatives[BC] {
        type Events = CNil
      }
      case class Alternative[BC <: BoundedContext, A, T <: Alternatives[BC]](await: Await[BC, A], more: T) extends Alternatives[BC] {
        type Events = A :+: T#Events
      }
    }
  }
}