package slfes2

import java.time.Instant
import slfes2.EventSelector.WithEventType

object Process {

  sealed trait ProcessAction[+A]
  object ProcessAction {
    sealed trait Await[+A] extends ProcessAction[A]
    case class AwaitEvent[S <: WithEventType](selector: S)(implicit ev: EventSelector[S]) extends Await[S#Event]
    case class WaitUntil(when: Instant) extends Await[Unit]
    case class FirstOf[A](alternatives: Await[A]*) extends Await[A]

    case class Execute[A <: Aggregate](command: A#Command) extends ProcessAction[Unit] // TODO force error handling
    case class Fail(reason: String) extends ProcessAction[Unit]
  }

}
