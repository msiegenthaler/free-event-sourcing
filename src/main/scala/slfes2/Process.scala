package slfes2

import java.time.Instant
import scala.annotation.implicitNotFound
import shapeless._
import shapeless.ops.hlist.Selector
import slfes2.EventSelector.WithEventType

trait Process[BC <: BoundedContext] {
  @implicitNotFound("${S} is not a valid EventSelector within this bounded context.")
  type ValidSelector[S <: WithEventType] = Selector[BC#Selectors, S]

  sealed trait ProcessAction[+A]
  object ProcessAction {
    sealed trait Await[+A] extends ProcessAction[A]
    case class AwaitEvent[S <: WithEventType: ValidSelector](selector: S)(implicit ev: EventSelector[S])
      extends Await[S#Event]
    case class WaitUntil(when: Instant) extends Await[Unit]
    case class FirstOf[A](alternatives: Await[A]*) extends Await[A]

    case class Execute[A <: Aggregate](command: A#Command) extends ProcessAction[Unit] // TODO force error handling
    case class Fail(reason: String) extends ProcessAction[Unit]
  }

}
