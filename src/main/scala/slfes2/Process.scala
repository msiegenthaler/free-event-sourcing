package slfes2

import java.time.Instant
import scala.annotation.implicitNotFound
import shapeless.{ HList, :: }
import slfes2.EventSelector.WithEventType
import slfes2.Process.ValidAggregateSelector

class Process[BC <: BoundedContext](boundedContext: BC) {
  @implicitNotFound("${S} is not a valid EventSelector for this process.")
  sealed trait ValidSelector[S]
  object ValidSelector {
    implicit def aggregate[S](implicit ev: ValidAggregateSelector[S, BC#Aggregates]) = new ValidSelector[S] {}
  }

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
object Process {
  @implicitNotFound("${S} is not a valid aggregate event selector for the aggregates ${Aggregates}")
  sealed trait ValidAggregateSelector[S, Aggregates <: HList]
  object ValidAggregateSelector {
    implicit def head[A <: Aggregate, E <: A#Event, T <: HList] =
      new ValidAggregateSelector[AggregateEventSelector[A, E], A :: T] {}
    implicit def tail[S, H, T <: HList](implicit t: ValidAggregateSelector[S, T]) =
      new ValidAggregateSelector[S, H :: T] {}
  }
}
