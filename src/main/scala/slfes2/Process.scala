package slfes2

import java.time.Instant
import scala.annotation.implicitNotFound
import cats.free.Free
import shapeless.ops.hlist.Selector
import slfes2.EventSelector.WithEventType

class Process[BC <: BoundedContext](boundedContext: BC) {
  @implicitNotFound("${S} is not a valid EventSelector for this process.")
  sealed trait ValidSelector[S]
  object ValidSelector {
    implicit def aggregate[S](implicit ev: AggregateEventSelector.ValidFor[S, BC#Aggregates]) = new ValidSelector[S] {}
  }

  @implicitNotFound("Aggregate ${A} is not a member of this bounded context.")
  type ValidAggregate[A <: Aggregate] = Selector[BC#Aggregates, A]

  object Syntax {
    import ProcessAction._

    //TODO firstOf

    def await[S <: WithEventType: EventSelector: ValidSelector](selector: S) =
      Free.liftF[ProcessAction, S#Event](AwaitEvent(selector))

    def waitUntil(when: Instant) =
      Free.liftF[ProcessAction, Unit](WaitUntil(when))

    def execute[A <: Aggregate: ValidAggregate](aggregateType: A)(aggregate: A#Id, command: A#Command) =
      Free.liftF[ProcessAction, Unit](Execute(aggregateType, aggregate, command))
  }

  sealed trait ProcessAction[+A]
  object ProcessAction {
    sealed trait Await[+A] extends ProcessAction[A]
    case class AwaitEvent[S <: WithEventType: EventSelector: ValidSelector](selector: S) extends Await[S#Event]
    case class WaitUntil(when: Instant) extends Await[Unit]
    case class FirstOf[A](alternatives: Await[A]*) extends Await[A]

    case class Execute[A <: Aggregate: ValidAggregate](aggregateType: A, aggregate: A#Id, command: A#Command)
      extends ProcessAction[Unit] // TODO force error handling
  }
}
object Process {
}
