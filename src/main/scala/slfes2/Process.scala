package slfes2

import java.time.Instant
import scala.annotation.implicitNotFound
import cats.free.Free
import shapeless.ops.hlist.Selector
import slfes.utils.StringSerializable
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

    /** Await an event using a selector. The event must be from this bounded context. */
    def await[S <: WithEventType: EventSelector: ValidSelector](selector: S) =
      Free.liftF[ProcessAction, S#Event](AwaitEvent(selector))

    /** Await an event from an aggregate from the same bounded context. */
    //TODO we should be able to infer the aggregateType automatically
    def awaitFrom[A <: Aggregate](aggregateType: A)(aggregate: A#Id) =
      new AwaitFromBuilder[A](aggregateType, aggregate)

    /** Wait until the specified date/time. If the time has already passed it will still be called. */
    def waitUntil(when: Instant) =
      Free.liftF[ProcessAction, Unit](WaitUntil(when))

    /** Execute a command on an aggregate from the same bounded context. */
    //TODO we should be able to infer the aggregateType automatically
    def execute[A <: Aggregate: ValidAggregate](aggregateType: A)(aggregate: A#Id, command: A#Command) =
      Free.liftF[ProcessAction, Unit](Execute(aggregateType, aggregate, command))

    class AwaitFromBuilder[A <: Aggregate] private[Syntax] (aggregateType: A, aggregate: A#Id) {
      def apply[E <: A#Event: AggregateEventType](implicit ev: ValidAggregate[A], idser: StringSerializable[A#Id]) = {
        val selector = AggregateEventSelector(aggregateType)(aggregate)[E]
        await(selector)
      }
    }
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
