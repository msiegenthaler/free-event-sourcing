package slfes2

import java.time.Instant
import scala.annotation.implicitNotFound
import cats.free.Free
import shapeless.{ CNil, Coproduct }
import shapeless.ops.hlist.Selector
import shapeless.ops.coproduct.{ Remove, Selector ⇒ CPSelector }
import slfes.utils.StringSerializable
import slfes2.EventSelector.WithEventType
import slfes2.support.AggregateFromId

class Process[BC <: BoundedContext](boundedContext: BC) {
  @implicitNotFound("${S} is not a valid EventSelector for this process.")
  sealed trait ValidSelector[S]
  object ValidSelector {
    implicit def aggregate[S](implicit ev: AggregateEventSelector.ValidFor[S, BC#Aggregates]) = new ValidSelector[S] {}
  }

  @implicitNotFound("Aggregate ${A} is not a member of this bounded context.")
  type ValidAggregate[A <: Aggregate] = Selector[BC#Aggregates, A]

  /** The same as C =:= CNil, but with a nicer error message. */
  @implicitNotFound("Not all possible errors of the command were handled, ${C} are still unhandled")
  sealed trait AllErrorsHandled[C <: Coproduct]
  object AllErrorsHandled {
    implicit val handled = new AllErrorsHandled[CNil] {}
  }

  @implicitNotFound("The command cannot result in an error of type ${E}")
  type HandleError[Unhandled <: Coproduct, E] = Remove[Unhandled, E]

  type ProcessMonad[A] = Free[ProcessAction, A]

  object Syntax {
    import ProcessAction._

    //TODO firstOf

    /** Await an event using a selector. The event must be from this bounded context. */
    def await[S <: WithEventType: EventSelector: ValidSelector](selector: S) =
      Free.liftF[ProcessAction, S#Event](AwaitEvent(selector))

    /** Actions (execution and await) regarding a specific aggregate. */
    def on[Id, A](aggregate: Id)(implicit afi: AggregateFromId[Id, BC#Aggregates]) = {
      val aggregateType = afi.aggregate(boundedContext.aggregates)
      new OnAggregateBuilder[afi.Out](aggregateType, aggregate)
    }

    /** Alias for 'on'. */
    def from[Id, A](id: Id)(implicit afi: AggregateFromId[Id, BC#Aggregates]) = on(id)

    /** Wait until the specified date/time. If the time has already passed it will still be called. */
    def waitUntil(when: Instant) =
      Free.liftF[ProcessAction, Unit](WaitUntil(when))

    /** Terminate this process instance. */
    def terminate =
      Free.liftF[ProcessAction, Unit](End())

    class OnAggregateBuilder[A <: Aggregate] private[Syntax] (aggregateType: A, aggregate: A#Id) {
      //TODO delete
      /** Execute a command on an aggregate from the same bounded context. */
      def execute(command: A#Command)(implicit ev: ValidAggregate[A]) =
        Free.liftF[ProcessAction, Unit](Execute(aggregateType, aggregate, command))

      /** Execute a command on an aggregate from the same bounded context. */
      //TODO maybe use an implicit for nicer error message
      def execute2[R <: Coproduct](command: A#Command)(catches: ExecuteBuilder[command.type, command.Error] ⇒ ExecuteBuilder[command.type, R])(
        implicit
        ev: ValidAggregate[A],
        ev2: AllErrorsHandled[R]
      ) = {
        val builder = new ExecuteBuilder[command.type, command.Error](command)
        catches(builder)
        //TODO
        terminate
      }

      /** Await an event from an aggregate from the same bounded context. */
      def await[E <: A#Event: AggregateEventType](implicit ev: ValidAggregate[A], idser: StringSerializable[A#Id]) = {
        val selector = AggregateEventSelector(aggregateType)(aggregate)[E]
        Syntax.await(selector)
      }

      class ExecuteBuilder[C <: A#Command, Unhandled <: Coproduct] private[OnAggregateBuilder] (command: C) {
        //TODO add type alias for nicer implicit not found
        def catching[E](handler: E ⇒ ProcessMonad[Unit])(implicit ev: HandleError[Unhandled, E], s: CPSelector[C#Error, E]) =
          new ExecuteBuilder[C, ev.Rest](command)
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

    case class End() extends ProcessAction[Unit]
  }
}
object Process {
}
