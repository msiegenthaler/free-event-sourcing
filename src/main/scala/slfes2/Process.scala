package slfes2

import java.time.Instant
import scala.annotation.implicitNotFound
import cats.Monad
import cats.free.Free
import shapeless.{ :+:, CNil, Coproduct, Poly1 }
import shapeless.ops.hlist.Selector
import shapeless.ops.coproduct.{ Folder, Remove, Selector ⇒ CPSelector }
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

    /** Does nothing. */
    def noop = Monad[Free[ProcessAction, ?]].pure(())

    final class OnAggregateBuilder[A <: Aggregate] private[Syntax] (aggregateType: A, aggregate: A#Id) {
      /** Execute a command on an aggregate from the same bounded context. */
      def execute[R <: Coproduct](command: A#Command)(
        catches: CommandErrorHandler.EmptyBuilder[command.Error] ⇒ CommandErrorHandler.Builder[command.Error, R]
      )(implicit ev: ValidAggregate[A], ev2: AllErrorsHandled[R]) = {
        val builder = CommandErrorHandler.builder[command.Error]
        val errorHandler = catches(builder).errorHandler
        Free.liftF[ProcessAction, Unit](Execute(aggregateType, aggregate, command, errorHandler))
      }

      /** Await an event from an aggregate from the same bounded context. */
      def await[E <: A#Event: AggregateEventType](implicit ev: ValidAggregate[A], idser: StringSerializable[A#Id]) = {
        val selector = AggregateEventSelector(aggregateType)(aggregate)[E]
        Syntax.await(selector)
      }
    }

    type CommandErrorHandler[Error <: Coproduct] = Error ⇒ ProcessMonad[Unit]
    object CommandErrorHandler {
      private[Syntax] def builder[E <: Coproduct]() = new Builder[E, E](e ⇒
        throw new AssertionError("Error in CommandErrorHandler, type was constructed that does not " +
          "handle all cases. Should have been prevented by the compiler"))

      type EmptyBuilder[E <: Coproduct] = Builder[E, E]

      final class Builder[All <: Coproduct, Unhandled <: Coproduct] private[CommandErrorHandler] (handled: CommandErrorHandler[All]) {
        def catching[E](handler: E ⇒ ProcessMonad[Unit])(implicit ev: HandleError[Unhandled, E], s: CPSelector[All, E]) = {
          new Builder[All, ev.Rest](error ⇒ s(error).map(handler).getOrElse(handled(error)))
        }

        /** Return the fully constructed error handler. Can only be called if all cases have been handled */
        private[Syntax] def errorHandler(implicit ev: AllErrorsHandled[Unhandled]) = handled
      }
    }
  }

  //TODO move to top level (include the bounded context).
  sealed trait ProcessAction[+A]
  object ProcessAction {
    sealed trait Await[+A] extends ProcessAction[A]
    case class AwaitEvent[S <: WithEventType: EventSelector: ValidSelector](selector: S) extends Await[S#Event]
    case class WaitUntil(when: Instant) extends Await[Unit]
    case class FirstOf[A](alternatives: Await[A]*) extends Await[A]

    case class Execute[A <: Aggregate: ValidAggregate, Cmd <: A#Command](
      aggregateType: A, aggregate: A#Id, command: A#Command, errorHandler: Cmd#Error ⇒ ProcessMonad[Unit]
    ) extends ProcessAction[Unit]

    case class End() extends ProcessAction[Unit]
  }
}
