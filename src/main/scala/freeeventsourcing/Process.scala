package freeeventsourcing

import java.time.Instant
import scala.annotation.implicitNotFound
import cats.Monad
import cats.free.Free
import shapeless.ops.coproduct.{ Remove, Unifier, Selector ⇒ CPSelector }
import shapeless.ops.hlist.Selector
import shapeless.{ :+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr }
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.support.AggregateFromId
import freeeventsourcing.utils.StringSerializable

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

    /** Await an event using a selector. The event must be from this bounded context. */
    def await[S <: WithEventType: EventSelector: ValidSelector](selector: S) =
      Free.liftF[ProcessAction, S#Event](AwaitEvent(selector))

    /** Send commands to a specific aggregate. */
    def on[Id, A](aggregate: Id)(implicit afi: AggregateFromId[Id, BC#Aggregates]) = {
      val aggregateType = afi.aggregate(boundedContext.aggregates)
      new OnAggregateBuilder[afi.Out](aggregateType, aggregate)
    }

    /** Await events from a specific aggregate. */
    def from[Id, A](aggregate: Id)(implicit afi: AggregateFromId[Id, BC#Aggregates]) = {
      val aggregateType = afi.aggregate(boundedContext.aggregates)
      new FromAggregateBuilder[afi.Out](aggregateType, aggregate)
    }

    /** Wait until the specified date/time. If the time has already passed it will still be called. */
    def waitUntil(when: Instant) =
      Free.liftF[ProcessAction, Unit](WaitUntil(when))

    /** Wait for multiple events and run the path of the first event. */
    def firstOf[Paths <: HList](b: FirstOfBuilder[HNil] ⇒ FirstOfBuilder[Paths])(implicit switch: Switch[Paths]): ProcessMonad[switch.Result] = {
      val paths = b(new FirstOfBuilder(HNil)).collect
      val alternatives = switch.alternatives(paths)
      Free.liftF[ProcessAction, alternatives.Events](FirstOf(alternatives))
        .flatMap(e ⇒ switch.effectFor(paths)(e))
    }

    /** Wait for multiple events and run the path of the first event. */
    def firstOfUnified[Paths <: HList, R <: Coproduct](b: FirstOfBuilder[HNil] ⇒ FirstOfBuilder[Paths])(implicit s: Switch.Aux[Paths, R], u: Unifier[R]) =
      firstOf(b).map(r ⇒ u(r))

    /** Terminate this process instance. */
    def terminate = Free.liftF[ProcessAction, Unit](End())

    /** Does nothing. */
    def noop = Monad[Free[ProcessAction, ?]].pure(())

    /** Helper class for on. */
    final class OnAggregateBuilder[A <: Aggregate] private[Syntax] (aggregateType: A, aggregate: A#Id) {
      /** Execute a command on the aggregate. */
      def execute[R <: Coproduct](command: A#Command)(
        catches: CommandErrorHandler.EmptyBuilder[command.Error] ⇒ CommandErrorHandler.Builder[command.Error, R]
      )(implicit ev: ValidAggregate[A], ev2: AllErrorsHandled[R]) = {
        val builder = CommandErrorHandler.builder[command.Error]
        val errorHandler = catches(builder).errorHandler
        Free.liftF[ProcessAction, Unit](Execute(aggregateType, aggregate, command, errorHandler))
      }
    }

    /** Helper class for from. */
    final class FromAggregateBuilder[A <: Aggregate] private[Syntax] (aggregateType: A, aggregate: A#Id) {
      /** Await an event from the aggregate. */
      def await[E <: A#Event: AggregateEventType](implicit ev: ValidAggregate[A], idser: StringSerializable[A#Id]) = {
        val selector = AggregateEventSelector(aggregateType)(aggregate)[E]
        Syntax.await(selector)
      }
    }

    /** Helper class for execute. */
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

    /** Option inside a firstOf. */
    sealed trait SwitchPath {
      type Event
      type Result
      val action: Await[Event]
      def effect(event: Event): ProcessMonad[Result]
    }
    object SwitchPath {
      type Aux[E, R] = SwitchPath { type Event = E; type Result = R }
    }
    /** All options of a firstOf. */
    sealed trait Switch[Paths <: HList] {
      type A <: Alternatives
      type Events <: Coproduct
      type Result <: Coproduct
      def effectFor(paths: Paths)(event: A#Events): ProcessMonad[Result]
      def alternatives(paths: Paths): A
    }
    object Switch {
      type Aux[P <: HList, R <: Coproduct] = Switch[P] { type Result = R }

      import Alternatives.Alternative

      implicit def end = new Switch[HNil] {
        type A = Alternatives.No
        type Events = CNil
        type Result = CNil
        def effectFor(paths: HNil)(event: CNil) = throw new AssertionError("Cannot be reached, the compiler is supposed to prevent that")
        def alternatives(paths: HNil) = Alternatives.No
      }
      implicit def head[E, R, T <: HList](implicit t: Switch[T]) = new Switch[SwitchPath.Aux[E, R] :: T] {
        type A = E Alternative t.A
        type P = SwitchPath.Aux[E, R]
        type Events = P#Event :+: t.Events
        type Result = P#Result :+: t.Result
        def effectFor(paths: P :: T)(event: A#Events) = event match {
          case Inl(myEvent)    ⇒ paths.head.effect(myEvent).map(Inl(_))
          case Inr(otherEvent) ⇒ t.effectFor(paths.tail)(otherEvent).map(Inr(_))
        }
        def alternatives(paths: P :: T) =
          Alternative(paths.head.action, t.alternatives(paths.tail))
      }
    }

    final class FirstOfBuilder[A <: HList] private[Syntax] (paths: A) {
      /** Await an event using a selector. The event must be from this bounded context. */
      def on[S <: WithEventType: EventSelector: ValidSelector](selector: S) = new OnBuilder(selector)

      /** Await event from a specific aggregate. */
      def from[Id, A](aggregate: Id)(implicit afi: AggregateFromId[Id, BC#Aggregates]) = {
        val aggregateType = afi.aggregate(boundedContext.aggregates)
        new FromAggregateBuilder[afi.Out](aggregateType, aggregate)
      }

      /** Wait until the specified instant, then return unit. */
      def timeout[R](when: Instant)(thenDo: ProcessMonad[R]): FirstOfBuilder[SwitchPath.Aux[Unit, R] :: A] = {
        val path = new SwitchPath {
          type Event = Unit
          type Result = R
          val action = WaitUntil(when)
          def effect(event: Unit) = thenDo

        }
        new FirstOfBuilder(path :: paths)
      }

      private[Syntax] def collect: A = paths

      final class OnBuilder[S <: WithEventType: EventSelector: ValidSelector] private[FirstOfBuilder] (selector: S) {
        /** This is a flatMap */
        def execute[R](body: S#Event ⇒ ProcessMonad[R]) = flatMap(body)

        def map[R](f: S#Event ⇒ R) = flatMap(f.andThen(Monad[ProcessMonad].pure))

        def event = map(identity)

        /** Terminate the process if the event occurs. */
        def terminate = flatMap(_ ⇒ Syntax.terminate)

        def flatMap[R](body: S#Event ⇒ ProcessMonad[R]): FirstOfBuilder[SwitchPath.Aux[S#Event, R] :: A] = {
          val path = new SwitchPath {
            type Event = S#Event
            type Result = R
            val action = AwaitEvent(OnBuilder.this.selector)
            def effect(event: Event) = body(event)
          }
          new FirstOfBuilder(path :: paths)
        }
      }

      final class FromAggregateBuilder[A <: Aggregate] private[FirstOfBuilder] (aggregateType: A, aggregate: A#Id) {
        /** Await an event from the aggregate. */
        def on[E <: A#Event: AggregateEventType](implicit ev: ValidAggregate[A], idser: StringSerializable[A#Id]) = {
          val selector = AggregateEventSelector(aggregateType)(aggregate)[E]
          FirstOfBuilder.this.on(selector)
        }
      }
    }
  }

  //TODO move to top level (include the bounded context).
  sealed trait ProcessAction[+A]
  object ProcessAction {
    sealed trait Await[+A] extends ProcessAction[A]
    case class AwaitEvent[S <: WithEventType: EventSelector: ValidSelector](selector: S) extends Await[S#Event]
    case class WaitUntil(when: Instant) extends Await[Unit]
    case class FirstOf[Alt <: Alternatives](alternatives: Alt) extends Await[Alt#Events]

    case class Execute[A <: Aggregate: ValidAggregate, Cmd <: A#Command](
      aggregateType: A, aggregate: A#Id, command: A#Command, errorHandler: Cmd#Error ⇒ ProcessMonad[Unit]
    ) extends ProcessAction[Unit]

    case class End() extends ProcessAction[Unit]

    /** Specialized Coproduct that contains Awaits and keeps track of the resulting event coproduct. */
    sealed trait Alternatives {
      type Events <: Coproduct
    }
    object Alternatives {
      type No = No.type
      case object No extends Alternatives {
        type Events = CNil
      }
      case class Alternative[A, T <: Alternatives](await: Await[A], more: T) extends Alternatives {
        type Events = A :+: T#Events
      }
    }
  }
}
