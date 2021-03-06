package freeeventsourcing.syntax

import java.time.Instant
import scala.annotation.implicitNotFound
import cats.Monad
import cats.free.Free
import freeeventsourcing.api.domainmodel.EventSelector.WithEventType
import freeeventsourcing.api.domainmodel.EventSelector.ops._
import freeeventsourcing.api.domainmodel.ProcessAction.FirstOf.{ Alternative, Alternatives }
import freeeventsourcing.api.domainmodel.ProcessAction._
import freeeventsourcing.api.{ domainmodel, _ }
import freeeventsourcing.api.domainmodel._
import freeeventsourcing.api.domainmodel.eventselector.AggregateEventSelector
import freeeventsourcing.syntax.support.AggregateFromId
import freeeventsourcing.utils.StringSerializable
import shapeless._
import shapeless.ops.coproduct.{ Remove, Reverse, Selector, Unifier }

/** Nice monadic syntax to write processes. */
case class ProcessSyntax[DM <: DomainModel](domainModel: DM) {
  type Action[+A] = ProcessAction[DM, A]
  type ProcessMonad[A] = Free[Action, A]

  type ValidSelector[S] = domainmodel.eventselector.ValidSelector[DM, S]

  import Builders._

  /** Await an event using a selector. The event must be from this bounded context (domain model). */
  def await[S <: WithEventType: EventSelector: ValidSelector](selector: S) =
    awaitMetadata(selector).map(_.payload)

  /** Await an event (including its metadata) using a selector. The event must be from this bounded context. */
  def awaitMetadata[S <: WithEventType: EventSelector: ValidSelector](selector: S) =
    lift(AwaitEvent[DM, S](selector))

  /** Send commands to a specific aggregate. */
  def on[Id, A](aggregate: Id)(implicit afi: AggregateFromId[Id, DM#Aggregates]) = {
    val aggregateType = afi.aggregate(domainModel.aggregates)
    new OnAggregateBuilder[afi.Out](aggregateType, aggregate)
  }

  /** Await events from a specific aggregate. */
  def from[Id, A](aggregate: Id)(implicit afi: AggregateFromId[Id, DM#Aggregates]) = {
    val aggregateType = afi.aggregate(domainModel.aggregates)
    new FromAggregateBuilder[afi.Out](aggregateType, aggregate)
  }

  /** Wait until the specified date/time. If the time has already passed it will still be called. */
  def waitUntil(when: Instant) =
    lift[Unit](WaitUntil[DM](when))

  /** Wait for multiple events and run the path of the first event. */
  def firstOf[Paths <: HList, R <: Coproduct](b: FirstOfBuilder[HNil] ⇒ FirstOfBuilder[Paths])(
    implicit
    switch: Switch.Aux[Paths, R], r: Reverse[R]
  ): ProcessMonad[r.Out] = {
    val paths = b(new FirstOfBuilder(HNil)).collect
    val alternatives = switch.alternatives(paths)
    lift[alternatives.Events](FirstOf[DM, alternatives.type](alternatives))
      .flatMap(e ⇒ switch.effectFor(paths)(e).map(_.reverse))
  }

  /** Wait for multiple events and run the path of the first event. */
  def firstOfUnified[Paths <: HList, R <: Coproduct](b: FirstOfBuilder[HNil] ⇒ FirstOfBuilder[Paths])(implicit switch: Switch.Aux[Paths, R], u: Unifier[R]) = {
    val paths = b(new FirstOfBuilder(HNil)).collect
    val alternatives = switch.alternatives(paths)
    lift[alternatives.Events](FirstOf[DM, alternatives.type](alternatives))
      .flatMap(e ⇒ switch.effectFor(paths)(e).map(r ⇒ u(r)))
  }

  /** Terminate this process instance. */
  def terminate = lift[Unit](End[DM]())

  /** Lifts a value into the monad. Same as Monad.pure(v). */
  def value[A](v: A) = Monad[ProcessMonad].pure(v)

  /** Does nothing. */
  def noop = value(())

  /////////////////////////////////////////////////////

  private[this] def lift[A](action: ProcessAction[DM, A]) = Free.liftF[ProcessAction[DM, ?], A](action)

  /** Internas used by the above functions. Need to be public but are not meant to be used directly. */
  object Builders {
    /** Helper class for on. */
    final class OnAggregateBuilder[A <: Aggregate] private[ProcessSyntax] (aggregateType: A, aggregate: A#Id) {
      /** Execute a command on the aggregate. */
      def execute[R <: Coproduct](command: A#Command)(
        catches: CommandErrorHandler.EmptyBuilder[command.Error] ⇒ CommandErrorHandler.Builder[command.Error, R]
      )(implicit ev: ValidAggregate[DM, A], ev2: AllErrorsHandled[R]) = {
        val builder = CommandErrorHandler.builder[command.Error]
        val errorHandler = catches(builder).errorHandler
        lift[Unit](Execute[DM, A, command.type](aggregateType, aggregate, command, errorHandler))
      }
    }

    /** Helper class for from. */
    final class FromAggregateBuilder[A <: Aggregate] private[ProcessSyntax] (aggregateType: A, aggregate: A#Id) {
      /** Await an event from the aggregate. */
      def await[E <: A#Event: AggregateEventType[A, ?]: Typeable](implicit ev: ValidAggregate[DM, A], s: StringSerializable[A#Id]) =
        awaitMetadata.map(_.payload.event)

      /** Await an event (including its metdata) from the aggregate. */
      def awaitMetadata[E <: A#Event: AggregateEventType[A, ?]: Typeable](implicit ev: ValidAggregate[DM, A], s: StringSerializable[A#Id]) = {
        val selector = AggregateEventSelector(aggregateType)(aggregate)[E]
        ProcessSyntax.this.awaitMetadata(selector)
      }
    }

    /** Helper class for execute. */
    type CommandErrorHandler[Error <: Coproduct] = Error ⇒ ProcessMonad[Unit]
    object CommandErrorHandler {
      private[ProcessSyntax] def builder[E <: Coproduct]() = new Builder[E, E](e ⇒
        throw new AssertionError("Error in CommandErrorHandler, type was constructed that does not " +
          "handle all cases. Should have been prevented by the compiler"))

      type EmptyBuilder[E <: Coproduct] = Builder[E, E]

      final class Builder[All <: Coproduct, Unhandled <: Coproduct] private[CommandErrorHandler] (handled: CommandErrorHandler[All]) {
        /** Handle the specified error. */
        def catching[E](handler: E ⇒ ProcessMonad[_])(implicit ev: HandleError[Unhandled, E], s: Selector[All, E]) =
          new Builder[All, ev.Rest](error ⇒ s(error).map(e ⇒ handler(e).map(_ ⇒ ())).getOrElse(handled(error)))

        /** Same as #catching, but does ignores the error. */
        def catched[E: HandleError[Unhandled, ?]: Selector[All, ?]](handler: ProcessMonad[_]) =
          catching[E]((e: E) ⇒ handler)

        /** Terminate the process (see ProcessSyntax#terminate) if the specified error occurs. */
        def terminateOn[E: HandleError[Unhandled, ?]: Selector[All, ?]] =
          catching((_: E) ⇒ terminate)

        /** Return the fully constructed error handler. Can only be called if all cases have been handled */
        private[ProcessSyntax] def errorHandler(implicit ev: AllErrorsHandled[Unhandled]) = handled
      }
    }

    /** Option inside a firstOf. */
    sealed trait SwitchPath {
      type Event
      type Result
      val action: Await[DM, Event]
      def effect(event: Event): ProcessMonad[Result]
    }
    object SwitchPath {
      type Aux[E, R] = SwitchPath { type Event = E; type Result = R }
    }
    /** All options of a firstOf. */
    sealed trait Switch[Paths <: HList] {
      type Events <: Coproduct
      type A <: Alternatives[DM]
      type Result <: Coproduct
      def effectFor(paths: Paths)(event: A#Events): ProcessMonad[Result]
      def alternatives(paths: Paths): A
    }
    object Switch {
      type Aux[P <: HList, R <: Coproduct] = Switch[P] { type Result = R }

      implicit def end = new Switch[HNil] {
        type A = FirstOf.Empty[DM]
        type Events = CNil
        type Result = CNil
        def effectFor(paths: HNil)(event: CNil) = throw new AssertionError("Cannot be reached, the compiler is supposed to prevent that")
        def alternatives(paths: HNil) = FirstOf.Empty()
      }
      implicit def head[E, R, T <: HList](implicit t: Switch[T]) = new Switch[SwitchPath.Aux[E, R] :: T] {
        type A = Alternative[DM, E, t.A]
        type P = SwitchPath.Aux[E, R]
        type Events = P#Event :+: t.Events
        type Result = P#Result :+: t.Result
        def effectFor(paths: P :: T)(event: A#Events) = event match {
          case Inl(myEvent)    ⇒ paths.head.effect(myEvent).map(Inl(_))
          case Inr(otherEvent) ⇒ t.effectFor(paths.tail)(otherEvent).map(Inr(_))
        }
        def alternatives(paths: P :: T) = {
          def alt[A, M <: Alternatives[DM]](await: Await[DM, A], more: M) = Alternative[DM, A, M](await, more)
          alt(paths.head.action, t.alternatives(paths.tail))
        }
      }
    }

    final class FirstOfBuilder[A <: HList] private[ProcessSyntax] (paths: A) {
      /** Await an event using a selector. The event must be from this bounded context. */
      def on[S <: WithEventType: EventSelector: ValidSelector](selector: S) = new OnBuilder(selector)

      /** Await event from a specific aggregate. */
      def from[Id, A](aggregate: Id)(implicit afi: AggregateFromId[Id, DM#Aggregates]) = {
        val aggregateType = afi.aggregate(domainModel.aggregates)
        new FromAggregateBuilder[afi.Out](aggregateType, aggregate)
      }

      /** Wait until the specified instant, then return unit. */
      def timeout[R](when: Instant)(thenDo: ProcessMonad[R]): FirstOfBuilder[SwitchPath.Aux[Unit, R] :: A] = {
        val path = new SwitchPath {
          type Event = Unit
          type Result = R
          val action = WaitUntil[DM](when)
          def effect(event: Unit) = thenDo

        }
        new FirstOfBuilder(path :: paths)
      }

      private[ProcessSyntax] def collect: A = paths

      final class OnBuilder[S <: WithEventType: EventSelector: ValidSelector] private[FirstOfBuilder] (selector: S) {
        def execute[R](body: ProcessMonad[R]) = flatMap(_ ⇒ body)

        def map[R](f: S#Event ⇒ R) = flatMap(f.andThen(Monad[ProcessMonad].pure))

        def mapMetadata[R](f: EventWithMetadata[S#Event] ⇒ R) = flatMapMetadata(f.andThen(Monad[ProcessMonad].pure))

        def value[R](value: R) = map(_ ⇒ value)

        def event = map(identity)

        def eventWithMetadata = mapMetadata(identity)

        /** Terminate the process if the event occurs. */
        def terminate = flatMap(_ ⇒ ProcessSyntax.this.terminate)

        def flatMap[R](body: S#Event ⇒ ProcessMonad[R]) =
          flatMapMetadata(e ⇒ body(e.payload))

        def flatMapMetadata[R](body: EventWithMetadata[S#Event] ⇒ ProcessMonad[R]): FirstOfBuilder[SwitchPath.Aux[EventWithMetadata[S#Event], R] :: A] = {
          val path = new SwitchPath {
            type Event = EventWithMetadata[S#Event]
            type Result = R
            val action = AwaitEvent[DM, S](OnBuilder.this.selector)
            def effect(event: Event) = body(event)
          }
          new FirstOfBuilder(path :: paths)
        }
      }

      final class FromAggregateBuilder[A <: Aggregate] private[FirstOfBuilder] (aggregateType: A, aggregate: A#Id) {
        /** Await an event from the aggregate. */
        def on[E <: A#Event: AggregateEventType[A, ?]: Typeable](implicit ev: ValidAggregate[DM, A], s: StringSerializable[A#Id]) =
          when[E].select

        /** Advanced selection of an element (filter, after). */
        def when[E <: A#Event: AggregateEventType[A, ?]: Typeable](implicit ev: ValidAggregate[DM, A], s: StringSerializable[A#Id]) = {
          val selector = AggregateEventSelector(aggregateType)(aggregate)[E]
          new SelectorBuilder(selector)
        }

        final class SelectorBuilder[S <: WithEventType: EventSelector: ValidSelector] private[FromAggregateBuilder] (selector: S) {
          /** Select only events that happened after the specified time. */
          def after(time: EventTime) =
            new SelectorBuilder(selector.after(time))
          /** Select only events that match the predicate. */
          def matches(predicate: S#Event ⇒ Boolean) =
            matchMetadata((e, m) ⇒ predicate(e))
          /** Select only events that match the predicate - include the metadata into the decision. */
          def matchMetadata[E](predicate: (S#Event, EventMetadata) ⇒ Boolean) =
            new SelectorBuilder(selector.where((e, m) ⇒ if (predicate(e, m)) Some(e) else None))
          def select = FirstOfBuilder.this.on(selector)
        }
      }
    }

    /** The same as C =:= CNil, but with a nicer error message. */
    @implicitNotFound("Not all possible errors of the command were handled, ${C} are still unhandled")
    sealed trait AllErrorsHandled[C <: Coproduct]
    object AllErrorsHandled {
      implicit val handled = new AllErrorsHandled[CNil] {}
    }

    @implicitNotFound("The command cannot result in an error of type ${E}")
    type HandleError[Unhandled <: Coproduct, E] = Remove[Unhandled, E]
  }
}
