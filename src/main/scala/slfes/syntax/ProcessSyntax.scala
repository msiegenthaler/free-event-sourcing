package slfes.syntax

import cats.Monad
import cats.free.Free
import shapeless.{Lub, CNil, :+:, Coproduct}
import shapeless.ops.coproduct.{Inject, Prepend, Basis, Selector}
import slfes._
import slfes.ProcessBodyAction._

object ProcessSyntax {
  def noop: ProcessBody = Monad[ProcessBodyM].pure(())

  /** Send a command to an aggregate.    */
  def execute[Id, C <: Cmd, CS <: Coproduct](to: Id, command: C)(implicit cfi: CommandForId[Id, C]) =
    Free.liftF[ProcessBodyAction, CommandResult[Cmd]](Command[cfi.Aggregate, C](to, command))

  /** Send a command to an aggregate.
      Use this if the automatic conversion from Id to Aggregate does not work. */
  def execute[A <: Aggregate, C <: Cmd : A#IsCommand](to: A#Id, command: C) =
    Free.liftF[ProcessBodyAction, CommandResult[Cmd]](Command(to, command))

  /** Wait for an event to happen. Syntax: await(from(id).event[My](...)) */
  def await[A <: Aggregate, For <: Coproduct, R](handler: Handler[A, For, R]): ProcessBodyM[R] =
    Free.liftF(Await(handler.id, handler.handle))

  /** Wait for an event to happen. Syntax: await(from(id).event[My](...)) */
  def awaitM[A <: Aggregate, For <: Coproduct, R](handler: Handler[A, For, ProcessBodyM[R]]): ProcessBodyM[R] =
    Free.liftF(Await(handler.id, handler.handle)).flatMap((r) ⇒ r)


  /** Used inside await. */
  def from[Id](id: Id)(implicit a: AggregateFromId[Id]): From[a.Out] = From[a.Out](id)
  case class From[A <: Aggregate](id: A#Id) {
    def event[E]: OnFirst[A, E] = new OnFirst[A, E](id)
  }
  case class OnFirst[A <: Aggregate, E](id: A#Id) {
    def apply[R](f: E ⇒ R)(implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil]): Handler[A, E :+: CNil, R] =
      withMetadata[R](e ⇒ f(e.event))
    def withMetadata[R](f: Evt[E, A] ⇒ R)(implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil]): Handler[A, E :+: CNil, R] =
      Handler[A, E :+: CNil, R](id, Evt.select(_).map(f))
  }
  case class Handler[A <: Aggregate, For <: Coproduct, R](id: A#Id, handle: AggregateEvt[A] ⇒ Option[R]) {
    def event[E] = OnChain[A, For, E, R](id, handle)
  }
  case class OnChain[A <: Aggregate, For <: Coproduct, E, R1](id: A#Id, handle: AggregateEvt[A] ⇒ Option[R1]) {
    def apply[R2, ROut](f: E ⇒ R2)
      (implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil], b2: Basis[A#Event, For],
        p: Prepend[For, E :+: CNil], l: Lub[R1, R2, ROut]) = withMetadata(e ⇒ f(e.event))
    def withMetadata[R2, ROut](f: Evt[E, A] ⇒ R2)
      (implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil], b2: Basis[A#Event, For],
        p: Prepend[For, E :+: CNil], l: Lub[R1, R2, ROut]): Handler[A, p.Out, ROut] = {
      Handler[A, p.Out, ROut](id, (event) ⇒
        b1.apply(event.event) match {
          case Left(e) ⇒ handle(event).map(l.left)
          case Right(e) ⇒ Evt.select(event).map(f).map(l.right)
        }
      )
    }
  }

  /** Selector to start a process. */
  def spawnFor[A <: Aggregate](aggregate: A) = SpawnFor(aggregate)
  case class SpawnFor[A <: Aggregate](aggregate: A) {
    def on[E] = SpawnOn[A, E](aggregate)
  }
  case class SpawnOn[A <: Aggregate, E](aggregate: A) {
    def apply[Id](f: E ⇒ Id)(implicit s: Selector[A#Event, E]): (A#Event ⇒ Option[Id]) =
      s(_).map(f)
  }

  /** A fluent way to write the complete process definition. */
  def process(name: String) = ProcessName(name)
  case class ProcessName(name: String) {
    def startedAt[A <: Aggregate](aggregate: A) = ProcessStartedAt(name, aggregate)
  }
  case class ProcessStartedAt[A <: Aggregate](name: String, aggregate: A) {
    def on[E] = ProcessSpawnOn[A, E](name, aggregate)
  }
  case class ProcessSpawnOn[A <: Aggregate, E](name: String, aggregate: A) {
    def apply[Id](f: E ⇒ Id)(implicit s: Selector[A#Event, E]) =
      withMetadata(e ⇒ f(e.event))
    def withMetadata[Id](f: Evt[E, A] ⇒ Id)(implicit s: Selector[A#Event, E]) =
      ProcessWithBody[A, E, Id](name, aggregate, Evt.select[E, A](_).map(f))
  }
  case class ProcessWithBody[A <: Aggregate, E, Id](name: String, aggregate: A, spawn: AggregateEvt[A] ⇒ Option[Id]) {
    def withBody(body: Id ⇒ ProcessBody) = ProcessType[A, Id](name, aggregate, spawn, body)
  }
}
