package fes

import scala.language.higherKinds
import cats.Monad
import cats.free.Free

/** Helper to deal with the CommandEffect monad. */
case class CommandSyntax[Command[_], Event, State]() {
  type Op[A] = CommandEffect.CommandOp[Event, State, A]
  type Effect[A] = Free[Op, A]

  import CommandEffect._

  /** The current state of the aggregate. */
  def current = Free.liftF[Op, State](Current[State]())

  /** Emit an event. */
  def emit(event: Event) = Free.liftF[Op, Unit](Emit(event))
  /** Let the command fail. */
  def fail(reason: String) = Free.liftF[Op, Nothing](Fail(reason))
  /** Does nothing (useful i.e. for else-branches). */
  def noop = Monad[Effect].pure(())

  /** Checks a condition. If the condition is false then the command fails. */
  def failIf(condition: Boolean, reason: ⇒ String) =
    if (condition) fail(reason) else noop

  /** Requires something to be true about the current state. If the condition is false then the command fails. */
  def precondition(precond: State ⇒ Boolean, description: String) = for {
    s ← current
    _ ← failIf(precond(s), s"Precondition failed: $description")
  } yield ()

  /** Executes the action only if the condition is true. */
  def when[A](cond: State ⇒ Boolean, action: Effect[A]) = for {
    s ← current
    _ ← if (cond(s)) action else noop
  } yield ()

  /** Emit an event. */
  def emitS(f: State ⇒ Event) = for {
    s ← current
    event = f(s)
    _ ← emit(event)
  } yield ()
}