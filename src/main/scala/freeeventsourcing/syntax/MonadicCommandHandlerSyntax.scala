package freeeventsourcing.syntax

import scala.language.implicitConversions
import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq
import cats.Monad
import cats.data.WriterT
import cats.instances.either._
import cats.instances.list._
import freeeventsourcing.api.domainmodel.AggregateCommand
import shapeless.ops.coproduct.Inject

/** Syntactic support for writing command handlers in monadic style. */
trait MonadicCommandHandlerSyntax[Command <: AggregateCommand, Event, State] {
  protected type ResultM[C <: Command, A] = WriterT[Either[C#Error, ?], List[Event], A]

  protected[this] def onM[C <: Command](f: OnCallM[C] ⇒ ResultM[C, Unit]): C ⇒ State ⇒ Either[C#Error, Seq[Event]] = { c ⇒
    s: State ⇒
      val call = new OnCallM[C](c, s)
      f(call).run.map(_._1)
  }

  /** Needed to allow usage of ResultM inside the for comprehension. */
  protected[this] implicit def eventListMonoid = catsKernelStdMonoidForList[Event]
  /** Else everybody using us will have to 'import cats.instances.either._' */
  protected[this] implicit def commandResultMonad[C]: Monad[Either[C, ?]] = catsStdInstancesForEither[C]

  protected[this] implicit def onCallToCommand[C <: Command](call: OnCallM[C]): C = call.command

  protected[this] class OnCallM[C <: Command](val command: C, val state: State) {
    type CommandResult[A] = Either[C#Error, A]
    type M[A] = WriterT[CommandResult, List[Event], A]
    @implicitNotFound("${E} is not a valid error for this command")
    type IsError[E] = Inject[C#Error, E]

    /** Do nothing (return Unit). */
    def ignore = Monad[M].pure(())
    /** Do nothing (return Unit). */
    def noop = ignore

    /** Get the command. Sometimes easier to use than simply #command. */
    def commandM = Monad[M].pure(command)
    def cmd = command

    /** Get the state. Sometimes easier to use than simply #state. */
    def stateM = Monad[M].pure(state)

    /** Emit an event (will only be emitted if the handler completes without an error). */
    def emit(event: Event*): M[Unit] = WriterT.tell(event.toList)

    /** Fail the command handling with the error. */
    def fail[Error](error: Error)(implicit inject: IsError[Error]): M[Unit] = {
      val l: CommandResult[Unit] = Left(inject(error))
      WriterT.valueT(l)
    }

    /** Fail with the error if the condition matches. */
    def failIf[Error: IsError](cond: ⇒ Boolean)(error: ⇒ Error): M[Unit] =
      if (cond) fail(error) else noop

    /** Fail with the error if the condition does not match. */
    def assertThat[Error: IsError](cond: ⇒ Boolean)(error: ⇒ Error): M[Unit] =
      failIf(!cond)(error)

    /** Emit event only if the condition matches. */
    def emitIf(cond: ⇒ Boolean)(event: ⇒ Event): M[Unit] =
      if (cond) emit(event) else noop
  }
}
