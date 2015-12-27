package slfes

import cats.{MonadCombine, Functor, Monad}

import scala.language.higherKinds
import scala.language.implicitConversions
import cats._
import cats.std.list
import cats.functor._
import cats.Monad._
import cats.data._
import cats.data.Kleisli
import shapeless._
import shapeless.ops.coproduct.Inject
import CoproductConstraint._


trait CommandHandler[State, Commands <: Coproduct, Events <: Coproduct] extends Poly1 {
  type ACmd = {type Error <: Coproduct}
  type Result[Cmd <: ACmd] = Xor[Cmd#Error, Seq[Events]]
  type IsCommand[Cmd] = Inject[Commands, Cmd]
  type IsEvent[Event] = Inject[Events, Event]

  def on[Cmd <: ACmd : IsCommand](f: Cmd ⇒ State ⇒ Result[Cmd]): Case.Aux[Cmd, State ⇒ Result[Cmd]] =
    at(c ⇒ s ⇒ f(c)(s))

  def onM[Cmd <: ACmd : IsCommand](f: CommandContext[Cmd] ⇒ CommandMonad[Cmd, Unit]): Case.Aux[Cmd, State ⇒ Result[Cmd]] = {
    at(c ⇒ s ⇒ f(CommandContext[Cmd]).run(c).run(s).run.map(_._1))
  }

  def liftEvent[Event](event: Event)(implicit inject: Inject[Events, Event]): Events = inject(event)

  // Add utility methods on the command (usage cmd.success(..))
  implicit class CommandResponse[Errors <: Coproduct](val cmd: Command[Errors]) {
    type Res = Result[cmd.type]

    def success: Res =
      Xor.right(Seq.empty)
    def success[Event: IsEvent](event: Event): Res =
      Xor.right(Seq(liftEvent(event)))
    def success(events: Seq[Events]): Res =
      Xor.right(events)
    /** Usage: success(Event1() :: Event2() :: HList) */
    def success[EventHList <: HList : <*<[Events]#λ](events: EventHList): Res =
      Xor.right(CoproductConstraint[EventHList, Events].coproductList(events))

    def fail[Error](error: Error)(implicit inject: Inject[Errors, Error]): Res =
      Xor.Left(inject(error))
  }


  type CommandMonad[Cmd <: ACmd, A] = ReaderT[ReaderT[WriterT[Xor[Cmd#Error, ?], List[Events], ?], State, ?], Cmd, A]

  case class CommandContext[Cmd <: ACmd]() {
    //Help the compiler a bit..
    type CM1[A] = Xor[Cmd#Error, A]
    type CM2[A] = WriterT[CM1, List[Events], A]
    type CM3[A] = ReaderT[CM2, State, A]
    type CM[A] = ReaderT[CM3, Cmd, A]
    implicit def listMonoid = list.listAlgebra[Events]
    implicit def cm2: Monad[CM2] = WriterT.writerTMonad[CM1, List[Events]]

    // Verbs for the CommandMonad
    /** Aggregate state to command is handled in. */
    def state: CM[State] = ???
    /** The command to handle. */
    def command: CM[Cmd] = ???
    /** Fail the command handling with the error. */
    def fail[E](error: E)(implicit inject: Inject[Cmd#Error, E]): CM[Unit] = ???
    /** Emit an event (in case the command handling is successful. */
    def emit[Event: IsEvent](event: Event): CM[Unit] = ???
    /** Do nothing */
    def noop: CM[Unit] = ???
  }
}
