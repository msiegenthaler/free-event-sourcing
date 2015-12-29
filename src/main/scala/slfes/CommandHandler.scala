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
    at(c ⇒ s ⇒ f(CommandContext(c, s)).run.map(_._1))
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

  type CommandMonad[Cmd <: ACmd, A] = WriterT[Xor[Cmd#Error, ?], List[Events], A]

  implicit def listMonoid = list.listAlgebra[Events]

  case class CommandContext[Cmd <: ACmd](command: Cmd, state: State) {
    //Help the compiler a bit..
    type M1[A] = Xor[Cmd#Error, A]
    type CM[A] = WriterT[M1, List[Events], A]
    type IsError[E] = Inject[Cmd#Error, E]

    // Verbs for the CommandMonad
    /** Fail the command handling with the error. */
    def fail[Error: IsError](error: Error): CM[Unit] = {
      val l: M1[Unit] = Xor.left(Inject[Cmd#Error, Error].apply(error))
      WriterT.valueT(l)
    }
    /** Emit an event (in case the command handling is successful. */
    def emit[Event: IsEvent](event: Event): CM[Unit] = {
      val e = Inject[Events, Event].apply(event) :: Nil
      WriterT.tell(e)
    }
    /** Do nothing */
    def noop: CM[Unit] = Monad[CM].pure(())

    def failIf[Error: IsError](cond: Boolean, error: ⇒ Error): CM[Unit] =
      if (cond) fail(error) else noop
  }
}
