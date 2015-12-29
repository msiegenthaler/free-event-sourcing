package slfes

import cats.{MonadCombine, Functor, Monad}
import slfes.utils.CoproductConstraint

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
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]
  type IsEvent[Event] = Inject[Events, Event]

  def on[C <: Cmd : IsCommand](f: C ⇒ State ⇒ Result[C]): Case.Aux[C, State ⇒ Result[C]] =
    at(c ⇒ s ⇒ f(c)(s))

  def onM[C <: Cmd : IsCommand](f: CommandContext[C] ⇒ CommandMonad[C, Unit]): Case.Aux[C, State ⇒ Result[C]] = {
    at(c ⇒ s ⇒ f(CommandContext(c, s)).run.map(_._1))
  }

  def liftEvent[Event](event: Event)(implicit inject: Inject[Events, Event]): Events = inject(event)

  // Add utility methods on the command (usage cmd.success(..))
  implicit class CommandResponse[E <: Coproduct](val cmd: Cmd {type Errors = E}) {
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

    def fail[Error](error: Error)(implicit inject: Inject[E, Error]): Res =
      Xor.Left(inject(error))
  }

  type CommandMonad[C <: Cmd, A] = WriterT[Xor[C#Errors, ?], List[Events], A]

  implicit def listMonoid = list.listAlgebra[Events]

  case class CommandContext[C <: Cmd](command: C, state: State) {
    //Help the compiler a bit..
    type M1[A] = Xor[C#Errors, A]
    type CM[A] = WriterT[M1, List[Events], A]
    type IsError[E] = Inject[C#Errors, E]

    // Verbs for the CommandMonad
    /** Fail the command handling with the error. */
    def fail[Error: IsError](error: Error): CM[Unit] = {
      val l: M1[Unit] = Xor.left(Inject[C#Errors, Error].apply(error))
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
