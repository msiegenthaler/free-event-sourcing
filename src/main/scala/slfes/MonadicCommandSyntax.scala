package slfes

import cats.Monad
import cats.data.{WriterT, Xor}
import cats.std.list
import shapeless.ops.coproduct.Inject
import shapeless.{Poly1, Coproduct}

case class MonadicCommandSyntax[State, Commands <: Coproduct, Events <: Coproduct, T <: Poly1](poly: T) {
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]
  type IsEvent[Event] = Inject[Events, Event]

  def onM[C <: Cmd : IsCommand](f: CommandContext[C] ⇒ CommandMonad[C, Unit]): poly.Case.Aux[C, State ⇒ Result[C]] = {
    poly.at(c ⇒ s ⇒ f(CommandContext(c, s)).run.map(_._1))
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