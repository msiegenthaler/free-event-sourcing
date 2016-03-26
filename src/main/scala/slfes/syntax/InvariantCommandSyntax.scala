package slfes.syntax

import scala.collection.immutable.Seq
import scala.language.implicitConversions
import cats.data.{ WriterT, Xor }
import cats.std.list
import shapeless.ops.coproduct.{ Basis, Inject }
import shapeless.{ Coproduct, Poly1 }
import slfes.{ Cmd, Inv }

import scala.annotation.implicitNotFound

case class InvariantCommandSyntax[State, Commands <: Coproduct, Events <: Coproduct, In <: Inv[State], InErrs <: Coproduct, T <: Poly1](
    applyEvent: Events ⇒ State ⇒ State,
    invariants: Traversable[In],
    onInvariantFailed: In ⇒ InErrs
)(val poly: T) {
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]

  def on[C <: Cmd: IsCommand](f: C ⇒ State ⇒ Result[C])(implicit basis: Basis[C#Errors, InErrs]): poly.Case.Aux[C, State ⇒ Result[C]] = poly.at { c ⇒ s ⇒
    def mkError(failed: In): Result[C] = Xor.left(basis.inverse(Right(onInvariantFailed(failed))))
    f(c)(s).flatMap { events ⇒
      val s2 = events.foldLeft(s)((s2, e) ⇒ applyEvent(e)(s2))
      invariants.find(inv ⇒ !inv(s2)).map(mkError).getOrElse(Xor.right(events))
    }
  }

  implicit def commandSyntaxResponse[E <: Coproduct](cmd: Cmd { type Errors = E }): CommandSyntaxResponse[E, Events] =
    CommandSyntaxResponse(cmd)
}

case class InvariantMonadicCommandSyntax[State, Commands <: Coproduct, Events <: Coproduct, In <: Inv[State], InErrs <: Coproduct, T <: Poly1](
    applyEvent: Events ⇒ State ⇒ State,
    invariants: Traversable[In],
    onInvariantFailed: In ⇒ InErrs
)(val poly: T) {
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]

  @implicitNotFound("Command ${C} does not have the defined invariants as valid errors.")
  type ContainsInvariantErrors[C <: Cmd] = Basis[C#Errors, InErrs]

  def onM[C <: Cmd: IsCommand](
    f: MonadicCommandContext[State, Events, C] ⇒ CommandMonad[C, Unit]
  )(implicit basis: ContainsInvariantErrors[C]): poly.Case.Aux[C, State ⇒ Result[C]] = poly.at { c ⇒ s ⇒
    def mkError(failed: In): Result[C] = Xor.left(basis.inverse(Right(onInvariantFailed(failed))))
    f(MonadicCommandContext(c, s)).run.map(_._1).flatMap { events ⇒
      val s2 = events.foldLeft(s)((s2, e) ⇒ applyEvent(e)(s2))
      invariants.find(inv ⇒ !inv(s2)).map(mkError).getOrElse(Xor.right(events))
    }
  }

  implicit def listMonoid = list.listAlgebra[Events]
  type CommandMonad[C <: Cmd, A] = WriterT[Xor[C#Errors, ?], List[Events], A]
}