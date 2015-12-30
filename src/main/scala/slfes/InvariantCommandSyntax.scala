package slfes

import scala.language.implicitConversions
import cats.std.list
import cats.data.{WriterT, Xor}
import shapeless.ops.coproduct.Inject
import shapeless.{Poly1, Coproduct}


case class InvariantCommandSyntax[State, Commands <: Coproduct, Events <: Coproduct, In <: Inv[State], InErr, T <: Poly1](applyEvent: Events ⇒ State ⇒ State, invariants: Traversable[In], onInvariantFailed: In ⇒ InErr)(val poly: T) {
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]

  def on[C <: Cmd : IsCommand](f: C ⇒ State ⇒ Result[C])(implicit inject: Inject[C#Errors, InErr]): poly.Case.Aux[C, State ⇒ Result[C]] = poly.at { c ⇒ s ⇒
    f(c)(s).flatMap { events ⇒
      val s2 = events.foldLeft(s)((s2, e) ⇒ applyEvent(e)(s2))
      invariants.find(inv ⇒ !inv(s2)).
        fold[Result[C]](Xor.right(events))(failed ⇒ Xor.left(inject(onInvariantFailed(failed))))
    }
  }

  //TODO `on` that overwrites onInvariantFailed

  implicit def commandSyntaxResponse[E <: Coproduct](cmd: Cmd {type Errors = E}): CommandSyntaxResponse[E, Events] =
    CommandSyntaxResponse(cmd)
}

case class InvariantMonadicCommandSyntax[State, Commands <: Coproduct, Events <: Coproduct, In <: Inv[State], InErr, T <: Poly1](applyEvent: Events ⇒ State ⇒ State, invariants: Traversable[In], onInvariantFailed: In ⇒ InErr)(val poly: T) {
  type Result[C <: Cmd] = Xor[C#Errors, Seq[Events]]
  type IsCommand[C] = Inject[Commands, C]

  def onM[C <: Cmd : IsCommand](f: MonadicCommandContext[State, Events, C] ⇒ CommandMonad[C, Unit])(implicit inject: Inject[C#Errors, InErr]): poly.Case.Aux[C, State ⇒ Result[C]] = poly.at { c ⇒ s ⇒
    f(MonadicCommandContext(c, s)).run.map(_._1).flatMap { events ⇒
      val s2 = events.foldLeft(s)((s2, e) ⇒ applyEvent(e)(s2))
      invariants.find(inv ⇒ !inv(s2)).
        fold[Result[C]](Xor.right(events))(failed ⇒ Xor.left(inject(onInvariantFailed(failed))))
    }
  }

  //TODO `onM` that overwrites onInvariantFailed

  implicit def listMonoid = list.listAlgebra[Events]
  type CommandMonad[C <: Cmd, A] = WriterT[Xor[C#Errors, ?], List[Events], A]
}
