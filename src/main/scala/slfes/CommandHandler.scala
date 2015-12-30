package slfes

import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless.{Coproduct, Poly1}
import shapeless.ops.coproduct.Inject

trait CommandHandler[State, Commands <: Coproduct, Events <: Coproduct] extends Poly1 {
  val plain = CommandSyntax[State, Commands, Events, this.type](this)
  val monadic = MonadicCommandSyntax[State, Commands, Events, this.type](this)
}

trait CommandHandlerWithInvariants[State, Commands <: Coproduct, Events <: Coproduct, Invariant <: Inv[State], InvariantErrors <: Coproduct] extends Poly1 {
  def invariants: Traversable[Invariant]
  def invariantError(failed: Invariant): InvariantErrors
  def applyEvent: Events ⇒ State ⇒ State

  implicit def injectInvariantError[E](e: E)(implicit inject: Inject[InvariantErrors, E]): InvariantErrors =
    inject(e)

  val plain = InvariantCommandSyntax[State, Commands, Events, Invariant, InvariantErrors, this.type](applyEvent, invariants, invariantError)(this)
  val monadic = InvariantMonadicCommandSyntax[State, Commands, Events, Invariant, InvariantErrors, this.type](applyEvent, invariants, invariantError)(this)
}