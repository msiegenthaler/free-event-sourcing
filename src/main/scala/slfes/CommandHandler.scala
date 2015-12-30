package slfes

import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless.{Coproduct, Poly1}

trait CommandHandler[State, Commands <: Coproduct, Events <: Coproduct] extends Poly1 {
  val plain = CommandSyntax[State, Commands, Events, this.type](this)
  val monadic = MonadicCommandSyntax[State, Commands, Events, this.type](this)
}

trait CommandHandlerWithInvariants[State, Commands <: Coproduct, Events <: Coproduct, Invariant <: Inv[State], InvariantError] extends Poly1 {
  def invariants: Traversable[Invariant]
  def invariantError(failed: Invariant): InvariantError
  def applyEvent: Events ⇒ State ⇒ State

  val plain = InvariantCommandSyntax[State, Commands, Events, Invariant, InvariantError, this.type](applyEvent, invariants, invariantError)(this)
  val monadic = InvariantMonadicCommandSyntax[State, Commands, Events, Invariant, InvariantError, this.type](applyEvent, invariants, invariantError)(this)
}