package slfes

import scala.language.higherKinds
import scala.language.implicitConversions
import shapeless.{Coproduct, Poly1}

trait CommandHandler[State, Commands <: Coproduct, Events <: Coproduct] extends Poly1 {
  val plain = CommandSyntax[State, Commands, Events, this.type](this)
  val monadic = MonadicCommandSyntax[State, Commands, Events, this.type](this)
}