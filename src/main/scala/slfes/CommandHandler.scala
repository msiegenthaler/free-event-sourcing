package slfes

import scala.language.higherKinds
import cats.data.Xor
import shapeless._
import shapeless.ops.coproduct.Inject


trait CommandHandler[Commands <: Coproduct] extends Poly1 {
  implicit class CommandResponse[Errors <: Coproduct](val cmd: Command[Errors]) {
    def success: cmd.Result = Xor.right(())
    def fail[Error](error: Error)(implicit inject: Inject[Errors, Error]): cmd.Result =
      Xor.Left(inject(error))
  }

  def on[Cmd <: {type Result}](f: Cmd ⇒ Cmd#Result)(implicit ev: Inject[Commands, Cmd]): Case.Aux[Cmd, Cmd#Result] = at(f)
}
