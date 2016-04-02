package slfes2.syntax

import cats.data.Xor
import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Poly1 }
import slfes2.syntax.CoproductCommandHandler.HandleCommand

import scala.annotation.implicitNotFound

abstract class CoproductCommandHandler[Command <: { type Error <: Coproduct }, Event] extends Poly1 {
  def handle[C <: Command, CC <: Coproduct](
    command: C
  )(implicit
    generic: Generic.Aux[Command, CC],
    folder: HandleCommand[this.type, CC, C, Command, Event]): C#Error Xor Seq[Event] = {
    val cc = generic.to(command)
    cc.fold(this)
  }
}
object CoproductCommandHandler {
  @implicitNotFound("Not all commands (subclasses of ${Command}) are handled in ${X}.")
  type HandleCommand[X <: Poly1, CC <: Coproduct, C <: Command, Command <: { type Error <: Coproduct }, Event] = Folder.Aux[X, CC, C#Error Xor Seq[Event]]
}