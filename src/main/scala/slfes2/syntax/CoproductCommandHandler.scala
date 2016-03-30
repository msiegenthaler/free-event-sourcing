package slfes2.syntax

import cats.data.Xor
import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Poly1 }

import scala.annotation.implicitNotFound

abstract class CoproductCommandHandler[Command <: { type Error <: Coproduct }, Event] extends Poly1 {
  def handle[C <: Command, CC <: Coproduct](
    command: C
  )(implicit
    generic: Generic.Aux[Command, CC],
    folder: Handler[CC, C]): C#Error Xor Seq[Event] = {
    val cc = generic.to(command)
    cc.fold(this)
  }

  @implicitNotFound("Not all commands for the aggregate are handled.")
  type Handler[CC <: Coproduct, C <: Command] = Folder.Aux[this.type, CC, C#Error Xor Seq[Event]]
}