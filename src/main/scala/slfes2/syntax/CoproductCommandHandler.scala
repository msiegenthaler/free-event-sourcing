package slfes2.syntax

import scala.annotation.implicitNotFound
import cats.data.Xor
import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Poly1 }
import slfes2.AggregateCommand
import slfes2.AggregateImplementation.CommandHandler
import slfes2.syntax.CoproductCommandHandler.HandleCommand

trait CoproductCommandHandler[Command <: AggregateCommand, S, Event] extends Poly1 with CommandHandler[S, Command, Event] {
  type State = S

  /** Always implement as: doHandle(command).apply(state). You need to implement the method because of the implicit resolution. */
  def handle[C <: Command](command: C, state: State): C#Error Xor Seq[Event]

  final def apply[C <: Command](command: C, state: State) = handle(command, state).map(_.toList)

  def doHandle[C <: Command, CC <: Coproduct](
    command: C
  )(implicit
    generic: Generic.Aux[Command, CC],
    folder: HandleCommand[this.type, CC, C, Command, State, Event]): State ⇒ C#Error Xor Seq[Event] = {
    val cc = generic.to(command)
    cc.fold(this)
  }
}
object CoproductCommandHandler {
  @implicitNotFound("Not all commands (subclasses of ${Command}) are handled in ${X}.")
  type HandleCommand[X <: Poly1, CC <: Coproduct, C <: Command, Command <: AggregateCommand, State, Event] = Folder.Aux[X, CC, State ⇒ C#Error Xor Seq[Event]]
}