package freeeventsourcing.syntax

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq
import cats.data.Xor
import shapeless.ops.coproduct.Folder
import shapeless.{ Coproduct, Generic, Poly1 }
import freeeventsourcing.AggregateCommand
import freeeventsourcing.AggregateImplementation.CommandHandler
import freeeventsourcing.syntax.CoproductCommandHandler.HandleCommand

trait CoproductCommandHandler[Cmd <: AggregateCommand, S, Evt] extends Poly1
    with CommandHandler[S, Cmd, Evt] with PlainCommandHandlerSyntax with MonadicCommandHandlerSyntax {
  protected type Command = Cmd
  protected type Event = Evt
  protected type State = S

  /** Always implement as: doHandle(command).apply(state). You need to implement the method because of the implicit resolution. */
  def handle[C <: Cmd](command: C, state: State): C#Error Xor Seq[Evt]

  final override def apply[C <: Cmd](command: C, state: State) = handle(command, state).map(_.toList)

  def doHandle[C <: Cmd, CC <: Coproduct](command: C)(implicit
    generic: Generic.Aux[Cmd, CC],
    folder: HandleCommand[this.type, CC, C, Cmd, State, Evt]): State ⇒ C#Error Xor Seq[Evt] = {
    val cc = generic.to(command)
    cc.fold(this)
  }
}
object CoproductCommandHandler {
  @implicitNotFound("Not all commands (subclasses of ${Command}) are handled in ${X}.")
  type HandleCommand[X <: Poly1, CC <: Coproduct, C <: Command, Command <: AggregateCommand, State, Event] = Folder.Aux[X, CC, State ⇒ C#Error Xor Seq[Event]]
}