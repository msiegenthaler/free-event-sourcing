package freeeventsourcing.syntax

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import cats.data.Xor
import freeeventsourcing.AggregateCommand
import freeeventsourcing.AggregateImplementation.CommandHandler
import freeeventsourcing.syntax.CoproductCommandHandler.CommandType
import shapeless.{ :+:, CNil, Coproduct, Generic, HMap }

//TODO fix parameter order
trait CoproductCommandHandler[Cmd <: AggregateCommand, S, Evt]
    extends CommandHandler[S, Cmd, Evt] with PlainCommandHandlerSyntax[Cmd, Evt, S] with MonadicCommandHandlerSyntax[Cmd, Evt, S] {
  protected type State = S
  protected type Handler[C <: Cmd] = State ⇒ C#Error Xor Seq[Evt]

  /** Always implement as: doHandle(command).apply(state). You need to implement the method because of the implicit resolution. */
  def handle[C <: Cmd](command: C, state: State): C#Error Xor Seq[Evt]

  final override def apply[C <: Cmd](command: C, state: State) = handle(command, state).map(_.toList)

  def doHandle[C <: Cmd, CP <: Coproduct](command: C)(
    implicit
    g: Generic.Aux[Cmd, CP],
    m: CommandMap[Cmd],
    me: CommandToCommandCase[CommandType[C], C ⇒ Handler[C]]
  ): Handler[C] = {
    m.get(CommandType.fromInstance(command))
      .map(_.apply(command))
      .getOrElse(throw new AssertionError("Unhandled command: Probably an invalid command structure."))
  }

  protected[this] def commandCase[C <: Cmd](f: C ⇒ Handler[C]): CommandCase[C] = f

  protected type CommandCase[C <: Cmd] = C ⇒ Handler[C]

  protected[this] class CommandToCommandCase[K, V] private[CoproductCommandHandler] ()
  protected[this] implicit final def cmdToError[C <: Cmd] = new CommandToCommandCase[CommandType[C], C ⇒ Handler[C]]
  @implicitNotFound("Not all commands (subclasses of sealed trait ${C}) are handled.")
  protected type CommandMap[C <: AggregateCommand] = HMap[CommandToCommandCase]

  /** Build a map from Command to CommandCase from the Coproduct and our implicit CommandCases */
  protected[this] implicit final def commandCaseMap[CP <: Coproduct](implicit g: Generic.Aux[Cmd, CP], m: CommandMapBuilder[CP]) =
    m.get

  /** Builds the CommandMap from the implicit CommandCase(s). */
  protected[this] sealed trait CommandMapBuilder[CP <: Coproduct] {
    def get: CommandMap[Cmd]
  }
  protected[this] object CommandMapBuilder {
    implicit def cnil = new CommandMapBuilder[CNil] {
      def get = HMap.empty
    }
    implicit def lr[L <: Cmd: ClassTag, R <: Coproduct](implicit l: CommandCase[L], r: CommandMapBuilder[R]) = new CommandMapBuilder[L :+: R] {
      def get = r.get + (CommandType.fromType[L], l)
    }
  }
}
object CoproductCommandHandler {
  case class CommandType[C <: AggregateCommand] private (tpe: String)
  object CommandType {
    def fromInstance[C <: AggregateCommand](command: C): CommandType[C] =
      CommandType(command.getClass.getName)
    def fromType[C <: AggregateCommand](implicit t: ClassTag[C]): CommandType[C] =
      CommandType(t.runtimeClass.getName)
  }
}