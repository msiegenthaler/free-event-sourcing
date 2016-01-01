package slfes

import cats.data.Xor
import cats.free.Free
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

object Process {
  import ProcessAction._

  /** Receive Events from the aggregate with the specified id. */
  def receive[Id](from: Id)(implicit a: AggregateFromId[Id]) =
    Free.liftF[ProcessAction, a.Out#Event](Receive[a.Out](from))

  /** Receive Events from the aggregate with the specified id.
        Use this if the automatic conversion from Id to Aggregate does not work. */
  def receive[A <: Aggregate](from: A#Id) =
    Free.liftF[ProcessAction, A#Event](Receive(from))

  /** Send a command to an aggregate. */
  def execute[A <: Aggregate, C <: Cmd : Inject[A#Command, ?]](to: A#Id, command: C) =
    Free.liftF[ProcessAction, CommandResult[Cmd]](Command(to, command))

  /** Send a command to an aggregate.
        Use this if the automatic conversion from Id to Aggregate does not work. */
  def execute[Id, C <: Cmd, CS <: Coproduct](to: Id, command: C)(implicit ev: CommandsFromId.Aux[Id, CS], i: Inject[CS, C]) =
    Free.liftF[ProcessAction, CommandResult[Cmd]](Command[ev.Aggregate, C](to, command)(i))

  def end = Free.liftF(End)

}

sealed trait ProcessAction[+A]
object ProcessAction {
  type CommandResult[C <: Cmd] = C#Errors Xor Unit

  case class Receive[A <: Aggregate](from: A#Id) extends ProcessAction[A#Event]
  case class Command[A <: Aggregate, C <: Cmd](to: A#Id, command: C)(implicit i: Inject[A#Command, C]) extends ProcessAction[CommandResult[C]]
  case object End extends ProcessAction[Nothing]
}