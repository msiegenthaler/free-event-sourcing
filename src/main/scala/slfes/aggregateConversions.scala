package slfes

import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

/** Gets the aggregate from the type of the id. */
trait AggregateFromId[Id] {
  type Out <: AggregateInterface.WithId[Id]
  def aggregate: Out
}
object AggregateFromId {
  def apply[Id](implicit a: AggregateFromId[Id]): AggregateFromId[Id]#Out = a.aggregate
  type Aux[A <: AggregateInterface.WithId[I], I] = AggregateFromId[I] {type Out = A}
  implicit def fromId[Id](implicit a: AggregateInterface.WithId[Id]): Aux[a.type, Id] = new AggregateFromId[Id] {
    type Out = a.type
    def aggregate = a
  }
}


/** Same as CommandForAggregate, but on aggregate's id instead of on the aggregate itself. */
sealed trait CommandForId[I, C <: Cmd] {
  type Command <: Coproduct
  type Aggregate <: slfes.AggregateInterface.AuxIC[I, Command]
  val inject: Inject[Aggregate#Command, C]
  def apply(command: Aggregate#Command): Aggregate#Command = inject(command)
}
object CommandForId {
  def apply[Id, C <: Cmd](implicit cfi: CommandForId[Id, C]) = cfi
  type Aux[I, C <: Cmd, A <: AggregateInterface.AuxIC[I, C], CP <: Coproduct] = CommandForId[I, C] {
    type Aggregate = A
    type Cmds = CP
  }
  implicit def valid[Id, C <: Cmd, CP <: Coproduct](implicit ev: CommandsFromId.Aux[Id, CP],
    i: Inject[CP, C]): Aux[Id, C, ev.Aggregate, CP] = new CommandForId[Id, C] {
    type Cmds = CP
    type Aggregate = ev.Aggregate
    val inject = i
  }
}
/** Gets the commands from the type of the aggregate's id. */
trait CommandsFromId[Id] {
  type Command <: Coproduct
  type Aggregate <: slfes.AggregateInterface.AuxIC[Id, Command]
  def aggregate: Aggregate
}
object CommandsFromId {
  def apply[Id](implicit a: CommandsFromId[Id]): CommandsFromId[Id]#Aggregate = a.aggregate
  type Aux[I, C <: Coproduct] = CommandsFromId[I] {type Commands = C}
  type Aux2[I, C <: Coproduct, A <: AggregateInterface.AuxIC[I, C]] = CommandsFromId[I] {type Command = C; type Aggregate = A}
  implicit def fromId[I](implicit a: AggregateInterface.WithId[I]): Aux2[I, a.Command, a.type] = new CommandsFromId[I] {
    type Command = a.Command
    type Aggregate = a.type
    def aggregate = a
  }
}

