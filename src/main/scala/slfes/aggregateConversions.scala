package slfes

import shapeless.Coproduct

/** Gets the aggregate from the type of the id. */
trait AggregateFromId[I] {
  type Out <: Aggregate {type Id = I}
  def aggregate: Out
}
object AggregateFromId {
  def apply[Id](implicit a: AggregateFromId[Id]): AggregateFromId[Id]#Out = a.aggregate
  type Aux[A, I] = AggregateFromId[I] {type Out = A}
  implicit def fromId[I](implicit a: Aggregate {type Id = I}): Aux[a.type, I] = new AggregateFromId[I] {
    type Out = a.type
    def aggregate = a
  }
}


/** Gets the aggregate from the type of the command (the coproduct). */
trait AggregateFromCommands[C <: Coproduct] {
  type Out <: Aggregate {type Command = C}
  def aggregate: Out
}
object AggregateFromCommands {
  def apply[C <: Coproduct](implicit a: AggregateFromCommands[C]): AggregateFromCommands[C]#Out = a.aggregate
  type Aux[A, C <: Coproduct] = AggregateFromCommands[C] {type Out = A}
  implicit def fromCommands[A <: Aggregate {type Command = C}, C <: Coproduct](implicit a: A): Aux[A, C] = new AggregateFromCommands[C] {
    type Out = A
    def aggregate = a
  }
}


/** Gets the commands from the type of the aggregate's id. */
trait CommandsFromId[I] {
  type Commands <: Coproduct
  type Aggregate <: slfes.Aggregate {type Id = I; type Command = Commands}
  def aggregate: Aggregate
}
object CommandsFromId {
  type Aux[I, C <: Coproduct] = CommandsFromId[I] {type Commands = C}
  type Aux2[I, C <: Coproduct, A <: Aggregate {type Id = I; type Command = C}] = CommandsFromId[I] {type Commands = C; type Aggregate = A}
  implicit def fromId[I](implicit a: Aggregate {type Id = I}): Aux2[I, a.Command, a.type] = new CommandsFromId[I] {
    type Commands = a.Command
    type Aggregate = a.type
    def aggregate = a
  }
}