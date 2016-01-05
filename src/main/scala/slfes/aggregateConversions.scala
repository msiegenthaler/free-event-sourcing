package slfes

import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

/** Gets the aggregate from the type of the id. */
trait AggregateFromId[I] {
  type Out <: AggregateInterface {type Id = I}
  def aggregate: Out
}
object AggregateFromId {
  def apply[Id](implicit a: AggregateFromId[Id]): AggregateFromId[Id]#Out = a.aggregate
  type Aux[A, I] = AggregateFromId[I] {type Out = A}
  implicit def fromId[I](implicit a: AggregateInterface {type Id = I}): Aux[a.type, I] = new AggregateFromId[I] {
    type Out = a.type
    def aggregate = a
  }
}


/** Gets the aggregate from the type of the command (the coproduct). */
trait AggregateFromCommands[C <: Coproduct] {
  type Out <: AggregateInterface {type Command = C}
  def aggregate: Out
}
object AggregateFromCommands {
  def apply[C <: Coproduct](implicit a: AggregateFromCommands[C]): AggregateFromCommands[C]#Out = a.aggregate
  type Aux[A, C <: Coproduct] = AggregateFromCommands[C] {type Out = A}
  implicit def fromCommands[A <: AggregateInterface {type Command = C}, C <: Coproduct](implicit a: A): Aux[A, C] = new AggregateFromCommands[C] {
    type Out = A
    def aggregate = a
  }
}

/** Gets the commands from the type of the aggregate's id. */
trait CommandsFromId[I] {
  type Commands <: Coproduct
  type Aggregate <: slfes.AggregateInterface {type Id = I; type Command = Commands}
  def aggregate: Aggregate
}
object CommandsFromId {
  def apply[Id](implicit a: CommandsFromId[Id]): CommandsFromId[Id]#Aggregate = a.aggregate
  type Aux[I, C <: Coproduct] = CommandsFromId[I] {type Commands = C}
  type Aux2[I, C <: Coproduct, A <: AggregateInterface {type Id = I; type Command = C}] = CommandsFromId[I] {type Commands = C; type Aggregate = A}
  implicit def fromId[I](implicit a: AggregateInterface {type Id = I}): Aux2[I, a.Command, a.type] = new CommandsFromId[I] {
    type Commands = a.Command
    type Aggregate = a.type
    def aggregate = a
  }
}

/** Gets the events from the type of the aggregate's id. */
trait EventsFromId[I] {
  type Events <: Coproduct
  type Aggregate <: slfes.AggregateInterface {type Id = I; type Event = Events}
  def aggregate: Aggregate
}
object EventsFromId {
  def apply[Id](implicit a: EventsFromId[Id]): EventsFromId[Id]#Aggregate = a.aggregate
  type Aux[I, E <: Coproduct] = EventsFromId[I] {type Events = E}
  type Aux2[I, E <: Coproduct, A <: AggregateInterface {type Id = I; type Event = E}] = EventsFromId[I] {type Events = E; type Aggregate = A}
  implicit def fromId[I](implicit a: AggregateInterface {type Id = I}): Aux2[I, a.Event, a.type] = new EventsFromId[I] {
    type Events = a.Event
    type Aggregate = a.type
    def aggregate = a
  }
}

/** Checks that a command applies to an aggregate and allows lifting the command. */
sealed trait CommandForAggregate[A <: AggregateInterface, C <: Cmd] {
  def inject(command: C): A#Command
}
object CommandForAggregate {
  def apply[A <: AggregateInterface, C <: Cmd](implicit cfa: CommandForAggregate[A, C]) = cfa
  implicit def valid[A <: AggregateInterface, C <: Cmd](implicit i: Inject[A#Command, C]): CommandForAggregate[A, C] = new CommandForAggregate[A, C] {
    def inject(command: C) = i(command)
  }
  implicit def fromCfi[I, C <: Cmd](implicit cfi: CommandForId[I, C]) = cfi.toCfa
}

/** Same as CommandForAggregate, but on aggregate's id instead of on the aggregate itself. */
sealed trait CommandForId[I, C <: Cmd] {
  type Aggregate <: slfes.AggregateInterface {type Id = I}
  def inject(command: C): Aggregate#Command
  private def inject2(command: C) = inject(command)
  def toCfa[A <: Aggregate {type Command = Aggregate#Command}] = {
    new CommandForAggregate[A, C] {
      def inject(command: C) = inject2(command)
    }
  }
}
object CommandForId {
  def apply[Id, C <: Cmd](implicit cfi: CommandForId[Id, C]) = cfi
  type Aux[I, C <: Cmd, A <: AggregateInterface {type Id = I}] = CommandForId[I, C] {type Aggregate = A}
  implicit def valid[Id, C <: Cmd, CP <: Coproduct](implicit ev: CommandsFromId[Id], i: Inject[CP, C]): Aux[Id, C, ev.Aggregate] = ???
}