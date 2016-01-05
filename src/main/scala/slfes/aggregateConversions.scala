package slfes

import shapeless.ops.hlist.Selector
import shapeless.{::, HList, Coproduct}
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


/** Checks if the Id and the Command belong to the same aggregate. The aggragate type must be made visible in the
  * implicit scope. */
sealed trait CommandForId[I, C <: Cmd] {
  type Command <: Coproduct
  type Aggregate <: AggregateInterface.AuxIC[I, Command]
  val inject: Inject[Aggregate#Command, C]
  def apply(command: C): Aggregate#Command = inject(command)
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
  type Aggregate <: AggregateInterface.AuxIC[Id, Command]
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


/** Checks if the Id and the Command belong to the same aggregate the is a member of the list L. */
sealed trait CommandForIdInList[L <: HList, Id, C <: Cmd] {
  type Aggregate <: AggregateInterface.WithId[Id]
  implicit val inject: Inject[Aggregate#Command, C]
  implicit val aggregateSelector: Selector[L, Aggregate]
  def apply(command: C): Aggregate#Command = inject(command)
  def aggregate(l: L): Aggregate = aggregateSelector(l)
}
object CommandForIdInList {
  type Aux[L <: HList, I, C <: Cmd, A <: slfes.AggregateInterface.WithId[I]] = CommandForIdInList[L, I, C] {
    type Aggregate = A
  }
  implicit def valid[L <: HList, I, C <: Cmd, CP <: Coproduct](ev: CommandsFromIdInList.Aux[L, I, CP],
    i: Inject[CP, C]): Aux[L, I, C, ev.Aggregate] = new CommandForIdInList[L, I, C] {
    type Command = CP
    type Aggregate = ev.Aggregate
    val inject = new Inject[Aggregate#Command, C] {
      def apply(c: C) = ev.convert(i(c))
    }
    val aggregateSelector = ev.selector
  }
}
/** Helper for CommandForIdInList. Extracts the command coproduct of the aggregate with the given Id-type. */
sealed trait CommandsFromIdInList[L <: HList, Id] {
  type Aggregate <: AggregateInterface.WithId[Id]
  type Commands <: Coproduct
  implicit val selector: Selector[L, Aggregate]
  def aggregate(l: L): Aggregate = selector(l)
  def convert(c: Commands): Aggregate#Command
}
object CommandsFromIdInList {
  def apply[L <: HList, Id](implicit a: CommandsFromIdInList[L, Id]): CommandsFromIdInList[L, Id] = a
  type Aux[L <: HList, I, C <: Coproduct] = CommandsFromIdInList[L, I] {type Commands = C}
  type Aux2[L <: HList, I, C <: Coproduct, A <: AggregateInterface.WithId[I]] = CommandsFromIdInList[L, I] {
    type Commands = C
    type Aggregate = A
  }
  implicit def head[H <: AggregateInterface.WithId[I], T <: HList, I]: Aux2[H :: T, I, H#Command, H] = new CommandsFromIdInList[H :: T, I] {
    type Commands = H#Command
    type Aggregate = H
    val selector = Selector.select[H, T]
    def convert(c: Commands): Aggregate#Command = c
  }
  implicit def tail[H, T <: HList, I](implicit
    ev: CommandsFromIdInList[T, I]): Aux2[H :: T, I, ev.Commands, ev.Aggregate] = new CommandsFromIdInList[H :: T, I] {
    override type Aggregate = ev.Aggregate
    override type Commands = ev.Commands
    val selector = new Selector[H :: T, Aggregate] {
      def apply(l: H :: T) = ev.selector(l.tail)
    }
    def convert(c: Commands) = ev.convert(c)
  }
}
