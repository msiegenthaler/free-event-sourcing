package slfes

import shapeless.ops.coproduct.Inject
import slfes.BoundedContext.CommandForIdInList

import scala.language.higherKinds
import cats.Monad
import cats.free.Free
import shapeless.LUBConstraint.<<:
import shapeless.{Coproduct, HList, ::}
import shapeless.ops.hlist.Selector
import slfes.ProcessBodyAction.CommandResult

/** Model of a subdomain of the problem (i.e. inventory or customer relationship mangement). */
sealed trait BoundedContext {
  type Aggregates <: HList
  type Processes <: HList

  type Contains[A <: AggregateInterface] = Selector[Aggregates, A]

  type Monad[A] = BoundedContext.BoundedContextM[this.type, A]
  type Action[+A] = BoundedContextAction[this.type, A]
  type Exec[F[_]] = cats.free.Inject[Action, F]
  type BoundedContext = this.type

  def noop[F[_] : Exec]: Free[F, Unit] = Monad[Free[F, ?]].pure(())

  def exec[F[_] : Exec, I, C <: Cmd](to: I, command: C)
    (implicit cfi: CommandForIdInList[Aggregates, I, C]): Free[F, CommandResult[C]] = {
    val action = BoundedContextAction.Command[BoundedContext, cfi.Aggregate, C](to, command)(cfi.aggregateSelector, ???)
    lift[F, CommandResult[C]](action)
  }

  private def lift[F[_], A](op: Action[A])(implicit inject: cats.free.Inject[Action, F]) = Free.liftF(inject.inj(op))
}
object BoundedContext {
  type BoundedContextM[BC <: BoundedContext, A] = Free[BoundedContextAction[BC, ?], A]


  sealed trait CommandForIdInList[L <: HList, I, C <: Cmd] {
    type Command <: Coproduct
    type Aggregate <: slfes.Aggregate {type Id = I}
    implicit val inject: Inject[Command, C]
    implicit val aggregateSelector: Selector[L, Aggregate]
    def apply(command: C): Command = inject(command)
    def aggregate(l: L): Aggregate = aggregateSelector(l)
  }
  object CommandForIdInList {
    type Aux[L <: HList, I, C <: Cmd, A <: slfes.Aggregate {type Id = I}, CP <: Coproduct] = CommandForIdInList[L, I, C] {
      type Command = CP
      type Aggregate = A
    }
    implicit def valid[L <: HList, I, C <: Cmd, CP <: Coproduct](ev: CommandsFromIdInList.Aux[L, I, CP],
      i: Inject[CP, C]): Aux[L, I, C, ev.Aggregate, CP] = new CommandForIdInList[L, I, C] {
      type Command = CP
      type Aggregate = ev.Aggregate
      val inject = i
      val aggregateSelector = ev.selector
    }
  }

  sealed trait CommandsFromIdInList[L <: HList, I] {
    type Aggregate <: slfes.Aggregate {type Id = I}
    type Commands <: Coproduct
    implicit val selector: Selector[L, Aggregate]
    def aggregate(l: L): Aggregate = selector(l)
  }
  object CommandsFromIdInList {
    def apply[L <: HList, Id](implicit a: CommandsFromIdInList[L, Id]): CommandsFromIdInList[L, Id] = a
    type Aux[L <: HList, I, C <: Coproduct] = CommandsFromIdInList[L, I] {type Commands = C}
    type Aux2[L <: HList, I, C <: Coproduct, A <: Aggregate {type Id = I}] = CommandsFromIdInList[L, I] {
      type Commands = C
      type Aggregate = A
    }
    implicit def head[H <: Aggregate {type Id = I}, T <: HList, I]: Aux2[H :: T, I, H#Command, H] = new CommandsFromIdInList[H :: T, I] {
      type Commands = H#Command
      type Aggregate = H
      val selector = Selector.select[H, T]
    }
    implicit def tail[H, T <: HList, I](implicit
      ev: CommandsFromIdInList[T, I]): Aux2[H :: T, I, ev.Commands, ev.Aggregate] = new CommandsFromIdInList[H :: T, I] {
      override type Aggregate = ev.Aggregate
      override type Commands = ev.Commands
      val selector = new Selector[H :: T, Aggregate] {
        def apply(l: H :: T) = ev.selector(l.tail)
      }
    }
  }
}

case class BoundedContextType[AS <: HList : <<:[AggregateType]#λ , PS <: HList : <<:[ProcessType]#λ](name: String, aggregates: AS, processes: PS) {

  def boundedContext = new BoundedContext {
    override type Aggregates = AS
    override type Processes = PS
  }
}

sealed trait BoundedContextAction[BC <: BoundedContext, +A]
object BoundedContextAction {
  case class Command[BC <: BoundedContext, A <: Aggregate : BC#Contains, C <: Cmd : A#IsCommand](to: A#Id,
    command: C)
    extends BoundedContextAction[BC, CommandResult[C]]

}
