package slfes

import scala.language.higherKinds
import cats.Monad
import cats.data.Xor
import cats.free.Free
import shapeless.LUBConstraint.<<:
import shapeless.{ ::, HList }
import shapeless.ops.coproduct.Inject
import shapeless.ops.hlist.{ Mapper, Selector }
import slfes.BoundedContextAction.CommandResult

sealed trait BoundedContextDefinition[AS <: HList, PS <: HList] {
  val name: String
  val aggregates: Aggregates
  val processes: Processes

  type Aggregates <: AS
  type Processes <: PS
  type AggregateInterfaces <: HList

  protected val aggregateInterfaces: AggregateInterfaces
  private def outer = this
  val boundedContext = new BoundedContextType {
    type Interface = BoundedContext.Aux[AggregateInterfaces, PS]
    val interface = new BoundedContext {
      type Aggregates = AggregateInterfaces
      val aggregates = aggregateInterfaces
    }
  }
}
object BoundedContextDefinition {
  def apply[AS <: HList: <<:[AggregateType]#λ, PS <: HList: <<:[ProcessType]#λ](name: String, aggregates: AS,
    processes: PS)(implicit ai: Mapper[AggregateType.ToAggregateInterface.type, AS]) = {
    def n = name
    def a = aggregates
    def p = processes
    new BoundedContextDefinition[AS, PS] {
      val name = n
      val aggregates = a
      val processes = p
      protected val aggregateInterfaces = aggregates.map(AggregateType.ToAggregateInterface)
      type Aggregates = AS
      type Processes = PS
      type AggregateInterfaces = ai.Out
    }
  }
}

sealed trait BoundedContextType {
  type Interface <: BoundedContext
  val interface: Interface
}

/** Model of a subdomain of the problem (i.e. inventory or customer relationship mangement). */
sealed trait BoundedContext {
  type Aggregates <: HList
  val aggregates: Aggregates

  type IsAggregate[A <: Aggregate] = Selector[Aggregates, A]

  type Monad[A] = BoundedContext.BoundedContextM[this.type, A]
  type Action[+A] = BoundedContextAction[this.type, A]
  type Exec[F[_]] = cats.free.Inject[Action, F]
  type BoundedContext = this.type

  def noop[F[_]: Exec]: Free[F, Unit] = Monad[Free[F, ?]].pure(())

  def execute[F[_]: Exec, I, C <: Cmd](to: I, command: C)(implicit cfi: CommandForIdInList[Aggregates, I, C]): Free[F, CommandResult[C]] = {
    import cfi._
    val action = BoundedContextAction.Command[BoundedContext, cfi.Aggregate, C](to, command)
    lift[F, CommandResult[C]](action)
  }

  private def lift[F[_], A](op: Action[A])(implicit inject: cats.free.Inject[Action, F]) = Free.liftF(inject.inj(op))
}
object BoundedContext {
  type Aux[A <: HList, P <: HList] = BoundedContext {
    type Aggregates = A
  }

  type BoundedContextM[BC <: BoundedContext, A] = Free[BoundedContextAction[BC, ?], A]
}

sealed trait BoundedContextAction[BC <: BoundedContext, +A]
object BoundedContextAction {
  case class Command[BC <: BoundedContext, A <: Aggregate: BC#IsAggregate, C <: Cmd: Inject[A#Command, ?]](
    to: A#Id,
    command: C
  ) extends BoundedContextAction[BC, CommandResult[C]]
  type CommandResult[C <: Cmd] = C#Errors Xor Unit
}