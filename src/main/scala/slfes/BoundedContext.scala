package slfes

import cats.Monad
import cats.data.Xor
import cats.free.Free
import shapeless.HList
import shapeless.LUBConstraint.<<:
import shapeless.ops.coproduct.Inject
import shapeless.ops.hlist.{ Mapper, Selector }
import slfes.BoundedContextAction.CommandResult

import scala.language.higherKinds

sealed trait BoundedContextDefinition[AS <: HList, PS <: HList] {
  val name: String
  val aggregates: Aggregates
  val processes: Processes

  type Aggregates <: AS
  type Processes <: PS
  type AggregateInterfaces <: HList
  type AggregateImplementations <: HList
  type ProcessImplementations <: HList

  protected val aggregateInterfaces: AggregateInterfaces
  protected val aggregateImplementations: AggregateImplementations
  protected val processImplementations: ProcessImplementations
  private def outer = this
  val boundedContext = new BoundedContextType {
    type Interface = BoundedContext.Aux[AggregateInterfaces, PS]
    val interface = new BoundedContext {
      val name = outer.name
      type Aggregates = AggregateInterfaces
      val aggregates = aggregateInterfaces
    }
    type Implementation = BoundedContextImplementation.Aux[AggregateImplementations, ProcessImplementations]
    val implementation = new BoundedContextImplementation {
      val name = outer.name
      type Aggregates = AggregateImplementations
      val aggregates = aggregateImplementations
      def aggregatesUnified = ??? //TODO
      type Processes = ProcessImplementations
      val processes = processImplementations
    }
  }
}
object BoundedContextDefinition {
  def apply[AS <: HList: <<:[AggregateType]#λ, PS <: HList: <<:[ProcessType]#λ](
    name: String,
    aggregates: AS,
    processes: PS
  )(implicit
    ai: Mapper[AggregateType.ToInterface.type, AS],
    aimpl: Mapper[AggregateType.ToImplementation.type, AS],
    pimpl: Mapper[ProcessType.ToImplementation.type, PS]) = {
    def n = name
    def a = aggregates
    def p = processes
    new BoundedContextDefinition[AS, PS] {
      val name = n
      val aggregates = a
      val processes = p
      protected val aggregateInterfaces = aggregates.map(AggregateType.ToInterface)
      protected val aggregateImplementations = aggregates.map(AggregateType.ToImplementation)
      protected val processImplementations = processes.map(ProcessType.ToImplementation)
      type Aggregates = AS
      type Processes = PS
      type AggregateInterfaces = ai.Out
      type AggregateImplementations = aimpl.Out
      type ProcessImplementations = pimpl.Out
    }
  }
}

sealed trait BoundedContextType {
  type Interface <: BoundedContext
  val interface: Interface
  type Implementation <: BoundedContextImplementation
  val implementation: Implementation
}

/** Model of a subdomain of the problem (i.e. inventory or customer relationship mangement). */
sealed trait BoundedContext {
  val name: String

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

sealed trait BoundedContextImplementation {
  val name: String

  type Aggregates <: HList
  val aggregates: Aggregates
  def aggregatesUnified: List[AggregateImplementation]

  type Processes <: HList
  val processes: Processes
}
object BoundedContextImplementation {
  type Aux[A <: HList, P <: HList] = BoundedContextImplementation {
    type Aggregates = A
    type Processes = P
  }
}

sealed trait BoundedContextAction[BC <: BoundedContext, +A]
object BoundedContextAction {
  case class Command[BC <: BoundedContext, A <: Aggregate: BC#IsAggregate, C <: Cmd: Inject[A#Command, ?]](
    to: A#Id,
    command: C
  ) extends BoundedContextAction[BC, CommandResult[C]]
  type CommandResult[C <: Cmd] = C#Errors Xor Unit
}