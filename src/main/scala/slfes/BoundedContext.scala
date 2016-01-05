package slfes

import scala.language.higherKinds
import cats.Monad
import cats.free.Free
import shapeless.LUBConstraint.<<:
import shapeless.{HList, ::}
import shapeless.ops.coproduct.Inject
import shapeless.ops.hlist.Selector
import slfes.ProcessBodyAction.CommandResult

/** Model of a subdomain of the problem (i.e. inventory or customer relationship mangement). */
sealed trait BoundedContext {
  type Aggregates <: HList
  type Processes <: HList

  type IsAggregate[A <: AggregateInterface] = Selector[Aggregates, A]

  type Monad[A] = BoundedContext.BoundedContextM[this.type, A]
  type Action[+A] = BoundedContextAction[this.type, A]
  type Exec[F[_]] = cats.free.Inject[Action, F]
  type BoundedContext = this.type

  def noop[F[_] : Exec]: Free[F, Unit] = Monad[Free[F, ?]].pure(())

  def exec[F[_] : Exec, I, C <: Cmd](to: I, command: C)
    (implicit cfi: CommandForIdInList[Aggregates, I, C]): Free[F, CommandResult[C]] = {
    import cfi._
    val action = BoundedContextAction.Command[BoundedContext, cfi.Aggregate, C](to, command)
    lift[F, CommandResult[C]](action)
  }

  private def lift[F[_], A](op: Action[A])(implicit inject: cats.free.Inject[Action, F]) = Free.liftF(inject.inj(op))
}
object BoundedContext {
  type BoundedContextM[BC <: BoundedContext, A] = Free[BoundedContextAction[BC, ?], A]
}

case class BoundedContextType[AS <: HList : <<:[AggregateType]#λ , PS <: HList : <<:[ProcessType]#λ](name: String, aggregates: AS, processes: PS) {

  def boundedContext = new BoundedContext {
    override type Aggregates = AS
    override type Processes = PS
  }
}

sealed trait BoundedContextAction[BC <: BoundedContext, +A]
object BoundedContextAction {
  case class Command[BC <: BoundedContext, A <: AggregateInterface : BC#IsAggregate, C <: Cmd : Inject[A#Command, ?]](to: A#Id,
    command: C)
    extends BoundedContextAction[BC, CommandResult[C]]

}
