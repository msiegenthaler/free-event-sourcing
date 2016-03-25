package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import akka.pattern.ask
import akka.util.Timeout
import cats.data.Xor
import cats.~>
import shapeless.ops.coproduct.Inject
import slfes.{ Cmd, _ }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

object AkkaBoundedContext {
  case class ExecuteCommand(id: CommandId, aggregateType: String, aggregateId: Any, cmd: Cmd)

  def props(bc: BoundedContextImplementation) =
    Props(new Impl(bc))

  private class Impl(bc: BoundedContextImplementation) extends Actor {
    val aggregateTypes = {
      bc.aggregatesUnified.map { aggregate ⇒
        val props = AkkaAggregateType.props(bc.name, aggregate)
        val actor = context actorOf props
        (aggregate.name → actor)
      }.toMap
    }
    override def receive = {
      case ExecuteCommand(id, aggregateType, aggregateId, cmd) ⇒
        aggregateTypes.get(aggregateType) match {
          case Some(aggregateActor) ⇒
            aggregateActor forward AkkaAggregateType.ExecuteCommand(id, aggregateId, cmd)

          case None ⇒
            sender() ! CommandFailed(id, s"Unknown aggregate type ${aggregateType} in bounded context ${bc.name}")
        }
    }
  }
}

class Xxx[BC <: BoundedContext](actor: ActorRef, bc: BC)(implicit timeout: Timeout, ec: ExecutionContext)
    extends (BoundedContextAction[BC, ?] ~> Future) {
  def apply[A](fa: BoundedContextAction[BC, A]) = fa match {
    case BoundedContextAction.Command(to, cmd) ⇒
      val id = CommandId.generate
      val msg = AkkaBoundedContext.ExecuteCommand(id, bc.name, to, cmd)
      actor.ask(msg).flatMap(_ match {
        case CommandExecuted(`id`) ⇒
          Future.successful(Xor.right(()))
        case CommandFailed(`id`, error) ⇒
          //not very elegant, but it can only happen if there is a severe implementation error in this package which would
          // cause every error to fail. So make this tradeoff instead of going for the more comprehensive approach with
          // a Typeable for Error on the Cmd.
          Try(error.asInstanceOf[cmd.Errors])
            .map(e ⇒ Future.successful(Xor.left(e)))
            .getOrElse(Future.failed(new IllegalArgumentException(s"Command invalid: Error type ${error.getClass.getName} is invalid")))
        case CommandInvalid(`id`, reason) ⇒
          Future.failed(new IllegalArgumentException(s"Command invalid: ${reason}"))
      })
  }
}

class BoundedContextIf[BC <: BoundedContextImplementation, IF <: BoundedContext](boundedContext: BC, interface: IF, actor: ActorRef) {
  def executeCommand[A <: Aggregate, C <: Cmd: Inject[A#Command, ?]](to: A#Id, command: C)(sel: interface.IsAggregate[A]) = {
    val aggregate = sel(interface.aggregates)
    actor ! AkkaBoundedContext.ExecuteCommand(CommandId.generate, aggregate.name, to, command)
  }
}