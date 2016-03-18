package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import shapeless.ops.coproduct.Inject
import slfes.{ Cmd, _ }

object AkkaBoundedContext {
  case class ExecuteCommand(id: CommandId, aggregateType: String, aggregateId: Any, cmd: Cmd)

  def props(bc: BoundedContextImplementation, eventBus: ActorRef) =
    Props(new Impl(bc, eventBus))

  private class Impl(bc: BoundedContextImplementation, eventBus: ActorRef) extends Actor {
    val aggregateTypes = {
      bc.aggregatesUnified.map { aggregate ⇒
        val props = AkkaAggregateType.props(aggregate, eventBus)
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

class BoundedContextIf[BC <: BoundedContextImplementation, IF <: BoundedContext](boundedContext: BC, interface: IF, actor: ActorRef) {
  def executeCommand[A <: Aggregate, C <: Cmd: Inject[A#Command, ?]](to: A#Id, command: C)(sel: interface.IsAggregate[A]) = {
    val aggregate = sel(interface.aggregates)
    actor ! AkkaBoundedContext.ExecuteCommand(CommandId.generate, aggregate.name, to, command)
  }
}