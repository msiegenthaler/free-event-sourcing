package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import slfes.{ AggregateImplementation, Cmd }

/** Manages a class of Aggregates */
object AkkaAggregateType {
  case class ExecuteCommand(id: CommandId, aggregateId: Any, cmd: Cmd)

  def props(boundedContext: String, aggregate: AggregateImplementation) =
    Props(new Impl(boundedContext, aggregate))

  class Impl(boundedContext: String, aggregate: AggregateImplementation) extends Actor {
    def receive = handle(Map.empty)
    def handle(instances: Map[aggregate.Id, ActorRef]): Receive = {
      case ExecuteCommand(commandId, aggregateId, cmd) ⇒
        aggregate.IdTypeable.cast(aggregateId) match {
          case Some(id) ⇒
            val actor = instances.getOrElse(id, {
              val a = create(id)
              context become handle(instances + (id → a))
              a
            })
            actor forward AkkaAggregate.ExecuteCommand(commandId, cmd)

          case None ⇒
            sender() ! CommandInvalid(commandId, s"Invalid id for aggregate ${aggregate.name}: type was ${aggregateId.getClass.getName}")
        }
    }

    private def create(id: aggregate.Id) = {
      context.actorOf(AkkaAggregate.props(boundedContext, aggregate)(id))
    }
  }
}