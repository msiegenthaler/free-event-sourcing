package slfesakka

import akka.actor.{ Actor, ActorRef, Props }
import slfes.BoundedContextImplementation

object AkkaBoundedContext {
  def props(bc: BoundedContextImplementation, eventBus: ActorRef) =
    Props(new Impl(bc, eventBus))

  private class Impl(bc: BoundedContextImplementation, eventBus: ActorRef) extends Actor {
    override def preStart = {
      val props = bc.aggregatesUnified.map { aggregate ⇒
        AkkaAggregateType.props(aggregate, eventBus)
      }
      props.foreach(p ⇒ context.actorOf(p))
    }
    override def receive = {
      case _ ⇒ ()
    }
  }
}
