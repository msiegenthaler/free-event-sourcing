package slfesakka

import akka.actor.{ Actor, ActorRef }
import slfes.BoundedContextImplementation

class AkkaBoundedContext[BC <: BoundedContextImplementation](bc: BC) {
  private class Impl(eventBus: ActorRef) extends Actor {
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
