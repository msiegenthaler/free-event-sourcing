package slfes2.akka

import akka.actor.{ Actor, ActorRef, Props }
import simulacrum.typeclass
import slfes.utils.StringSerializable
import slfes2.accountprocessing.{ Account, AccountProcessing, Transaction }
import slfes2._
import slfes2.accountprocessing.impl.{ AccountApplicator, AccountHandler, AccountState }

import scala.language.implicitConversions
import scala.annotation.implicitNotFound

object AkkaBoundedContext {
  case class ExecuteAggregateCommand(commandId: CommandId, aggregate: String, aggregateId: Any, command: AggregateCommand)

  //private
  class ActorImpl[BC <: BoundedContext](boundedContext: BC)(implicit impl: SupportedBoundedContext[BC]) extends Actor {
    val aggregateTypes = impl.aggregates.map { case (k, v) ⇒ (k.name → createFor(v)) }

    def createFor[A <: Aggregate](implicit a: SupportedAggregate[A]): ActorRef = {
      import a._
      def persistenceIdFor(id: A#Id): String = PersistenceIds.forAggregate(boundedContext, a.aggregate)(id)
      val props = AkkaAggregateType.props(a.aggregate, persistenceIdFor)
      context actorOf props
    }

    def receive = {
      case ExecuteAggregateCommand(commandId, aggregate, aggregateId, command) ⇒
        aggregateTypes.get(aggregate) match {
          case Some(aggregateActor) ⇒
            aggregateActor forward AkkaAggregateType.Execute(commandId, aggregateId, command)

          case None ⇒
            sender() ! CommandFailed(commandId, s"Unknown aggregate type ${aggregate} in bounded context ${boundedContext.name}")
        }
    }
  }
}