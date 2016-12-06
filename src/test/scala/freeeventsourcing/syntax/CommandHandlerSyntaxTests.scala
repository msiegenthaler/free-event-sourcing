package freeeventsourcing.syntax

import scala.collection.immutable.Seq
import freeeventsourcing.api.domainmodel.{ AggregateCommand, DomainEvent }
import shapeless.{ :+:, CNil, Poly1 }

trait CommandHandlerSyntaxTests {
  case class ErrorOne(a: String)

  sealed trait Command extends AggregateCommand
  object Command {
    case class FirstCommand(arg: String) extends Command {
      type Error = ErrorOne :+: CNil
    }
    case class SecondCommand(arg: Int) extends Command {
      type Error = CNil
    }
  }

  sealed trait Event extends DomainEvent
  object Event {
    case class Event1() extends Event
    case class Event2() extends Event
    case class Event3(v: String) extends Event
  }

  def call[T, C <: Command, O](t: T, value: C)(state: String)(
    implicit
    c: C ⇒ String ⇒ Either[value.Error, Seq[Event]]
  ): Either[value.Error, Seq[Event]] = {
    c.apply(value).apply(state)
  }
}
