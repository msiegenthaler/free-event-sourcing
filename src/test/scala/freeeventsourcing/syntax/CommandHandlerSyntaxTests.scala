package freeeventsourcing.syntax

import scala.collection.immutable.Seq
import cats.data.Xor
import freeeventsourcing.AggregateCommand
import shapeless.poly.Case1
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

  sealed trait Event
  object Event {
    case class Event1() extends Event
    case class Event2() extends Event
    case class Event3(v: String) extends Event
  }

  trait BaseS extends Poly1 {
    type State = String
    type Event = CommandHandlerSyntaxTests.this.Event
    type Command = CommandHandlerSyntaxTests.this.Command
  }

  def call[T <: BaseS, C <: Command, O](t: T, value: C)(state: t.State)(
    implicit
    c: Case1.Aux[T, C, t.State ⇒ value.Error Xor Seq[t.Event]]
  ): value.Error Xor Seq[t.Event] = {
    c.apply(value).apply(state)
  }
}
