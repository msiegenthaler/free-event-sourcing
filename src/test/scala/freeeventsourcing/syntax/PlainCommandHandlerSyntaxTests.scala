package freeeventsourcing.syntax

import scala.collection.immutable.Seq
import cats.data.Xor
import freeeventsourcing.AggregateCommand
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ :+:, CNil, Coproduct, Generic, Poly1 }
import shapeless.poly.Case1

class PlainCommandHandlerSyntaxTests extends FlatSpec with Matchers {
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
  }

  import Command._
  import Event._

  trait BaseH extends Poly1 with PlainCommandHandlerSyntax {
    type State = String
    type Event = PlainCommandHandlerSyntaxTests.this.Event
    type Command = PlainCommandHandlerSyntaxTests.this.Command
  }

  def call[T <: BaseH, C <: Command, O](t: T, value: C)(state: t.State)(
    implicit
    c: Case1.Aux[T, C, t.State ⇒ value.Error Xor Seq[t.Event]]
  ): value.Error Xor Seq[t.Event] = {
    c.apply(value).apply(state)
  }

  "PlainCommandHandlerSyntax " should " allow for no-op" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.ignore
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Xor.right(Seq.empty)
  }

  "PlainCommandHandlerSyntax " should " allow for single event" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.success(Event1())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Xor.right(List(Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for multiple events" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.success(Event1(), Event2())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Xor.right(List(Event1(), Event2()))
  }

  "PlainCommandHandlerSyntax " should " allow for error" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.fail(ErrorOne("X"))
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Xor.left(r)
  }

  "PlainCommandHandlerSyntax " should " allow for error or single event depending on the state" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.state == "nok") c.fail(ErrorOne("X"))
        else c.success(Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("nok")(H.first) shouldBe Xor.left(r)
    call(H, FirstCommand("hi"))("ok")(H.first) shouldBe Xor.right(List(Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for error or multiple events depending on the state" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.state == "nok") c.fail(ErrorOne("X"))
        else c.success(Event2(), Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("nok")(H.first) shouldBe Xor.left(r)
    call(H, FirstCommand("hi"))("ok")(H.first) shouldBe Xor.right(List(Event2(), Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for error or single event depending on the command" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.command.arg == "hi") c.fail(ErrorOne("X"))
        else c.success(Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Xor.left(r)
    call(H, FirstCommand("ho"))("state")(H.first) shouldBe Xor.right(List(Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for direct access to the command" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.arg == "hi") c.fail(ErrorOne("X"))
        else c.success(Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Xor.left(r)
    call(H, FirstCommand("ho"))("state")(H.first) shouldBe Xor.right(List(Event1()))
  }
}
