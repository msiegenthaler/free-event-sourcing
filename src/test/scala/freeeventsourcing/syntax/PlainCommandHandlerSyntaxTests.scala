package freeeventsourcing.syntax

import scala.collection.immutable.Seq
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ Coproduct, Poly1 }

class PlainCommandHandlerSyntaxTests extends FlatSpec with Matchers with CommandHandlerSyntaxTests {
  trait BaseH extends Poly1 with PlainCommandHandlerSyntax[Command, Event, String]
  import Command._
  import Event._

  "PlainCommandHandlerSyntax " should " allow for no-op" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.ignore
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(Seq.empty)
  }

  "PlainCommandHandlerSyntax " should " allow for single event" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.success(Event1())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for multiple events" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.success(Event1(), Event2())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1(), Event2()))
  }

  "PlainCommandHandlerSyntax " should " allow for error" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] {
        _.fail(ErrorOne("X"))
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Left(r)
  }

  "PlainCommandHandlerSyntax " should " allow for error or single event depending on the state" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.state == "nok") c.fail(ErrorOne("X"))
        else c.success(Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("nok")(H.first) shouldBe Left(r)
    call(H, FirstCommand("hi"))("ok")(H.first) shouldBe Right(List(Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for error or multiple events depending on the state" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.state == "nok") c.fail(ErrorOne("X"))
        else c.success(Event2(), Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("nok")(H.first) shouldBe Left(r)
    call(H, FirstCommand("hi"))("ok")(H.first) shouldBe Right(List(Event2(), Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for error or single event depending on the command" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.command.arg == "hi") c.fail(ErrorOne("X"))
        else c.success(Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Left(r)
    call(H, FirstCommand("ho"))("state")(H.first) shouldBe Right(List(Event1()))
  }

  "PlainCommandHandlerSyntax " should " allow for direct access to the command" in {
    object H extends BaseH {
      implicit val first = on[FirstCommand] { c ⇒
        if (c.arg == "hi") c.fail(ErrorOne("X"))
        else c.success(Event1())
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Left(r)
    call(H, FirstCommand("ho"))("state")(H.first) shouldBe Right(List(Event1()))
  }
}
