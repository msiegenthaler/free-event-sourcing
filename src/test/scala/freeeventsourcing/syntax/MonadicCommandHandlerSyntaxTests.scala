package freeeventsourcing.syntax

import scala.collection.immutable.Seq
import org.scalatest.{ FlatSpec, Matchers }
import shapeless.{ Coproduct, Poly1 }

class MonadicCommandHandlerSyntaxTests extends FlatSpec with Matchers with CommandHandlerSyntaxTests {
  trait BaseH extends Poly1 with MonadicCommandHandlerSyntax[Command, Event, String]
  import Command._
  import Event._

  "MonadicCommandHandlerSyntax " should " allow for no-op" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] {
        _.ignore
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(Seq.empty)
  }

  "MonadicCommandHandlerSyntax " should " allow to emit a single event" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] {
        _.emit(Event1())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1()))
  }

  "MonadicCommandHandlerSyntax " should " allow to conditionally emit a single event" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] {
        _.emitIf(true)(Event1())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1()))
  }

  "MonadicCommandHandlerSyntax " should " allow to conditionally not emit a single event" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] {
        _.emitIf(false)(Event1())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List())
  }

  "MonadicCommandHandlerSyntax " should " allow to emit multiple events in one call" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] {
        _.emit(Event1(), Event2())
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1(), Event2()))
  }

  "MonadicCommandHandlerSyntax " should " collect events" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        _ ← c.emit(Event1())
        _ ← c.emit(Event2())
      } yield ())
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1(), Event2()))
  }

  "MonadicCommandHandlerSyntax " should " allow for error" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] {
        _.fail(ErrorOne("X"))
      }
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("X"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Left(r)
  }

  "MonadicCommandHandlerSyntax " should " allow to access the state directly" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] { c ⇒
        c.emit(Event3(c.state))
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event3("state")))
  }

  "MonadicCommandHandlerSyntax " should " allow to access the state in a monad" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        s ← c.stateM
        _ ← c.emit(Event3(s))
      } yield ())
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event3("state")))
  }

  "MonadicCommandHandlerSyntax " should " allow to access the command directly" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] { c ⇒
        c.emit(Event3(c.command.arg))
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event3("hi")))
  }

  "MonadicCommandHandlerSyntax " should " allow to access the command via implicit conversion" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand] { c ⇒
        c.emit(Event3(c.arg))
      }
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event3("hi")))
  }

  "MonadicCommandHandlerSyntax " should " allow to access the command in a monad" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        cmd ← c.commandM
        _ ← c.emit(Event3(cmd.arg))
      } yield ())
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event3("hi")))
  }

  "MonadicCommandHandlerSyntax.failIf " should " terminate if condition matches" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        _ ← c.failIf(c.state == "state")(ErrorOne("x"))
        _ ← c.emit(Event1())
      } yield ())
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("x"))
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Left(r)
  }

  "MonadicCommandHandlerSyntax.failIf " should " continue if condition does not match" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        _ ← c.failIf(c.state == "state")(ErrorOne("x"))
        _ ← c.emit(Event1())
      } yield ())
    }
    call(H, FirstCommand("hi"))("not-state")(H.first) shouldBe Right(List(Event1()))
  }

  "MonadicCommandHandlerSyntax.assertThat " should " terminate if assertion does not match" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        _ ← c.assertThat(c.state == "state")(ErrorOne("x"))
        _ ← c.emit(Event1())
      } yield ())
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("x"))
    call(H, FirstCommand("hi"))("not-state")(H.first) shouldBe Left(r)
  }

  "MonadicCommandHandlerSyntax.assertThat " should " continue if assertion matches" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        _ ← c.assertThat(c.state == "state")(ErrorOne("x"))
        _ ← c.emit(Event1())
      } yield ())
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1()))
  }

  "MonadicCommandHandlerSyntax " should " collect events before and after non matching" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        _ ← c.emit(Event1())
        _ ← c.assertThat(c.state == "state")(ErrorOne("x"))
        _ ← c.emit(Event2())
      } yield ())
    }
    call(H, FirstCommand("hi"))("state")(H.first) shouldBe Right(List(Event1(), Event2()))
  }

  "MonadicCommandHandlerSyntax " should " not emit events on error" in {
    object H extends BaseH {
      implicit val first = onM[FirstCommand](c ⇒ for {
        _ ← c.emit(Event1())
        _ ← c.assertThat(c.state == "state")(ErrorOne("x"))
        _ ← c.emit(Event2())
      } yield ())
    }
    val r = Coproduct[FirstCommand#Error](ErrorOne("x"))
    call(H, FirstCommand("hi"))("not-state")(H.first) shouldBe Left(r)
  }
}
