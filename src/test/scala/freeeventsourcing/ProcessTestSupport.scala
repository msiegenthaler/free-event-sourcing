package freeeventsourcing

import java.time.Instant
import cats.data.{ State, StateT, Xor, XorT }
import cats.free.Free
import cats.{ Monad, ~> }
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.Process.ProcessMonad
import freeeventsourcing.ProcessAction._
import org.scalatest.matchers.{ MatchResult, Matcher }

class ProcessTestSupport[BC <: BoundedContext](boundedContext: BC) {
  def runFrom(expectations: Expectation*) =
    ProcessMatcher(expectations.toList, _ ⇒ Xor.right(()))
  def runFromWithResult(expectations: Expectation*)(toResult: Any) =
    ProcessMatcher(expectations.toList, r ⇒
      if (r == toResult) Xor.right(()) else Xor.left(s"produced non matching result: ${r} instead of ${toResult}"))

  case class ProcessMatcher(expectations: List[Expectation], checkResult: Any ⇒ String Xor Unit) extends Matcher[M[_]] {
    def apply(process: M[_]) = {
      val x = process.foldMap(Implementation.Transform)
      val y = x
        .run(expectations)
        .leftMap(error ⇒ s"did not match the expectations: ${error}")
        .flatMap(r ⇒ if (r._1.isEmpty) Xor.right(r._2) else Xor.left(s"rmaining expectations: ${r._1}"))
        .flatMap(checkResult)
      new MatchResult(
        y.isRight,
        y.fold(identity, _ ⇒ "?"),
        s"matched the process expectations but should not have"
      )
    }
  }

  sealed trait Expectation
  case class ExpectAwaitEvent[S <: WithEventType](selector: S, result: S#Event) extends Expectation
  object ExpectAwaitEvent {
    def create[S <: WithEventType](selector: S)(result: selector.Event): ExpectAwaitEvent[S] =
      ExpectAwaitEvent(selector, result)
  }
  case class ExpectWaitUntil(instant: Instant) extends Expectation
  case object ExpectEnd extends Expectation
  case class ExpectCommand[A <: Aggregate, Cmd <: A#Command](
    aggregateType: A, aggregate: A#Id, command: A#Command, result: Cmd#Error Xor Unit
  ) extends Expectation
  object ExpectCommand {
    def create[A <: Aggregate, C <: A#Command](aggregateType: A, aggregate: A#Id, command: C)(result: command.Error Xor Unit): ExpectCommand[A, command.type] =
      ExpectCommand(aggregateType, aggregate, command, result)
  }

  type Action[+A] = ProcessAction[BC, A]
  type M[A] = ProcessMonad[BC, A]

  private[this] object Implementation {
    type OrFail[A] = Xor[String, A]
    type Expectations[A] = StateT[OrFail, List[Expectation], A]
    object Expectations {
      def next(received: String): Expectations[Expectation] = lift {
        case h :: t ⇒ Xor.right((t, h))
        case Nil    ⇒ Xor.left(s"No more expectations, but got ${received}")
      }
      def check[A](action: ProcessAction[BC, A]): Expectations[A] = next(action.toString).flatMap { exp ⇒
        val r: String Xor A = (action, exp) match {
          case (AwaitEvent(s1), ExpectAwaitEvent(s2, result)) ⇒
            if (s1 == s2) Xor.right(result)
            else Xor.left(s"AwaitEvent: selector mismatch ${s1} != ${s2}")
          case (WaitUntil(i1), ExpectWaitUntil(i2)) ⇒
            if (i1 == i2) Xor.right(())
            else Xor.left(s"WaitUntil: time is different: ${i1} != ${i2}")
          case (End(), ExpectEnd) ⇒
            Xor.right(())
          case (Execute(at1, a1, c1, errorHandler), ExpectCommand(at2, a2, c2, r)) ⇒
            if (at1 != at2) Xor.left(s"Execute: Aggregate type is different: ${at1} != ${at2}")
            else if (a1 != a2) Xor.left(s"Execute: Aggregate id is different: ${a1} != ${a2}")
            else if (c1 != c2) Xor.left(s"Execute: Command is different: ${c1} != ${c2}")
            else {
              Xor.right {
                r.fold(
                  //TODO actually we should evaluate recursivly...
                  //                e ⇒ errorHandler(e.asInstanceOf), //not really save, but don't care in tests..
                  e ⇒ (), //not really save, but don't care in tests..
                  _ ⇒ ()
                )
              }
            }

          //TODO Other expectations
          //        case FirstOf(alternatives) ⇒ ???
          case _ ⇒ ???
          //TODO catch non matching and produce a nice error
        }
        lift(s ⇒ r.map(x ⇒ (s, x)))
      }

      private[this] def lift[A](f: List[Expectation] ⇒ OrFail[(List[Expectation], A)]) =
        StateT[OrFail, List[Expectation], A](f)
    }

    object Transform extends (Action ~> Expectations) {
      def apply[A](fa: Action[A]) = {
        Expectations.check(fa)
      }
    }
  }
}
