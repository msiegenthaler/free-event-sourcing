package freeeventsourcing

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
          //TODO Other expectations
          //        case WaitUntil(instant) ⇒ ???
          //        case FirstOf(alternatives) ⇒ ???
          //        case Execute(aggregateType, aggregate, command, errorHandler) ⇒ ???
          //        case End() ⇒ ???
          case _ ⇒ ???
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
