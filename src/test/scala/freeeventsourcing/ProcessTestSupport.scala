package freeeventsourcing

import java.time.Instant
import cats.data.{ State, StateT, Xor, XorT }
import cats.{ Monad, ~> }
import freeeventsourcing.EventSelector.WithEventType
import freeeventsourcing.Process.ProcessMonad
import freeeventsourcing.ProcessAction._
import org.scalatest.matchers.{ MatchResult, Matcher }

/** Supports writing expectations agains process definitions. Usage:
 *  <code>
 *  val support = new ProcessTestSupport(AccountProcessing)
 *  import support._
 *
 *  waitUntil(i) should runFromWithResult(Expect.waitUntil(i))(())
 *  </code>
 */
class ProcessTestSupport[BC <: BoundedContext](boundedContext: BC) {
  /** Matcher to check if the process monad results in the expected actions. */
  def runFrom(expectations: Expectation*) =
    ProcessMatcher(expectations.toList, _ ⇒ Xor.right(()))

  /** Matcher to check if the process monad results in the expected actions and return the specified result. */
  def runFromWithResult(expectations: Expectation*)(toResult: Any) =
    ProcessMatcher(expectations.toList, r ⇒
      if (r == toResult) Xor.right(()) else Xor.left(s"produced non matching result: ${r} instead of ${toResult}"))

  /** Helper methods to construct the expectations. */
  object Expect {
    import Expectation._
    def awaitEvent[S <: WithEventType](selector: S)(result: S#Event) =
      ExpectAwaitEvent(selector, result)
    def waitUntil(instant: Instant) = ExpectWaitUntil(instant)
    def command[A <: Aggregate, C <: A#Command](aggregateType: A, aggregate: A#Id, command: C)(result: command.Error Xor Unit) =
      ExpectCommand(aggregateType, aggregate, command, result)
    def end = ExpectEnd
  }

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

  type Action[+A] = ProcessAction[BC, A]
  type M[A] = ProcessMonad[BC, A]

  sealed trait Expectation
  object Expectation {
    case class ExpectAwaitEvent[S <: WithEventType](selector: S, result: S#Event) extends Expectation
    case class ExpectWaitUntil(instant: Instant) extends Expectation
    case class ExpectCommand[A <: Aggregate, Cmd <: A#Command](
      aggregateType: A, aggregate: A#Id, command: A#Command, result: Cmd#Error Xor Unit
    ) extends Expectation
    case object ExpectEnd extends Expectation
  }

  private[this] object Implementation {
    import Expectation._

    type OrFail[A] = Xor[String, A]
    type Expectations[A] = StateT[OrFail, List[Expectation], A]
    object Expectations {
      def next(received: String): Expectations[Expectation] = lift {
        case h :: t ⇒ Xor.right((t, h))
        case Nil    ⇒ Xor.left(s"No more expectations, but got ${received}")
      }
      def check[A](action: ProcessAction[BC, A]): Expectations[A] = next(action.toString).flatMap { exp ⇒
        def lifted(x: String Xor A): Expectations[A] = lift(s ⇒ x.map(r ⇒ (s, r)))
        (action, exp) match {
          case (AwaitEvent(s1), ExpectAwaitEvent(s2, result)) ⇒ lifted {
            if (s1 == s2) Xor.right(result)
            else Xor.left(s"AwaitEvent: selector mismatch ${s1} != ${s2}")
          }

          case (WaitUntil(i1), ExpectWaitUntil(i2)) ⇒ lifted {
            if (i1 == i2) Xor.right(())
            else Xor.left(s"WaitUntil: time is different: ${i1} != ${i2}")
          }

          case (End(), ExpectEnd) ⇒ lifted {
            Xor.right(())
          }

          case (Execute(at1, a1, c1, errorHandler), e @ ExpectCommand(at2, a2, c2, r)) ⇒
            if (at1 != at2) lifted(Xor.left(s"Execute: Aggregate type is different: ${at1} != ${at2}"))
            else if (a1 != a2) lifted(Xor.left(s"Execute: Aggregate id is different: ${a1} != ${a2}"))
            else if (c1 != c2) lifted(Xor.left(s"Execute: Command is different: ${c1} != ${c2}"))
            else {
              r.fold({ error ⇒
                val subprocess: M[Unit] = errorHandler(error.asInstanceOf) //could be proved, but for tests a ClassCase Exception is ok
                val x = subprocess.foldMap(Implementation.Transform)
                lift(s ⇒ x.run(s))
              }, u ⇒ lifted(Xor.right(u)))
            }

          // TODO case FirstOf(alternatives) ⇒ ???

          case (a, b) ⇒
            lifted(Xor.left(s"Expected ${a} but got ${b}"))
        }
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
