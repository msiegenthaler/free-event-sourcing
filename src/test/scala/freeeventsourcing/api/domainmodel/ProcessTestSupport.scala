package freeeventsourcing.api.domainmodel

import java.time.Instant
import cats.data.StateT
import cats.~>
import cats.syntax.either._
import cats.instances.either._
import freeeventsourcing.api.domainmodel.EventSelector.WithEventType
import freeeventsourcing.api.domainmodel.ProcessAction.FirstOf.Alternatives
import freeeventsourcing.api.domainmodel.ProcessAction._
import freeeventsourcing.api.domainmodel.ProcessDefinition.ProcessMonad
import freeeventsourcing.utils.ADT
import org.scalatest.matchers.{ MatchResult, Matcher }
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

/** Supports writing expectations against process definitions. Usage:
 *  <code>
 *  val support = new ProcessTestSupport(AccountProcessing)
 *  import support._
 *
 *  waitUntil(i) should runFromWithResult(Expect.waitUntil(i))(())
 *  </code>
 */
class ProcessTestSupport[DM <: DomainModel](domainModel: DM) {
  /** Matcher to check if the process monad results in the expected actions. */
  def runFrom(expectations: Expectation*) =
    ProcessMatcher(expectations.toList, _ ⇒ Right(()))

  /** Matcher to check if the process monad results in the expected actions and return the specified result. */
  def runFromWithResult(expectations: Expectation*)(toResult: Any) =
    ProcessMatcher(expectations.toList, r ⇒
      if (r == toResult) Right(()) else Left(s"produced non matching result: ${r} instead of ${toResult}"))

  /** Helper methods to construct the expectations. */
  object Expect {
    import Expectation._
    def awaitEvent[S <: WithEventType](selector: S)(result: S#Event) =
      ExpectAwaitEvent(selector, EventWithMetadata(result, EventMetadata(MockEventId(), MockEventTime())))
    def waitUntil(instant: Instant) = ExpectWaitUntil(instant)
    def commandSuccessful[A <: Aggregate, C <: A#Command](aggregateType: A, aggregate: A#Id, command: C) =
      ExpectCommand(aggregateType, aggregate, command, Right(()))
    def firstOf(selectors: FirstOfOption*)(resultIndex: Int, result: Any) = result match {
      case () ⇒ ExpectFirstOf(selectors.toList, resultIndex, ())
      case _  ⇒ ExpectFirstOf(selectors.toList, resultIndex, EventWithMetadata(result, EventMetadata(MockEventId(), MockEventTime())))
    }
    def commandFailed[A <: Aggregate, C <: A#Command, E](aggregateType: A, aggregate: A#Id, command: C)(result: E)(
      implicit
      i: Inject[C#Error, E]
    ) =
      ExpectCommand(aggregateType, aggregate, command, Left(i(result)))
    def end = ExpectEnd
  }

  case class ProcessMatcher(expectations: List[Expectation], checkResult: Any ⇒ Either[String, Unit]) extends Matcher[M[_]] {
    def apply(process: M[_]) = {
      val x = process.foldMap(Implementation.Transform)
      val y = x
        .run(expectations)
        .leftMap(error ⇒ s"did not match the expectations: ${error}")
        .flatMap {
          case (Nil, r)       ⇒ Right(r)
          case (remaining, _) ⇒ Left(s"remaining expectations: ${remaining.mkString(", ")}")
        }
        .flatMap(checkResult)
      new MatchResult(
        y.isRight,
        y.fold(identity, _ ⇒ "?"),
        s"matched the process expectations but should not have"
      )
    }
  }

  type Action[+A] = ProcessAction[DM, A]
  type M[A] = ProcessMonad[DM, A]

  sealed trait Expectation extends ADT
  object Expectation {
    case class ExpectAwaitEvent[S <: WithEventType](selector: S, result: EventWithMetadata[S#Event]) extends Expectation
    case class ExpectWaitUntil(instant: Instant) extends Expectation
    case class ExpectFirstOf(of: List[FirstOfOption], resultIndex: Int, result: Any) extends Expectation
    case class ExpectCommand[A <: Aggregate, Cmd <: A#Command](
      aggregateType: A, aggregate: A#Id, command: A#Command, result: Either[Cmd#Error, Unit]
    ) extends Expectation
    case object ExpectEnd extends Expectation
  }

  sealed trait FirstOfOption
  case class FirstOfSelector[S <: WithEventType](selector: S)(implicit val es: EventSelector[S]) extends FirstOfOption
  case class FirstOfUntil(instant: Instant) extends FirstOfOption

  private[this] object Implementation {
    import Expectation._

    type OrFail[A] = Either[String, A]
    type Expectations[A] = StateT[OrFail, List[Expectation], A]
    object Expectations {
      def next(received: String): Expectations[Expectation] = lift {
        case h :: t ⇒ Right((t, h))
        case Nil    ⇒ Left(s"No more expectations, but got ${received}")
      }

      def check[A](action: Action[A]): Expectations[A] = next(action.toString).flatMap { exp ⇒
        compare.lift((action, exp)).getOrElse {
          lifted(Left(s"Expected ${exp} but got ${action}"))
        }
      }

      private[this]type Comparer[A] = PartialFunction[(Action[A], Expectation), Expectations[A]]

      private[this] def awaitEvent[A]: Comparer[A] = {
        case (AwaitEvent(s1), ExpectAwaitEvent(s2, result)) if s1 == s2 ⇒ {
          val metadata = EventMetadata(MockEventId(), MockEventTime())
          ok(result)
        }
        case (AwaitEvent(s1), ExpectAwaitEvent(s2, _)) ⇒
          fail(s"AwaitEvent: selector mismatch ${s1} != ${s2}")
      }

      private[this] def waitUnit[A]: Comparer[A] = {
        case (WaitUntil(i1), ExpectWaitUntil(i2)) if i1 == i2 ⇒
          ok(())
        case (WaitUntil(i1), ExpectWaitUntil(i2)) ⇒
          fail(s"WaitUntil: time is different: ${i1} != ${i2}")
      }

      private[this] def firstOf[A]: Comparer[A] = {
        case (FirstOf(as), ExpectFirstOf(s2, index, result)) ⇒
          val s1 = fromAlts(as)
          if (s1.zip(s2).forall { case (a, b) ⇒ a == b }) {
            val r = Coproduct.unsafeMkCoproduct(s2.length - index - 1, result) //not pretty, but does the job in a test
            ok(r.asInstanceOf[A]) //Just cast it, it'll result in an error later (good enough for test)
          } else {
            fail(s"Different expectations in FirstOf: ${s1.mkString(", ")} vs ${s2.mkString(", ")}")
          }
      }
      private[this] def execute[A]: Comparer[A] = {
        case (Execute(at1, _, _, _), ExpectCommand(at2, _, _, _)) if at1 != at2 ⇒
          fail(s"Execute: Aggregate type is different: ${at1} != ${at2}")
        case (Execute(_, a1, _, _), ExpectCommand(_, a2, _, _)) if a1 != a2 ⇒
          fail(s"Execute: Aggregate id is different: ${a1} != ${a2}")
        case (Execute(_, _, c1, _), ExpectCommand(_, _, c2, _)) if c1 != c2 ⇒
          fail(s"Execute: Command is different: ${c1} != ${c2}")
        case (ex @ Execute(_, _, _, errorHandler), e @ ExpectCommand(_, _, _, r)) ⇒
          r.fold({ error ⇒
            //could be proved, but for tests a ClassCastException is ok
            val err: ex.Error = error.asInstanceOf[ex.Error]
            val subprocess: M[Unit] = errorHandler(err).asInstanceOf[M[Unit]]
            val x = subprocess.foldMap(Implementation.Transform)
            lift(s ⇒ x.run(s))
          }, ok)
      }

      private[this] def end[A]: Comparer[A] = {
        case (End(), ExpectEnd) ⇒
          ok(())
      }

      private[this] def compare[A]: Comparer[A] =
        awaitEvent[A] orElse waitUnit[A] orElse firstOf[A] orElse execute[A] orElse end[A]

      private[this] def fromAlts(alt: Alternatives[DM]): List[FirstOfOption] = alt match {
        case FirstOf.Empty() ⇒
          Nil
        case FirstOf.Alternative(a @ AwaitEvent(selector), tail) ⇒
          fromAlts(tail) :+ FirstOfSelector(selector)(a.eventSelector)
        case FirstOf.Alternative(WaitUntil(instant), tail) ⇒
          fromAlts(tail) :+ FirstOfUntil(instant)
        case FirstOf.Alternative(FirstOf(alts), tail) ⇒
          fromAlts(tail) ::: fromAlts(alts)
      }

      private[this] def ok[A](value: A) = lifted(Right[String, A](value))
      private[this] def fail[A](error: String) = lifted(Left[String, A](error))
      private[this] def lift[A](f: List[Expectation] ⇒ OrFail[(List[Expectation], A)]) =
        StateT[OrFail, List[Expectation], A](f)
      private[this] def lifted[A](x: Either[String, A]): Expectations[A] = lift(s ⇒ x.map(r ⇒ (s, r)))
    }

    object Transform extends (Action ~> Expectations) {
      def apply[A](fa: Action[A]) = {
        Expectations.check(fa)
      }
    }
  }
}
