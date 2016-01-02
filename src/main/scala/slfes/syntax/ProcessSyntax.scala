package slfes.syntax

import cats.free.Free
import shapeless.{Lub, CNil, :+:, Coproduct}
import shapeless.ops.coproduct.{Prepend, Basis, Selector}
import slfes.{AggregateFromId, Aggregate}
import slfes.Process
import slfes.ProcessAction._

object ProcessSyntax {
  def await[A <: Aggregate, For <: Coproduct, R](handler: Handler[A, For, R]): Process[R] =
    Free.liftF(Await[A, R](handler.id, handler.handle))

  def from[Id](id: Id)(implicit a: AggregateFromId[Id]): From[a.Out] =
    From[a.Out](id)

  case class From[A <: Aggregate](id: A#Id) {
    def event[E]: OnFirst[A, E] = new OnFirst[A, E](id)
  }
  case class OnFirst[A <: Aggregate, E](id: A#Id) {
    def apply[R](f: E ⇒ R)(implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil]): Handler[A, E :+: CNil, R] =
      Handler[A, E :+: CNil, R](id, s(_).map(f))
  }
  case class Handler[A <: Aggregate, For <: Coproduct, R](id: A#Id, handle: A#Event ⇒ Option[R]) {
    def event[E] = OnChain[A, For, E, R](id, handle)
  }
  case class OnChain[A <: Aggregate, For <: Coproduct, E, R1](id: A#Id, handle: A#Event ⇒ Option[R1]) {
    def apply[R2, ROut](f: E ⇒ R2)
      (implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil], b2: Basis[A#Event, For],
        p: Prepend[For, E :+: CNil], l: Lub[R1, R2, ROut]): Handler[A, p.Out, ROut] = {
      Handler[A, p.Out, ROut](id, (event) ⇒
        b1.apply(event) match {
          case Left(e) ⇒ handle(event).map(l.left)
          case Right(e) ⇒ s(event).map(f).map(l.right)
        }
      )
    }
  }
}
