package slfes.syntax

import shapeless.{Lub, CNil, :+:, Coproduct}
import shapeless.ops.coproduct.{Prepend, Basis, Selector}
import slfes.{AggregateFromId, Aggregate}

object ProcessSyntax {
  case class Subscription[A <: Aggregate, R](id: A#Id, handler: A#Event ⇒ Option[R])

  def await[A <: Aggregate, For <: Coproduct, R](a: Handler[A, For, R]) = Subscription(a.id, a.eventHandler.lift)
  def from[Id](id: Id)(implicit a: AggregateFromId[Id]): From[a.Out] = From[a.Out](id)

  case class From[A <: Aggregate](id: A#Id) {
    def event[E]: OnFirst[A, E] = new OnFirst[A, E](id)
  }
  case class OnFirst[A <: Aggregate, E](id: A#Id) {
    def apply[R](f: E ⇒ R)(implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil]): Handler[A, E :+: CNil, R] =
      Handler[A, E :+: CNil, R](id, EventHandler.on[E, R, A#Event](f))
  }
  case class Handler[A <: Aggregate, For <: Coproduct, R](id: A#Id, eventHandler: EventHandler.Aux[For, A#Event, R]) {
    def event[E] = OnChain[A, For, E, R](id, eventHandler)
  }
  case class OnChain[A <: Aggregate, For <: Coproduct, E, R1](id: A#Id, e1: EventHandler.Aux[For, A#Event, R1]) {
    def apply[R2, ROut](f: E ⇒ R2)
      (implicit s: Selector[A#Event, E], b1: Basis[A#Event, E :+: CNil], b2: Basis[A#Event, For],
        p: Prepend[For, E :+: CNil], l: Lub[R1, R2, ROut]): Handler[A, p.Out, ROut] = {
      val e2 = EventHandler.on[E, R2, A#Event](f)
      Handler[A, p.Out, ROut](id, e1 or e2)
    }
  }

  sealed trait EventHandler[Of <: Coproduct, R] extends PartialFunction[Of, R] {
    type For <: Coproduct

    def or[R2, ROut](b: EventHandler[Of, R2])
      (implicit b1: Basis[Of, For], b2: Basis[Of, b.For], p: Prepend[For, b.For], l: Lub[R, R2, ROut]): EventHandler.Aux[p.Out, Of, ROut] = {
      val a = this
      new EventHandler[Of, ROut] {
        type For = p.Out
        def isDefinedAt(x: Of) = a.isDefinedAt(x) || b.isDefinedAt(x)
        def apply(x: Of) = if (a.isDefinedAt(x)) l.left(a(x)) else l.right(b(x))
      }
    }
  }
  object EventHandler {
    def on[E, R, Of <: Coproduct](f: E ⇒ R)(implicit s: Selector[Of, E], b: Basis[Of, E :+: CNil]): Aux[E :+: CNil, Of, R] = new EventHandler[Of, R] {
      type For = E :+: CNil
      def isDefinedAt(x: Of) = s(x).isDefined
      def apply(x: Of) = f(s(x).get)
    }

    type Aux[F <: Coproduct, Of <: Coproduct, R] = EventHandler[Of, R] {type For = F}
  }
}
