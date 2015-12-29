package slfes

import cats.data.Xor
import shapeless._

/** Describes a class of aggregates (i.e. Account or Customer). */
case class AggregateType[I, S, C <: Coproduct, E <: Coproduct](
  name: String,
  seed: I ⇒ S,
  handleCommand: C ⇒ S ⇒ Xor[Any, Seq[E]],
  applyEvent: E ⇒ S ⇒ S,
  invariants: Traversable[S ⇒ Boolean]) {

  type Id = I
  type State = S
  type Command = C
  type Event = E
}