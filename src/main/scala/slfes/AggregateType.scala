package slfes

import cats.data.Xor
import shapeless._

/** Describes a class of aggregates (i.e. Account or Customer). */
case class AggregateType[I, S, C <: Coproduct, E <: Coproduct](
  name: String,
  seed: I ⇒ S,
  handleCommand: C ⇒ S ⇒ CmdResult[E],
  applyEvent: E ⇒ S ⇒ S) {

  type Id = I
  type State = S
  type Command = C
  type Event = E
}