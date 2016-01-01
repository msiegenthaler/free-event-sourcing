package slfes

import shapeless._

/** Describes a class of aggregates (i.e. Account or Customer). */
case class AggregateType[I, S, C <: Coproduct, E <: Coproduct](name: String,
  seed: I ⇒ S,
  handleCommand: C ⇒ S ⇒ CmdResult[E],
  applyEvent: E ⇒ S ⇒ S) {
  type Id = I
  type State = S
  type Command = C
  type Event = E


  val aggregate = new Aggregate {
    type Id = I
    type Command = C
    type Event = E
  }
}

sealed trait Aggregate {
  type Id
  type Command <: Coproduct
  type Event <: Coproduct
}